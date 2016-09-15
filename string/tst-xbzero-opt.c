/* Test that explicit_bzero block clears are not optimized out.
   Copyright (C) 2016 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* This test is conceptually based on a test designed by Matthew
   Dempsky for the OpenBSD regression suite:
   <openbsd>/src/regress/lib/libc/explicit_bzero/explicit_bzero.c.
   The basic idea is, we have a function that contains a
   block-clearing operation (not necessarily explicit_bzero), after
   which the block is dead, in the compiler-jargon sense.  Execute
   that function from a signal handler running on an alternative
   signal stack.  Then we have another pointer to the memory region
   affected by the block clear -- namely, the sigaltstack buffer --
   and can find out whether it actually happened.

   The OpenBSD test cautions that some operating systems (e.g. Solaris
   and OSX) wipe the signal stack upon returning to the normal stack,
   so the test has to happen while still executing on the signal
   stack.  This, of course, means that the buffer might be clobbered
   by normal stack operations after the function with the block clear
   returns (it has to return, so that the block is truly dead).  The
   most straightforward way to deal with this is to have a large block
   containing several copies of a byte pattern that is unlikely to
   occur by chance, and check whether _any_ of them survives.  */

#define _GNU_SOURCE 1

#include <stdbool.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* The "byte pattern that is unlikely to occur by chance": the first
   16 prime numbers (OEIS A000040).  */
static const unsigned char test_pattern[16] =
{
  2, 3, 5, 7,  11, 13, 17, 19,  23, 29, 31, 37,  41, 43, 47, 53
};

#define PATTERN_SIZE (sizeof test_pattern)
#define PATTERN_REPS 32
#define TEST_BUFFER_SIZE (PATTERN_SIZE * PATTERN_REPS)

static void
fill_with_test_pattern (unsigned char *buf)
{
  for (unsigned int i = 0; i < PATTERN_REPS; i++)
    memcpy (buf + i*PATTERN_SIZE, test_pattern, PATTERN_SIZE);
}

static unsigned int
count_test_patterns (unsigned char *buf, size_t bufsiz)
{
  unsigned char *first = memmem (buf, bufsiz, test_pattern, PATTERN_SIZE);
  if (!first)
    return 0;
  unsigned int cnt = 0;
  for (unsigned int i = 0; i < PATTERN_REPS; i++)
    {
      unsigned char *p = first + i*PATTERN_SIZE;
      if (p + PATTERN_SIZE - buf > bufsiz)
        break;
      if (memcmp (p, test_pattern, PATTERN_SIZE) == 0)
        cnt++;
    }
  return cnt;
}

/* Global test state.  */
static int failed_subtests;
static bool this_subtest_failed;

/* The signal stack is allocated with memalign.  */
static unsigned char *signal_stack_buffer;
#define SIGNAL_STACK_SIZE (SIGSTKSZ + TEST_BUFFER_SIZE)

enum test_expectation { EXPECT_NONE, EXPECT_SOME, EXPECT_ALL };

static void
check_test_buffer (enum test_expectation expected,
                   const char *label, const char *stage)
{
  unsigned int cnt = count_test_patterns (signal_stack_buffer,
                                          SIGNAL_STACK_SIZE);
  switch (expected)
    {
    case EXPECT_NONE:
      if (cnt == 0)
        {
          printf ("PASS: %s/%s: expected 0 got %d\n", label, stage, cnt);
          fflush (stdout);
        }
      else
        {
          printf ("FAIL: %s/%s: expected 0 got %d\n", label, stage, cnt);
          fflush (stdout);
          this_subtest_failed = true;
          failed_subtests++;
        }
      break;

    case EXPECT_SOME:
      if (cnt > 0)
        {
          printf ("PASS: %s/%s: expected some got %d\n", label, stage, cnt);
          fflush (stdout);
        }
      else
        {
          printf ("FAIL: %s/%s: expected some got 0\n", label, stage);
          fflush (stdout);
          this_subtest_failed = true;
          failed_subtests++;
        }
      break;

     case EXPECT_ALL:
      if (cnt == PATTERN_REPS)
        {
          printf ("PASS: %s/%s: expected %d got %d\n", label, stage,
                  PATTERN_REPS, cnt);
          fflush (stdout);
        }
      else
        {
          printf ("FAIL: %s/%s: expected %d got %d\n", label, stage,
                  PATTERN_REPS, cnt);
          fflush (stdout);
          this_subtest_failed = true;
          failed_subtests++;
        }
      break;

    default:
      printf ("ERROR: %s/%s: invalid value for 'expected' = %d\n",
              label, stage, (int)expected);
      fflush (stdout);
      this_subtest_failed = true;
      failed_subtests++;
    }
}

/* Always check the test buffer immediately after filling it; this
   makes externally visible side effects depend on the buffer existing
   and having been filled in.  */
static void
prepare_test_buffer (unsigned char *buf, const char *label)
{
  fill_with_test_pattern (buf);
  check_test_buffer (EXPECT_ALL, label, "prepare");

  unsigned char *loc = memmem (signal_stack_buffer, SIGNAL_STACK_SIZE,
                               test_pattern, PATTERN_SIZE);
  if (loc == buf)
    {
      printf ("PASS: %s/prepare: expected buffer location %p got %p\n",
              label, buf, loc);
      fflush (stdout);
    }
  else
    {
      printf ("FAIL: %s/prepare: expected buffer location %p got %p\n",
              label, buf, loc);
      fflush (stdout);
      this_subtest_failed = true;
      failed_subtests++;
    }
}

/* There are three subtests, two of which are sanity checks.

   In the "no_clear" case, we don't do anything to the test buffer
   between preparing it and letting it go out of scope, and we expect
   to find it.  This confirms that the test buffer does get filled in
   and we can find it from the stack buffer.  In the "ordinary_clear"
   case, we clear it using memset, and we expect to find it.  This
   confirms that the compiler can optimize out block clears in this
   context; if it can't, the real test might be succeeding for the
   wrong reason.  Finally, the "explicit_clear" case uses
   explicit_bzero and expects _not_ to find the test buffer, which is
   the real test.  */

static void
setup_no_clear (void)
{
  unsigned char buf[TEST_BUFFER_SIZE];
  prepare_test_buffer (buf, "no clear");
}

static void
setup_ordinary_clear (void)
{
  unsigned char buf[TEST_BUFFER_SIZE];
  prepare_test_buffer (buf, "ordinary clear");
  if (this_subtest_failed)
    return;
  memset (buf, 0, TEST_BUFFER_SIZE);
}

static void
setup_explicit_clear (void)
{
  unsigned char buf[TEST_BUFFER_SIZE];
  prepare_test_buffer (buf, "explicit clear");
  if (this_subtest_failed)
    return;
  explicit_bzero (buf, TEST_BUFFER_SIZE);
}

struct subtest
{
  void (*setup_subtest) (void);
  const char *label;
  enum test_expectation expected;
};

static const struct subtest subtests[] =
{
  { setup_no_clear,       "no clear",       EXPECT_SOME },
  { setup_ordinary_clear, "ordinary clear", EXPECT_SOME },
  { setup_explicit_clear, "explicit clear", EXPECT_NONE },
  { 0,                    0,                -1          }
};
static const struct subtest *this_subtest;
static bool this_subtest_complete;

/* This function is called as a signal handler.  The signal is
   triggered by a call to raise, therefore it is safe to do
   non-async-signal-safe things within this function.

   The this_subtest_complete flag addresses a race.  The outer loop
   keeps SIGUSR1 blocked all the time, unblocking it only immediately
   after setting up the appropriate conditions for a test and then
   raising SIGUSR1 itself.  SIGUSR1 is not a real-time signal, so if
   another process sends this process SIGUSR1 _before_ it's unblocked
   by the outer loop, this function will only be called once.
   However, if another process sends this process SIGUSR1 _while this
   handler is already running_, that signal will be pending upon
   return from this function, and will fire before the outer loop has
   a chance to re-block SIGUSR1.  This is unavoidable; the workaround
   is to arrange for this function not to do anything if it's called
   several times in a row.  */
static void
do_subtest (int signo __attribute__ ((unused)))
{
  if (!this_subtest_complete)
    {
      this_subtest->setup_subtest ();
      if (!this_subtest_failed)
        check_test_buffer (this_subtest->expected,
                           this_subtest->label,
                           "test");
      this_subtest_complete = true;
    }
}

static int
do_test (void)
{
  /* test-skeleton.c unconditionally sets stdout to be unbuffered.
     vfprintf allocates a great deal of memory on the stack if called
     with an unbuffered FILE*, overflowing the alt-stack.  This is
     also why there is a call to fflush after every call to printf in
     this file.  */
  if (setvbuf (stdout, 0, _IOFBF, 0))
    {
      printf ("ERROR: restoring stdout buffering: %s\n", strerror (errno));
      fflush (stdout);
      return 2;
    }

  size_t page_alignment = sysconf (_SC_PAGESIZE);
  if (page_alignment < sizeof (void *))
    page_alignment = sizeof (void *);

  void *p;
  int err = posix_memalign (&p, page_alignment, SIGNAL_STACK_SIZE);
  if (err || !p)
    {
      printf ("ERROR: allocating alt stack: %s\n", strerror (err));
      fflush (stdout);
      return 2;
    }
  signal_stack_buffer = p;

  /* This program will malfunction if it receives SIGUSR1 signals at any
     time other than when it's just sent one to itself.  Therefore, keep
     it blocked most of the time.  */
  sigset_t sigusr1_mask;
  sigemptyset (&sigusr1_mask);
  sigaddset (&sigusr1_mask, SIGUSR1);
  if (sigprocmask (SIG_BLOCK, &sigusr1_mask, 0))
    {
      printf ("ERROR: block(SIGUSR1): %s\n", strerror (errno));
      fflush (stdout);
      return 2;
    }

  stack_t ss;
  ss.ss_sp    = signal_stack_buffer;
  ss.ss_flags = 0;
  ss.ss_size  = SIGNAL_STACK_SIZE;
  if (sigaltstack (&ss, 0))
    {
      printf ("ERROR: sigaltstack: %s\n", strerror (errno));
      fflush (stdout);
      return 2;
    }

  struct sigaction sa;
  sigemptyset (&sa.sa_mask);
  sigaddset (&sa.sa_mask, SIGUSR1);
  sa.sa_handler = do_subtest;
  sa.sa_flags = SA_RESTART | SA_ONSTACK;
  if (sigaction (SIGUSR1, &sa, 0))
    {
      printf ("ERROR: sigaction(SIGUSR1): %s\n", strerror (errno));
      fflush (stdout);
      return 2;
    }

  /* We use kill instead of raise, because raise screws with the
     signal mask, which we don't want.  */
  pid_t self = getpid ();

  this_subtest = subtests;
  while (this_subtest->label)
    {
      this_subtest_complete = false;
      this_subtest_failed = false;

      /* Completely clear the signal stack between tests, so that junk
         from previous tests cannot interfere with the current one.  */
      memset (signal_stack_buffer, 0, SIGNAL_STACK_SIZE);

      /* Raise SIGUSR1, then unblock it, then immediately block it
         again.  This will trigger do_subtest to run _once_ on the
         alternative stack, at the point of calling sigprocmask to
         unblock the signal.  */
      if (kill (self, SIGUSR1))
        {
          printf ("ERROR: raise(SIGUSR1): %s\n", strerror (errno));
          fflush (stdout);
          return 2;
        }
      if (sigprocmask (SIG_UNBLOCK, &sigusr1_mask, 0))
        {
          printf ("ERROR: unblock(SIGUSR1): %s\n", strerror (errno));
          fflush (stdout);
          return 2;
        }
      if (sigprocmask (SIG_BLOCK, &sigusr1_mask, 0))
        {
          printf ("ERROR: unblock(SIGUSR1): %s\n", strerror(errno));
          fflush (stdout);
          return 2;
        }

      this_subtest++;
    }

  return failed_subtests ? 1 : 0;
}

#define TEST_FUNCTION do_test ()
#include "../test-skeleton.c"
