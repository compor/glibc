/* Copyright (C) 2007-2013 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Richard Henderson.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include <math.h>

float
__nearbyintf (float x)
{
  if (isless (fabsf (x), 16777216.0f))	/* 1 << FLT_MANT_DIG */
    {
      /* Note that Alpha S_Floating is stored in registers in a
	 restricted T_Floating format, so we don't even need to
	 convert back to S_Floating in the end.  The initial
	 conversion to T_Floating is needed to handle denormals.  */

      float tmp1, tmp2, new_x;

      __asm ("cvtst/s %3,%2\n\t"
	     "cvttq/svd %2,%1\n\t"
	     "cvtqt/d %1,%0\n\t"
	     : "=f"(new_x), "=&f"(tmp1), "=&f"(tmp2)
	     : "f"(x));

      /* nearbyintf(-0.1) == -0, and in general we'll always have the same
	 sign as our input.  */
      x = copysignf(new_x, x);
    }
  return x;
}

weak_alias (__nearbyintf, nearbyintf)