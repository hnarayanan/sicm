/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>

#include "filter-frame.h"

struct f_context
{
  unsigned int m;
  REAL * h;
};

static void usage (void);
static REAL convert_real_arg (char * s);
static REAL * kaiser_bandpass_coefficients
  (REAL center_frequency, REAL bandwidth, REAL shape_factor, REAL delta,
   unsigned int * length);
static REAL kaiser_i0 (REAL x);
static void bandpass (REAL * ib, REAL * ob, unsigned int ndata, void * cv);

void
main (int argc, char ** argv)
{
  REAL fr = ((atan2 (1., 1.)) / 1000.);
  struct f_context c;

  if (argc != 4)
    usage ();
  (c.h) =
    (kaiser_bandpass_coefficients (((convert_real_arg (argv[1])) * fr),
				   ((convert_real_arg (argv[2])) * fr),
				   (convert_real_arg (argv[3])),
				   .001,
				   (& (c.m))));
  fprintf (stderr, "Filter order = %d\n", ((c.m) - 1));
  fflush (stderr);
  apply_filter ((make_filter_state ((fileno (stdin)),
				    (fileno (stdout)),
				    8000,
				    (c.m))),
		bandpass,
		(&c));
  exit (0);
}

static void
usage (void)
{
  fprintf (stderr, "usage: PROGRAM OMEGA_C BW SF");
  fprintf (stderr, "  OMEGA_C is center frequency in hertz\n");
  fprintf (stderr, "  BW is bandwidth in herzt\n");
  fprintf (stderr, "  SF is 6:60 shape factor\n");
  fprintf (stderr, "  assumed sampling rate is 8000 samples/second\n");
  exit (1);
}

static REAL
convert_real_arg (char * s)
{
  char * ep;
  REAL i = (strtod (s, (&ep)));
  if ((*ep) != 0)
    {
      fprintf (stderr, "Ill-formed real argument: %s\n", s);
      exit (1);
    }
  return (i);
}

static REAL *
kaiser_bandpass_coefficients
  (REAL center_frequency, REAL bandwidth, REAL shape_factor, REAL delta,
   unsigned int * length)
{
  REAL delta_omega = (bandwidth * (shape_factor - 1.));
  REAL half_bandwidth = (bandwidth / 2.);
  REAL passband_low = (center_frequency - half_bandwidth);
  REAL passband_high = (center_frequency + half_bandwidth);
  REAL a = (- (20. * (log10 (delta))));
  unsigned int m = ((unsigned int) (((a - 8.) / (2.285 * delta_omega)) + .5));
  REAL * h = (xmalloc ((m + 1) * (sizeof (REAL))));
  REAL alpha = (((REAL) m) / 2.);
  REAL beta =
    ((a < 21.) ? 0.
     : (a < 51.) ? ((.5842 * (pow ((a - 21.), .4))) + (.07886 * (a - 21.)))
     : (.1102 * (a - 8.7)));
  REAL i0_beta = (kaiser_i0 (beta));
  REAL pi = (4. * (atan2 (1., 1.)));
  unsigned int n;
  for (n = 0; n <= m; n += 1)
    {
      REAL n_minus_alpha = (((REAL) n) - alpha);
      REAL t = (n_minus_alpha / alpha);
      (h[n]) =
	(((kaiser_i0 (beta * (sqrt (1. - (t * t))))) / i0_beta)
	 * ((n_minus_alpha == 0.)
	    ? ((passband_high - passband_low) / pi)
	    : (((sin (passband_high * n_minus_alpha))
		- (sin (passband_low * n_minus_alpha)))
	       / (pi * n_minus_alpha))));
    }
  (*length) = n;
  return (h);
}

static REAL
kaiser_i0 (REAL x)
{
  REAL d = 0.;
  REAL ds = 1.;
  REAL s = 1.;
  while (1)
    {
      d += 2;
      ds *= ((x * x) / (d * d));
      s += ds;
      if (ds < (s * 2e-9))
	return (s);
    }
}

static void
bandpass (REAL * ib, REAL * ob, unsigned int ndata, void * cv)
{
  struct f_context * c = cv;
  unsigned int m = (c->m);
  REAL * h = (c->h);
  REAL * si = ib;
  REAL * ei = (si + ndata);
  REAL * so = ob;
  while (si < ei)
    {
      REAL * up = (si - m);
      REAL * hp = (h + m);
      REAL temp = ((*up++) * (*--hp));
      while (h < hp)
	temp += ((*up++) * (*--hp));
      (*so++) = temp;
      si += 1;
    }
}
