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

#include "filter-frame.h"

struct f_context
{
  unsigned int m;
  unsigned int delay;
  REAL mu;
  REAL * h;
};

static void usage (void);
static unsigned int convert_index_arg (char * s, unsigned int limit);
static REAL convert_real_arg (char * s);
static void cancel_noise (REAL * ib, REAL * ob, unsigned int ndata, void * cv);
static void cancel_tones (REAL * ib, REAL * ob, unsigned int ndata, void * cv);

void
main (int argc, char ** argv)
{
  struct f_context c;
  filter f;

  if (argc != 5)
    usage ();
  (c.m) = (convert_index_arg ((argv[1]), 8196));
  (c.delay) = (convert_index_arg ((argv[3]), 8196));
  (c.mu) = (convert_real_arg (argv[2]));
  (c.h) = (xmalloc ((c.m) * (sizeof (REAL))));
  {
    REAL * sh = (c.h);
    REAL * eh = (sh + (c.m));
    while (sh < eh)
      (*sh++) = 0.;
  }
  switch (convert_index_arg ((argv[4]), 2))
    {
    case 0:
      f = cancel_noise;
      break;
    case 1:
      f = cancel_tones;
      break;
    }
  apply_filter ((make_filter_state ((fileno (stdin)),
				    (fileno (stdout)),
				    8000,
				    ((c.m) + (c.delay)))),
		f,
		(&c));
  exit (0);
}

static void
usage (void)
{
  fprintf (stderr, "usage: PROGRAM ORDER MU DELAY TYPE\n");
  exit (1);
}

static unsigned int
convert_index_arg (char * s, unsigned int limit)
{
  char * ep;
  long i = (strtol (s, (&ep), 0));
  if ((*ep) != 0)
    {
      fprintf (stderr, "Ill-formed integer argument: %s\n", s);
      exit (1);
    }
  if ((i < 0) || (i >= limit))
    {
      fprintf (stderr, "Integer argument out of range: %d\n", i);
      exit (1);
    }
  return (i);
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

static void
cancel_noise (REAL * ib, REAL * ob, unsigned int ndata, void * cv)
{
  REAL * si = ib;
  REAL * ei = (si + ndata);
  REAL * so = ob;
  struct f_context * c = cv;
  unsigned int m = (c->m);
  unsigned int delay = (c->delay);
  REAL mu = (c->mu);
  REAL * h = (c->h);
  REAL * u;
  REAL * up;
  REAL * hp;
  REAL temp;

  while (si < ei)
    {
      u = (si - delay);
      up = (u - m);
      hp = (h + m);
      temp = ((*up++) * (*--hp));
      while (up < u)
	temp += ((*up++) * (*--hp));
      (*so++) = temp;
      temp = ((*si++) - temp);
      temp *= mu;
      up = (u - m);
      hp = (h + m);
      while (up < u)
	(*--hp) += ((*up++) * temp);
    }
}

static void
cancel_tones (REAL * ib, REAL * ob, unsigned int ndata, void * cv)
{
  REAL * si = ib;
  REAL * ei = (si + ndata);
  REAL * so = ob;
  struct f_context * c = cv;
  unsigned int m = (c->m);
  unsigned int delay = (c->delay);
  REAL mu = (c->mu);
  REAL * h = (c->h);
  REAL * u;
  REAL * up;
  REAL * hp;
  REAL temp;

  while (si < ei)
    {
      u = (si - delay);
      up = (u - m);
      hp = (h + m);
      temp = ((*up++) * (*--hp));
      while (up < u)
	temp += ((*up++) * (*--hp));
      temp = ((*si++) - temp);
      (*so++) = temp;
      temp *= mu;
      up = (u - m);
      hp = (h + m);
      while (up < u)
	(*--hp) += ((*up++) * temp);
    }
}
