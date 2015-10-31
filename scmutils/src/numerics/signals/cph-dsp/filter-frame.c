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

static unsigned int read_buffer (int fd, void * buffer, unsigned int nbytes);
static void write_buffer (int fd, void * buffer, unsigned int nbytes);

filter_state
make_filter_state
  (int input_fd, int output_fd, unsigned int buffer_size, unsigned int overlap)
{
  filter_state s = (xmalloc (sizeof (struct _filter_state)));
  (s->input_fd) = input_fd;
  (s->output_fd) = output_fd;
  (s->buffer_size) = buffer_size;
  (s->overlap) = overlap;
  (s->raw_input_buffer) = (xmalloc (buffer_size * (sizeof (short))));
  (s->raw_output_buffer) = (xmalloc (buffer_size * (sizeof (short))));
  (s->input_buffer) = (xmalloc ((buffer_size + overlap) * (sizeof (REAL))));
  (s->output_buffer) = (xmalloc (buffer_size * (sizeof (REAL))));
  return (s);
}

void
apply_filter (filter_state s, filter f, void * context)
{
  unsigned int overlap = (s->overlap);
  unsigned int buffer_size = (s->buffer_size);
  short * rib = (s->raw_input_buffer);
  short * rob = (s->raw_output_buffer);
  REAL * ib = (s->input_buffer);
  REAL * ob = (s->output_buffer);
  unsigned int nbytes = (buffer_size * (sizeof (short)));

  {
    REAL * sd = ib;
    REAL * ed = (sd + overlap);
    while (sd < ed)
      (*sd++) = 0.;
  }
  while (1)
    {
      unsigned int nread = (read_buffer ((s->input_fd), rib, nbytes));
      unsigned int ndata = (nread / (sizeof (short)));
      {
	short * si = rib;
	short * ei = (si + ndata);
	REAL * sd = (ib + overlap);
	while (si < ei)
	  (*sd++) = (((REAL) (*si++)) / 32768.);
      }
      (*f) ((ib + overlap), ob, ndata, context);
      {
	REAL * sd = ob;
	REAL * ed = (sd + ndata);
	short * so = rob;
	while (sd < ed)
	  {
	    REAL x = (*sd++);
	    (*so++) = ((short) ((x * 32768.) + ((x < 0) ? -.5 : .5)));
	  }
      }
      write_buffer ((s->output_fd), rob, (ndata * (sizeof (short))));
      if (nread < nbytes)
	break;
      {
	REAL * s1 = ib;
	REAL * s2 = (ib + buffer_size);
	REAL * e2 = (s2 + overlap);
	while (s2 < e2)
	  (*s1++) = (*s2++);
      }
    }
}

void *
xmalloc (unsigned int nbytes)
{
  void * p = (malloc (nbytes));
  if (p == 0)
    {
      fprintf (stderr, "Unable to allocate %d bytes.\n", nbytes);
      exit (1);
    }
  return (p);
}

static unsigned int
read_buffer (int fd, void * buffer, unsigned int nbytes)
{
  char * bp = buffer;
  while (nbytes > 0)
    {
      int nread = (read (fd, bp, nbytes));
      if (nread < 0)
	{
	  perror ("read");
	  exit (1);
	}
      if (nread == 0)
	break;
      bp += nread;
      nbytes -= nread;
    }
  return (bp - ((char *) buffer));
}

static void
write_buffer (int fd, void * buffer, unsigned int nbytes)
{
  char * bp = buffer;
  while (nbytes > 0)
    {
      int nwritten = (write (fd, bp, nbytes));
      if (nwritten < 0)
	{
	  perror ("write");
	  exit (1);
	}
      bp += nwritten;
      nbytes -= nwritten;
    }
}
