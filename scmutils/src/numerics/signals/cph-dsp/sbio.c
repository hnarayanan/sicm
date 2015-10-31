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
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <io/sound.h>

#include "dubuf.h"

int sbfd;
struct _double_buffer sbdb;

static void
fatal_perror (char * s)
{
  perror (s);
  fflush (stderr);
  db_close (&sbdb);
  exit (1);
}

static void
open_sb (void)
{
  struct sbparam sbp;
  struct sblevel sblevel;
  int insel;
  int monitor;

  sbfd = (open ("/dev/sb0", O_RDWR));
  if (sbfd < 0)
    fatal_perror ("open");

  if ((ioctl (sbfd, SBIOCRESET, 0)) < 0)
    fatal_perror ("SBIOCRESET");

  if ((ioctl (sbfd, SBIOCGETPARAM, (&sbp))) < 0)
    fatal_perror ("SBIOCGETPARAM");
  (sbp . sb_mode) = STRPCM;
  (sbp . sb_rate) = RATE8000;
  (sbp . sb_channel) = MONO;
  (sbp . sb_bitwidth) = RES16B;
  (sbp . sb_emphasis) = EMPH_OFF;
  if ((ioctl (sbfd, SBIOCSETPARAM, (&sbp))) < 0)
    fatal_perror ("SBIOCSETPARAM");

  (sblevel . sb_left) = 0x0;
  (sblevel . sb_right) = 0x0;
  if ((ioctl (sbfd, SBIOCSETINLVL, (&sblevel))) < 0)
    fatal_perror ("SBIOCSETINLVL");
  if ((ioctl (sbfd, SBIOCSETOUTLVL, (&sblevel))) < 0)
    fatal_perror ("SBIOCSETOUTLVL");
/*
  insel = RCALINE;
  if ((ioctl (sbfd, SBIOCSETINSW, (&insel))) < 0)
    fatal_perror ("SBIOCSETINSW");
*/
  monitor = MONITOR_OFF;
  if ((ioctl (sbfd, SBIOCSETMONSW, (&monitor))) < 0)
    fatal_perror ("SBIOCSETMONSW");
}

static int
read_sb (short * buffer, unsigned int length)
{
  unsigned int bl = (length * (sizeof (short)));
  while (1)
    {
      int result = (read (sbfd, buffer, bl));
      if (result >= 0)
	return (result / (sizeof (short)));
      if (errno != EIO)
	return (result);
      fprintf (stderr, "read overflow\n");
      fflush (stderr);
      if ((ioctl (sbfd, SBIOCABORT, 0)) < 0)
	fatal_perror ("SBIOCABORT");
    }
}

static void
read_process (double_buffer db)
{
  unsigned int read_size;
  short * bp;
  int n_read;

  while (1)
    {
      bp = ((short *) (db_get_input_pointer (db, (&read_size))));
      if (bp == 0)
	{
	  db_close (db);
	  exit (0);
	}
      n_read = (read_sb (bp, read_size));
      if (n_read > 0)
	{
	  db_record_input (db, n_read);
	  fprintf (stderr, "read %d\n", n_read);
	  fflush (stderr);
	}
      else if (n_read == 0)
	{
	  db_close (db);
	  exit (0);
	}
      else if (errno != EAGAIN)
	fatal_perror ("read");
    }
}

static int
write_sb (short * buffer, unsigned int length)
{
  int result = (write (sbfd, buffer, (length * (sizeof (short)))));
  if (result <= 0)
    return (result);
  if ((ioctl (sbfd, SBIOCFLUSH, 0)) < 0)
    fatal_perror ("SBIOCFLUSH");
  return (result / (sizeof (short)));
}

static void
write_process (double_buffer db)
{
  unsigned int write_size;
  short * bp;
  int n_written;

  while (1)
    {
      bp = ((short *) (db_get_output_pointer (db, (&write_size))));
      if (bp == 0)
	{
	  db_close (db);
	  exit (0);
	}
      n_written = (write_sb (bp, (write_size / (sizeof (short)))));
      if (n_written > 0)
	{
	  db_record_output (db, n_written);
	  fprintf (stderr, "write %d\n", n_written);
	  fflush (stderr);
	}
      else if (n_written == 0)
	{
	  db_close (db);
	  exit (0);
	}
      else if (errno != EAGAIN)
	fatal_perror ("read");
    }
}

void
main (int argc, char ** argv)
{
  db_initialize ((&sbdb),
		 (8000 * (sizeof (short))),
		 (8000 * (sizeof (short))));
  open_sb ();
  {
    pid_t child_pid = (fork ());
    if (child_pid == 0)
      {
	db_attach_output (&sbdb);
	write_process (&sbdb);
      }
    else if (child_pid > 0)
      {
	db_attach_input (&sbdb);
	read_process (&sbdb);
      }
    else
      fatal_perror ("fork");
  }
}
