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

int sbfd;

static void
fatal_perror (char * s)
{
  perror (s);
  fflush (stderr);
  if (sbfd > 0)
    close (sbfd);
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
write_sb (short * buffer, unsigned int length)
{
  int result = (write (sbfd, buffer, (length * (sizeof (short)))));
  if (result <= 0)
    return (result);
  if ((ioctl (sbfd, SBIOCFLUSH, 0)) < 0)
    fatal_perror ("SBIOCFLUSH");
  return (result / (sizeof (short)));
}

void
main (int argc, char ** argv)
{
  short buffer [8000];

  open_sb ();
  while (1)
    {
      short * bp = (&buffer[0]);
      int r = (read (0, ((char *) bp), 16000));
      if (r < 0)
	fatal_perror ("read");
      if (r == 0)
	{
	  close (sbfd);
	  exit (0);
	}
      {
	unsigned int n_left = (r / (sizeof (short)));
	while (1)
	  {
	    if (n_left <= 0)
	      break;
	    {
	      int n_written = (write_sb (bp, n_left));
	      if (n_written < 0)
		{
		  if (errno != EAGAIN)
		    fatal_perror ("write");
		}
	      else if (n_written == 0)
		{
		  fprintf (stderr, "write returned 0\n");
		  fflush (stderr);
		  close (sbfd);
		  exit (1);
		}
	      else
		{
		  n_left -= n_written;
		  bp += n_written;
		}
	    }
	  }
      }
    }
}
