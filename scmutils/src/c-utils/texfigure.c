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
#include <string.h>
#include <math.h>

#define MAXLABELS 20

#ifndef TEXWIDTH
#define TEXWIDTH 1024
#endif

#ifndef TEXHEIGHT
#define TEXHEIGHT 1024
#endif

int
main (int argc, char ** argv)
{
  unsigned int xintervals = 4;
  unsigned int yintervals = 4;
  unsigned int nxv = 0;
  unsigned int nyv = 0;
  unsigned int i = 1;
  char * xvalues [MAXLABELS];
  char * yvalues [MAXLABELS];
  char * xlabel = 0;
  char * ylabel = 0;
  char * name = 0;
  char name_tex[128];
  FILE *out;
  int setxv = 0;
  int setyv = 0;
  int xlabeldef = 0;
  int ylabeldef = 0;

  if (argc == 1)
    {
      printf ("%s -n name -xi xintervals -yi yintervals -xl xlabel -yl ylabel -xv xvalues -yv yvalues\n", argv[0]);
      exit (1);
    }

  while (i < argc)
    {
      const char * arg = (argv[i++]);
      if (!strcmp (arg, "-n"))
	{
	  setxv = 0;
	  setyv = 0;
	  name = argv[i++];
	}
      else if (!strcmp (arg, "-xi"))
	{
	  setxv = 0;
	  setyv = 0;
	  xintervals = (atoi (argv[i++]));
	}
      else if (!strcmp (arg, "-yi"))
	{
	  setxv = 0;
	  setyv = 0;
	  yintervals = (atoi (argv[i]));
	}
      else if (!strcmp (arg, "-xl"))
	{
	  setxv = 0;
	  setyv = 0;
	  xlabel = (argv[i++]);
	  xlabeldef = 1;
	}
      else if (!strcmp (arg, "-yl"))
	{
	  setxv = 0;
	  setyv = 0;
	  ylabel = (argv[i]);
	  ylabeldef = 1;
	}
      else if (!strcmp (arg, "-xv"))
	{
	  setxv = 1;
	  setyv = 0;
	}
      else if (!strcmp (arg, "-yv"))
	{
	  setxv = 0;
	  setyv = 1;
	}
      else if (setyv)
	(yvalues[nyv++]) = (argv[i++]);
      else if (setxv)
	(xvalues[nxv++]) = (argv[i++]);
    }

  strcpy(name_tex, name);
  strcat(name_tex, ".tex"); 
  out = fopen(name_tex, "w");
  
  {
    double x, y, dx, dy;
    double border = 30;
    double label_edge = -5.0;
    double x_size,  y_size, x_size_1, y_size_1, x_total, y_total;
    double x_interval, y_interval, t_size=5.0;
    int x_tics, y_tics;
    char x_size_str[128], y_size_str[128], x_bit_str[128], y_bit_str[128];

    x_size = TEXWIDTH  * 0.2409; x_size_1 = x_size + 1.0;
    y_size = TEXHEIGHT * 0.2409; y_size_1 = y_size + 1.0;

    x_total = x_size_1 + border + border;
    y_total = y_size_1 + border + border;

    x_interval = x_size_1 / ((double) xintervals);
    y_interval = y_size_1 / ((double) yintervals);

    x_tics = xintervals - 1;
    y_tics = yintervals - 1;

    sprintf(x_size_str, "%f", x_size);
    sprintf(y_size_str, "%f", y_size);

    sprintf(x_bit_str, "%f", ((double) TEXWIDTH));
    sprintf(y_bit_str, "%f", ((double) TEXHEIGHT));

    fprintf(out, "\\setlength{\\unitlength}{1pt}\n");
    fprintf(out, "\\begin{picture}( %f , %f )( %f , %f )\n", x_total, y_total, -border, -border);

    if(nxv>2) {
      dx = x_size_1 / ((double) (nxv - 1));
      x = 0.0;
      fprintf(out, "  \\putmbox { %f , %f }{ %s }{ $%s$ }\n", x, label_edge, "tl", xvalues[0]);
      for(i=1; i<nxv-1; i++) {
	x += dx;
	fprintf(out, "  \\putmbox { %f , %f }{ %s }{ $%s$ }\n", x, label_edge, "t", xvalues[i]);
      }
      fprintf(out, "  \\putmbox { %f , %f }{ %s }{ $%s$ }\n", x_size_1, label_edge, "tr", xvalues[nxv-1]);
    }

    if(nyv>2) {
      dy = y_size_1 / ((double) (nyv - 1));
      y = 0.0;
      fprintf(out, "  \\putmbox { %f , %f }{ %s }{ $%s$ }\n", label_edge, y, "br", yvalues[0]);
      for(i=1; i<nyv-1; i++) {
	y += dy;
	fprintf(out, "  \\putmbox { %f , %f }{ %s }{ $%s$ }\n", label_edge, y, "r", yvalues[i]);
      }
      fprintf(out, "  \\putmbox { %f , %f }{ %s }{ $%s$ }\n", label_edge, y_size_1, "tr", yvalues[nyv-1]);
    }


    if(xlabeldef == 1)
      fprintf(out, "  \\putmbox { %f , %f }{ %s }{ $%s$ }\n", 0.5*x_size_1, -0.5*border, "t", xlabel);

    if(ylabeldef == 1)
      fprintf(out, "  \\putmbox { %f , %f }{ %s }{ $%s$ }\n", -border, 0.5*y_size_1, "r", ylabel);

    fprintf(out, "  \\special{psfile=%s.ps}\n", name);


    fprintf(out, "  \\put(0,0){\\framebox ( %f , %f ) {}}\n", x_size_1, y_size_1);

    fprintf(out, "  \\multiput(%f,%f)(%f,%f){%d}{\\line%s{%f}}\n", 
	    x_interval, 0.0, x_interval, 0.0, x_tics, "(0,1)", t_size);
    fprintf(out, "  \\multiput(%f,%f)(%f,%f){%d}{\\line%s{%f}}\n", 
	    x_interval, y_size_1, x_interval, 0.0, x_tics, "(0,-1)", t_size);
    fprintf(out, "  \\multiput(%f,%f)(%f,%f){%d}{\\line%s{%f}}\n", 
	    0.0, y_interval, 0.0, y_interval, y_tics, "(1,0)", t_size);
    fprintf(out, "  \\multiput(%f,%f)(%f,%f){%d}{\\line%s{%f}}\n", 
	    x_size_1, y_interval, 0.0, y_interval, y_tics, "(-1,0)", t_size);

    fprintf(out, "\\end{picture}\n");

  }
  return (0);
}
