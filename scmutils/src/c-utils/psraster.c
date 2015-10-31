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

#define WIDTH 1664
#define HEIGHT 1664
#define COLS 208
#define SCALE 0.2409

unsigned char on_bit[]= {1, 2, 4, 8, 16, 32, 64, 128};
unsigned char data[HEIGHT][COLS];
int height=HEIGHT, width=WIDTH, cols=COLS;
double xmin, xmax, ymin, ymax;

static void print_ps_header (void);
static void print_ps_tail (void);
static void print_raster (void);

int
main (int argc, char ** argv)
{
  double xscale, yscale, x, y;
  int ix, iy;
  int i, size = 1;

  if(argc<=1) {
    printf("args: xmin xmax ymin ymax [-s[ize] dot-size (1 2 3 4 9)] [-w[idth] width-pixels] [-h[eight] height-pixels]\n");
    exit(-1);
  }

  i=1;
  while(i < argc) {
    if(!strncmp(argv[i], "-w", 2)) {
      i++; 
      width = atoi(argv[i++]); 
      cols = width / 8 + (((width % 8) == 0) ? 0 : 1); 
      continue;
    }
    if(!strncmp(argv[i], "-h", 2)) {i++; height = atoi(argv[i++]); continue;}
    if(!strncmp(argv[i], "-s", 2)) {i++; size = atoi(argv[i++]); continue;}
    /* then assume inputing limits */
    sscanf(argv[i++], "%lf", &xmin);
    sscanf(argv[i++], "%lf", &xmax);
    sscanf(argv[i++], "%lf", &ymin);
    sscanf(argv[i++], "%lf", &ymax);
  }

  print_ps_header();

  xscale = ((double) width) / (xmax-xmin);
  yscale = ((double) height) / (ymax-ymin);

  while (scanf("%lf %lf", &x, &y) != EOF) {
    ix = ((int) xscale * (x-xmin));
    iy = ((int) yscale * (y-ymin));

    if(size == 1) {
      if ((ix>=0) && (ix <width) &&
	  (iy>=0) && (iy <height)) 
	data[iy][ix/8] |= on_bit[7 - (ix%8)];
    } else if(size == 2) {
      if ((ix>=0) && (ix <width) &&
	  (iy>=0) && (iy <height)) 
	data[iy][ix/8] |= on_bit[7 - (ix%8)];
      iy--;
      if ((ix>=0) && (ix <width) &&
	  (iy>=0) && (iy <height))
	data[iy][ix/8] |= on_bit[7 - (ix%8)];
    } else if(size == 4) {
      if ((ix>=0) && (ix <width) && (iy>=0) && (iy <height))
	data[iy][ix/8] |= on_bit[7 - (ix%8)];
      ix++;
      if ((ix>=0) && (ix <width) && (iy>=0) && (iy <height))
	data[iy][ix/8] |= on_bit[7 - (ix%8)];
      iy++;
      if ((ix>=0) && (ix <width) && (iy>=0) && (iy <height))
	data[iy][ix/8] |= on_bit[7 - (ix%8)];
      ix--;
      if ((ix>=0) && (ix <width) && (iy>=0) && (iy <height))
	data[iy][ix/8] |= on_bit[7 - (ix%8)];
    } else if(size == 9) {
      if ((ix>=0) && (ix <width) && (iy>=0) && (iy <height))
	data[iy][ix/8] |= on_bit[7 - (ix%8)];
      iy--;
      if ((ix>=0) && (ix <width) && (iy>=0) && (iy <height))
	data[iy][ix/8] |= on_bit[7 - (ix%8)];
      ix++;
      if ((ix>=0) && (ix <width) && (iy>=0) && (iy <height))
	data[iy][ix/8] |= on_bit[7 - (ix%8)];
      iy++;
      if ((ix>=0) && (ix <width) && (iy>=0) && (iy <height))
	data[iy][ix/8] |= on_bit[7 - (ix%8)];
      iy++;
      if ((ix>=0) && (ix <width) && (iy>=0) && (iy <height))
	data[iy][ix/8] |= on_bit[7 - (ix%8)];
      ix--;
      if ((ix>=0) && (ix <width) && (iy>=0) && (iy <height))
	data[iy][ix/8] |= on_bit[7 - (ix%8)];
      ix--;
      if ((ix>=0) && (ix <width) && (iy>=0) && (iy <height))
	data[iy][ix/8] |= on_bit[7 - (ix%8)];
      iy--;
      if ((ix>=0) && (ix <width) && (iy>=0) && (iy <height))
	data[iy][ix/8] |= on_bit[7 - (ix%8)];
      iy--;
      if ((ix>=0) && (ix <width) && (iy>=0) && (iy <height))
	data[iy][ix/8] |= on_bit[7 - (ix%8)];
    }
  }
  print_raster();
  print_ps_tail();
  return (0);
}

static void
print_ps_header (void)
{
  printf("\\%%!PS-Adobe-2.0 EPSF-2.0\n");
  printf("%%%%BoundingBox: 0 0 %d %d\n", ((int) ceil(width*SCALE)), ((int) ceil(height*SCALE)));
  printf("%%%%EndProlog\n");

  printf("/picstr %d string def\n", cols);
  printf("/imageplot {{currentfile picstr readhexstring pop} image} def\n");
  printf("%f %f scale\n", width*SCALE, height*SCALE);
  printf("%d %d 1 [%d 0 0 %d 0 0] imageplot\n", width, height, width, height);
}

static void
print_ps_tail (void)
{
  printf("showpage\n");
  printf("%%%%Trailer\n");
}

static void
print_raster (void)
{
  unsigned char *datum, inverted;
  char hex_to_ascii();
  int i,j;

  for (j=0; j<height; j++) {
    datum = &data[j][0];
    for (i=0; i<cols; i++, datum++) {
      inverted = ~(*datum);
      printf("%c%c",  hex_to_ascii(inverted >> 4), hex_to_ascii(inverted & 0x0f));
    }
    printf("\n");
  }
}

char hex_to_ascii(hexnum)
     unsigned char hexnum;
{
  if (hexnum < 10)
    return('0' + hexnum);
  else
    return('a' + hexnum - 10);
}
