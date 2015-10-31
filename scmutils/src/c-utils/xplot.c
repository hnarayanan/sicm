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
/* linux: cc -o xplot ../../src/c-utils/xplot.c -L/usr/X11R6/lib -lX11 -lm */
/* on the sony: cc -verbose -o xplot xplot.c X11graphics.c -lX11 -lsocket -lnsl -lgen */
/* on 735s:     cc  -o xplot xplot.c -L/usr/lib/X11R5/ -I/usr/include/X11R5/ -lX11 */
/* not any more after HPUX 10 upgrade simply do cc -o xplot xplot.c -lX11 -lm */

/* the kludges are:
  (1) sleep to help synchronize 
  (2) resize doesn't work 
*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

typedef unsigned char byte;

Display   *theDisp;
int       theScreen;
Colormap  theCmap;
Window    rootW, mainW;
GC        theGC;
Visual    *theVisual;
XImage    *theImage, *expImage;
byte      *Image;
int       theDepth;
int       iWidth=500, iHeight=500, iPosx=0, iPosy=0;
int       eWidth, eHeight, ePosx, ePosy;

double xmin, xmax, ymin, ymax;
int DrawLine=False;
int DrawFrameLine=False;

static void HandleEvent (XEvent *);
static void NoResize (unsigned int, unsigned int);
#if 0
static void Resize (unsigned int, unsigned int);
#endif
static void load_image (void);
static void open_window (const char *);
static void Quit (void);
static void error (const char *);
static void Syntax (const char *);

int
main (int argc, const char ** argv)
{
  const char * cmd = (argv[0]);
  unsigned int i = 1;
  const char * display = 0;
  XEvent event;

  expImage = 0;

  if (argc == 1)
    Syntax (cmd);
  while (i < argc)
    {
      const char * arg = (argv[i++]);
      if ((index (arg, ':')) != 0)
	display = arg;
      else if (!strcmp (arg, "-w"))
	iWidth = (atoi (argv[i++]));
      else if (!strcmp (arg, "-h"))
	iHeight = (atoi (argv[i++]));
      else if (!strcmp (arg, "-l"))
	DrawLine = True;
      else if (!strcmp (arg, "-f"))
	DrawFrameLine = True;
      /* otherwise presume inputing the limits */
      else if ((i + 3) < argc)
	{
	  xmin = (atof (argv[i++]));
	  xmax = (atof (argv[i++]));
	  ymin = (atof (argv[i++]));
	  ymax = (atof (argv[i++]));
	}
      else
	Syntax (cmd);
    }

  open_window(display);
  system("sleep 1"); /* have a better idea??? */
  load_image();
  system("sleep 100000");
  theImage = XGetImage(theDisp, mainW, 0, 0, iWidth, iHeight, AllPlanes, ZPixmap);
  theDepth = theImage->depth;
  expImage = theImage;
  eWidth = iWidth;
  eHeight = iHeight;
  ePosx = iPosx;
  ePosy = iPosy;

  while(1) {
    XNextEvent(theDisp, &event);
    HandleEvent(&event);
  }

}

static void
HandleEvent (XEvent * event)
{
  switch (event->type) {
  case Expose:
    XPutImage(theDisp, mainW, theGC, expImage, 0, 0, 0, 0, 
	      ((unsigned int) eWidth), ((unsigned int) eHeight));
    break;
  case KeyPress: {
    XKeyEvent *key_event = (XKeyEvent *) event;
    char buf[128];
    KeySym ks;
    XComposeStatus status;
    XLookupString(key_event, buf, 128, &ks, &status);
    if(buf[0]=='q' || buf[0]=='Q') Quit();}
    break;
  case ConfigureNotify: {
    XConfigureEvent *conf_event = (XConfigureEvent *) event;
    if(conf_event->window == mainW)
      NoResize(conf_event->width, conf_event->height); /* we would if it worked */
      /*Resize(conf_event->width, conf_event->height); */ }
    break;
  case CirculateNotify:
  case MapNotify:
  case DestroyNotify:
  case GravityNotify:
  case ReparentNotify:
  case UnmapNotify:
    break;
  default:
    printf("unknown event type: %d\n", event->type);
    exit(-1);
  }
}

static void
NoResize (unsigned int w, unsigned int h)
{
  /* punt the resize */
  expImage = theImage;
  eWidth = iWidth;
  eHeight = iHeight;
}

#if 0
static void
Resize (unsigned int w, unsigned int h)
{
  byte *ximag;
  byte *ilptr, *ipptr, *elptr, *epptr;
  int ix, iy, ex, ey, ebytes_per_line, ibytes_per_line;
  int bytes_per_pad, bitmap_pad;
  int pix;

  if(expImage && expImage != theImage) XDestroyImage(expImage);
  eWidth = w;
  eHeight = h;

  ximag = (byte *) malloc(w*h);
  if(!ximag) {
    printf("cannot malloc %dx%d image\n", w, h);
    exit(-1);
  }


  bitmap_pad = theImage->bitmap_pad;
  bytes_per_pad = bitmap_pad/8;
  ebytes_per_line = bytes_per_pad*((theDepth*eWidth + bitmap_pad - 1)/bitmap_pad);

  expImage = XCreateImage(theDisp, theVisual, theDepth, ZPixmap, 0, ximag, 
			  ((unsigned int) eWidth), ((unsigned int) eHeight), 
			  bitmap_pad, ebytes_per_line);
  if(!expImage) {
    printf("cannot create %dx%d image\n", w, h);
    exit(-1);
  }

  for(ey=0; ey<eHeight; ey++) {
    iy =  ey*iHeight/eHeight;
    if(iy >= iHeight) {
      printf("."); fflush(stdout);
    } else {
      for(ex=0; ex<eWidth; ex++) {
	ix =  ex*iWidth/eWidth;
	if(ix >= iWidth) {
	  printf("."); fflush(stdout);
	} else {
	  pix = XGetPixel(theImage, ix, iy);
	  XPutPixel(expImage, ex, ey, pix);		
	}
      }
    }
  }

  /* would be faster if it worked ... 
  ibytes_per_line = theImage->bytes_per_line;
  elptr = epptr = (byte *) expImage->data;
  for(ey=0; ey<eHeight; ey++, elptr+=ebytes_per_line) {
    iy = (iHeight * ey) / eHeight; if(iy>=iHeight) iy = iHeight - 1;
    epptr = elptr;
    ilptr = (byte *) (theImage->data + (iy * ibytes_per_line));
    for(ex=0; ex<eWidth; ex++, epptr++) {
      ix = (iWidth*ex)/eWidth; if(ix>=iWidth) ix = iWidth - 1;
      ipptr = ilptr + ix;
      *epptr = *ipptr;
    }
  }
  */

}
#endif

/*****************************************************************/
static void
load_image (void)
{
  double x, y;
  int ix, iy, ixl = 0, iyl = 0;
  int First=1;

  if(DrawFrameLine) {
    XDrawLine(theDisp, mainW, theGC, 0, 0, 0, iHeight-1);
    XDrawLine(theDisp, mainW, theGC, iWidth-1, iHeight-1, 0, iHeight-1);
    XDrawLine(theDisp, mainW, theGC, iWidth-1, iHeight-1, iWidth-1, 0);
    XDrawLine(theDisp, mainW, theGC, iWidth-1, 0, 0, 0);
  }

  if(DrawLine) {
    while(scanf("%lf %lf", &x, &y) != EOF) {
      if(First) {
	ixl = iWidth*(x - xmin)/(xmax-xmin);
	iyl = iHeight*(ymax - y)/(ymax-ymin);
	First = 0;
      } else {
	ix = iWidth*(x - xmin)/(xmax-xmin);
	iy = iHeight*(ymax - y)/(ymax-ymin);
	XDrawLine(theDisp, mainW, theGC, ixl, iyl, ix, iy);
	XFlush(theDisp);
	ixl = ix;
	iyl = iy;
      }
    }
  } else {
    while(scanf("%lf %lf", &x, &y) != EOF) {
      if((x>xmin) && (x<xmax) && (y>ymin) && y<ymax) {
	ix = iWidth*(x - xmin)/(xmax-xmin);
	iy = iHeight*(ymax - y)/(ymax-ymin);
	XDrawPoint(theDisp, mainW, theGC, ix, iy);
	XFlush(theDisp);
      }
    }
  }

}

/*****************************************************************/
static void
open_window (const char * display)
{
  XSetWindowAttributes xswa;
  XSizeHints hints;
  unsigned int xswamask;
  int fcol, bcol;
  char **argv = 0;
  int argc=0;

  if( (theDisp = XOpenDisplay(display)) == NULL) error("Can't open display");

  theScreen = DefaultScreen(theDisp);
  theCmap = DefaultColormap(theDisp, theScreen);
  rootW = RootWindow(theDisp, theScreen);
  theGC = DefaultGC(theDisp, theScreen);
  fcol = WhitePixel(theDisp, theScreen);
  bcol = BlackPixel(theDisp, theScreen);
  theVisual = DefaultVisual(theDisp, theScreen);

  if(iWidth > DisplayWidth(theDisp, theScreen)) iWidth = DisplayWidth(theDisp, theScreen);
  if(iHeight > DisplayHeight(theDisp, theScreen)) iHeight = DisplayHeight(theDisp, theScreen);
  iPosx = XDisplayWidth(theDisp, theScreen) - iWidth;
  iPosy = 0;

  XSetForeground(theDisp, theGC, fcol);
  XSetBackground(theDisp, theGC, bcol);
  
  xswa.background_pixel = bcol;
  xswa.border_pixel = fcol;
  xswamask = CWBackPixel | CWBorderPixel;

  mainW = XCreateWindow(theDisp, rootW, iPosx, iPosy, 
			(unsigned int) iWidth, (unsigned int) iHeight, 
			2, 0, InputOutput, CopyFromParent, xswamask, &xswa);
  hints.flags = USPosition | USSize;
  hints.x = iPosx;
  hints.y = iPosy;
  hints.width = iWidth;
  hints.height = iHeight;
  XSetStandardProperties(theDisp, mainW, "xplot", "xplot", None, argv, argc, &hints);
  XSelectInput(theDisp, mainW, ExposureMask | KeyPressMask | StructureNotifyMask);
  XMapWindow(theDisp, mainW);
  XFlush(theDisp);
}
  
/*****************************************************************/
static void
Quit (void)
{
  exit(0);
}

static void
error (const char * str)
{
  printf("%s\n", str);
  exit(-1);
}

static void
Syntax (const char * cmd)
{
  printf("%s [display] [-w width] [-h height] xmin xmax ymin ymax\n", cmd);
  exit(-1);
}
