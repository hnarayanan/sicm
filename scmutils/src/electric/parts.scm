#| -*-Scheme-*-

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

|#

(define :E12				;10% resistors
  '(1.0 1.2 1.5
    1.8 2.2 2.7
    3.3 3.9 4.7
    5.6 6.8 8.2))

(define :E24				;5% resistors
  '(1.0 1.1 1.2 1.3 1.5 1.6
    1.8 2.0 2.2 2.4 2.7 3.0
    3.3 3.6 3.9 4.3 4.7 5.1
    5.6 6.2 6.8 7.5 8.2 9.1))

(define :E96				;1% resistors
  '(1.00 1.02 1.05 1.07 1.10 1.13 1.15 1.18
    1.21 1.24 1.27 1.30 1.33 1.37 1.40 1.43
    1.47 1.50 1.54 1.58 1.62 1.65 1.69 1.74
    1.78 1.82 1.87 1.91 1.96 2.00 2.05 2.10
    2.15 2.21 2.26 2.32 2.37 2.43 2.49 2.55
    2.61 2.67 2.74 2.80 2.87 2.94 3.01 3.09
    3.16 3.24 3.32 3.40 3.48 3.57 3.65 3.74
    3.83 3.92 4.02 4.12 4.22 4.32 4.42 4.53
    4.64 4.75 4.87 4.99 5.11 5.23 5.36 5.49
    5.62 5.76 5.90 6.04 6.19 6.34 6.49 6.65
    6.81 6.98 7.15 7.32 7.50 7.68 7.87 8.06
    8.25 8.45 8.66 8.87 9.09 9.31 9.53 9.76))

(define :wire
  '(;((Bare Dia. mm) (Bare Dia. in.) AWG SWG (Enamel tpi))
    (2.640 0.1040 -- 12 9.09)
    (2.590 0.1019 10 -- 9.60)
    (2.340 0.0920 -- 13 10.20)
    (2.305 0.0970 11 -- 10.70)
    (2.040 0.0800 12 14 11.90)
    (1.820 0.0720 13 15 13.17)
    (1.630 0.0640 14 16 15.00)
    (1.450 0.0571 15 -- 16.80)
    (1.420 0.0560 -- 17 16.90)
    (1.290 0.0508 16 -- 18.90)
    (1.220 0.0480 -- 18 19.70)
    (1.150 0.0453 17 -- 21.20)
    (1.020 0.0400 18 19 23.50)
    (0.910 0.0360 19 20 26.20)
    (0.812 0.0320 20 21 29.20)
    (0.720 0.0280 21 22 33.00)
    (0.644 0.0253 22 -- 37.00)
    (0.610 0.0240 -- 23 38.30)
    (0.573 0.0226 23 -- 41.30)
    (0.560 0.0220 -- 24 41.60)
    (0.510 0.0200 24 25 45.50)
    (0.460 0.0180 25 26 51.00)
    (0.420 0.0164 -- 27 55.10)
    (0.405 0.0159 26 -- 58.00)
    (0.380 0.0148 -- 28 61.00)
    (0.361 0.0142 27 -- 64.90)
    (0.350 0.0136 -- 29 66.00)
    (0.320 0.0125 28 30 72.50)
    (0.290 0.0116 -- 31 77.50)
    (0.286 0.0113 29 -- 81.60)
    (0.270 0.0108 -- 32 82.70)
    (0.255 0.0100 30 33 88.00)
    (0.230 0.0092 -- 34 98.30)
    (0.227 0.0089 31 -- 101.0)
    (0.210 0.0084 -- 35 105.8)))

