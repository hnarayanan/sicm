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


#|
(pp (lu-solve-linear-system (lu-hilbert 10) #(1 2 3 4 5 6 7 8 9 10)))
#(-11999.435131065547 641970.0000933461  -11023703.081043081 89180654.79719505
  -397509007.22415125 1049888726.4849118 -1683421110.3007996 1607494132.1032987
  -840061785.4403688  184828172.73657742)

(pp (gauss-jordan-solve-linear-system (lu-hilbert 10) #(1 2 3 4 5 6 7 8 9 10)))
#(-11998.641949647226 641927.3411961554  -11022984.623341404 89175018.79852961
  -397484774.62120646 1049827118.7729564 -1683326063.738778  1607406762.9493017
  -840017788.7911624  184818833.09312022)

(pp (full-pivot-solve-linear-system (lu-hilbert 10) #(1 2 3 4 5 6 7 8 9 10)))
#(-11999.286641821265 641961.3927420527  -11023553.196484178 89179461.28558874
  -397503846.48576456 1049875598.4922953 -1683400903.4390662 1607475631.0102098
  -840052514.5087323  184826215.35957083)

(pp (svd-solve-linear-system (lu-hilbert 10) #(1 2 3 4 5 6 7 8 9 10)))
#(-11995.662460948159 641786.1268410115  -11020804.123077387 89158966.82269388
  -397419031.33864605 1049666274.4133153 -1683085525.2003932 1607191277.1101058
  -839911602.4445388  184796704.64361978)
|#
