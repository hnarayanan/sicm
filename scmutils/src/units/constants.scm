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

;;;; Numerical constants

(define-constant ':pi "\\pi" "Pi"
  (* 4 (atan 1 1))
  &unitless)

(define-constant ':2pi "2\\pi" "2*Pi"
  (* 8 (atan 1 1))
  &unitless)

(define-constant ':pi/2 "\\pi/2" "Pi/2"
  (* 2 (atan 1 1))
  &unitless)

(define-constant ':pi/4 "\\pi/4" "Pi/4"
  (atan 1 1)
  &unitless)

(define-constant ':pi/3 "\\pi/3" "Pi/3"
  (* 4/3 (atan 1 1))
  &unitless)

(define-constant ':pi/6 "\\pi/6" "Pi/6"
  (* 2/3 (atan 1 1))
  &unitless)

(define-constant ':-pi "-\\pi" "-Pi"
  (* -4 (atan 1 1))
  &unitless)

(define-constant ':-2pi "-2\\pi" "-2*Pi"
  (* -8 (atan 1 1))
  &unitless)

(define-constant ':-pi/2 "-\\pi/2" "-Pi/2"
  (* -2 (atan 1 1))
  &unitless)

(define-constant ':-pi/4 "-\\pi/4" "-Pi/4"
  (- (atan 1 1))
  &unitless)

(define-constant ':-pi/3 "-\\pi/3" "-Pi/3"
  (* -4/3 (atan 1 1))
  &unitless)

(define-constant ':-pi/6 "-\\pi/6" "-Pi/6"
  (* -2/3 (atan 1 1))
  &unitless)

(define-constant ':+pi "+\\pi" "+Pi"
  (* +4 (atan 1 1))
  &unitless)

(define-constant ':+2pi "+2\\pi" "+2*Pi"
  (* +8 (atan 1 1))
  &unitless)

(define-constant ':+pi/2 "+\\pi/2" "+Pi/2"
  (* +2 (atan 1 1))
  &unitless)

(define-constant ':+pi/4 "+\\pi/4" "+Pi/4"
  (+ (atan 1 1))
  &unitless)

(define-constant ':+pi/3 "+\\pi/3" "+Pi/3"
  (* +4/3 (atan 1 1))
  &unitless)

(define-constant ':+pi/6 "+\\pi/6" "+Pi/6"
  (* +2/3 (atan 1 1))
  &unitless)


(define-constant ':gamma "\\gamma" "Euler-Macheroni-constant"
  0.57721566490153286060651209008240243104215933593992
  &unitless)

;;;; Universal Physical Constants

(define-constant ':c "c" "speed of light"
  2.99792458e8
  (/ &meter &second))

#|
;;; (1986)
(define-constant ':G "G" "gravitational constant"
  6.67259e-11
  (/ (* &newton (square &meter)) (square &kilogram))
  128e-6)
|#				;relative error
 
;;; CODATA 2006
(define-constant ':G "G" "gravitational constant"
  6.67428e-11
  (/ (* &newton (square &meter)) (square &kilogram))
  1.0e-4)

;;; CODATA 2006
(define-constant ':e "e" "elementary charge"
  1.602176487e-19
  &coulomb
  2.5e-8)

;;; CODATA 2006
(define-constant ':h "h" "Planck constant"
  6.62606896e-34
  (* &joule &second)
  5.0e-8)

;;; CODATA 2006
(define-constant ':N_A "N_A" "Avogadaro constant"
  6.02214179e23
  (/ &unitless &mole)
  5.0e-8)

;;; CODATA 2002
(define-constant ':m_e "m_e" "electron mass"
  9.1093826e-31
  &kilogram
  1.7e-7)

;;; CODATA 2002
(define-constant ':m_p "m_p" "proton mass"
  1.67262171e-27
  &kilogram
  1.7e-7)

;;; CODATA 2002
(define-constant ':m_n "m_n" "neutron mass"
  1.67492728e-27
  &kilogram
  1.7e-7)

;;; CODATA 2002
(define-constant ':m_u "m_u" "atomic mass unit"
  1.66053886e-27
  &kilogram
  1.7e-7)

;;; CODATA 2002
(define-constant ':mu_e "\\mu_e" "electron magnetic moment"
  -9.28476412e-24
  (/ &joule &tesla)
  8.6e-8)

;;; CODATA 2002
(define-constant ':mu_p "\\mu_p" "proton magnetic moment"
  1.41060671e-26
  (/ &joule &tesla)
  8.7e-8)

;;; CODATA 2002
(define-constant ':gamma_p "\\gamma_p" "proton gyromagnetic ratio"
  2.67522205e8
  (/ &radian (* &tesla &second))
  8.6e-8)

(define-constant ':R_H "R_H" "quantum Hall resistance"
  25812.8056
  &ohm
  0.045e-6)


(define-constant ':R "R" "molar gas constant"
  8.314510
  (/ &joule (* &mole &kelvin))
  8.4e-6)

;;; CODATA 2002
(define-constant ':k "k" "Boltzmann constant"
  1.3806505e-23
  (/ &joule &kelvin)
  1.8e-6)


;;; The following are derived.  Units must check.

(define-constant ':h-bar "\\hbar" "Planck constant"
  (/ :h (* 2 :pi))
  (* &joule &second))

(define-constant ':F "F" "Faraday constant"
  (* :N_A :e)
  (/ &coulomb &mole)
  0.30e-6)

(define-constant ':mu_0 "\\mu_0" "permeability of free space"
  (* 4e-7 :pi)
  (/ &henry &meter))

(define-constant ':epsilon_0 "\\epsilon_0" "permittivity of free space"
  (/ 1
     (* :mu_0 (square :c)))
  (/ &farad &meter))

(define-constant ':Z0 "Z_0" "impedance of free space"
  (* :mu_0 :c)
  &ohm)

(define-constant ':alpha "\\alpha" "fine structure constant"
  (* (/ 1 (* 4 :pi :epsilon_0))
     (/ (square :e) (* :h-bar :c)))
  &unitless)				

(define-constant ':R_infinity "R_\\inf" "Rydberg constant"
  (/ (* :m_e :c (square :alpha))
     (* 2 :h))
  (/ &unitless &meter)
  0.0012e-6)

(define-constant ':r_e "r_e" "classical electron radius"
  (* (/ :h-bar (* :m_e :c)) :alpha)
  &meter
  0.013e-6)

(define-constant ':lambda_C "\\lambda_C" "electron Compton wavelength"
  (/ :h (* :m_e :c))
  &meter
  0.089e-6)

(define-constant ':a_0 "a_0" "Bohr radius"
  (/ :r_e (square :alpha))
  &meter
  0.045e-6)

(define-constant ':Phi_0 "\\Phi_0" "magnetic flux quantum"
  (/ :h (* 2 :e))
  &weber
  0.30e-6)

(define-constant ':h/2m_e "h/(2m_e)" "quantum of circulation"
  (/ :h (* 2 :m_e))
  (/ (square &meter) &second)
  0.089e-6)

(define-constant ':e/m_e "e/m_e" "specific electron charge"
  (/ (- :e) :m_e)
  (/ &coulomb &kilogram)
  0.30e-6)

(define-constant ':mu_B "\\mu_B" "Bohr magneton"
  (/ (* :e :h-bar) (* 2 :m_e))
  (/ &joule &tesla)
  0.34e-6)

(define-constant ':mu_e/mu_B "\\mu_e/\\mu_B" "electron magnetic moment ratio"
  (/ :mu_e :mu_B)
  &unitless
  1e-11)

(define-constant ':mu_N "\\mu_N" "nuclear magneton"
  (/ (* :e :h-bar) (* 2 :m_p))
  (/ &joule &tesla)
  0.34e-6)

(define-constant ':sigma "\\sigma" "Stefan-Boltzmann constant"
  (* (/ (square :pi) 60)
     (/ (expt :k 4)
	(* (expt :h-bar 3) (expt :c 2))))
  (/ &watt (* (square &meter) (expt &kelvin 4)))
  34.0e-6)

(define-constant ':sigma_T "\\sigma_T" "Thomson cross section"
  (* 8/3 :pi (square :r_e))
  (square &meter))

;;;; Observed and measured numbers

(define background-temperature		;Cobe 1994
  (& 2.726 &kelvin))			;+-.005 Kelvin



;;; Thermodynamic

(define water-freezing-temperature
  (& 273.15 &kelvin))

(define room-temperature
  (& 300.00 &kelvin))

(define water-boiling-temperature
  (& 373.15 &kelvin))



;;; Earth


(define earth-orbital-velocity
  (& 29.8e3 (/ &meter &second)))

(define earth-mass
  (& 5.976e24 &kilogram))

(define earth-radius			
  (& 6371e3 &meter))

(define earth-surface-area
  (& 5.101e14 (expt &meter 2)))

(define earth-escape-velocity
  (& 11.2e3 (/ &meter &second)))

(define earth-gravitational-acceleration
  (& 9.80665 (/ &meter (expt &second 2))))

(define :g 
  earth-gravitational-acceleration)

(define earth-mean-density
  (& 5.52e3 (/ &kilogram (expt &meter 3))))


;;;     This is the average amount of 
;;; sunlight available at Earth on an
;;; element of surface normal to a
;;; radius from the sun.  The actual
;;; power at the surface of the earth, 
;;; for a panel in full sunlight, is
;;; not very different, because, in 
;;; absence of clouds the atmosphere
;;; is quite transparent.  The number 
;;; differs from the obvious geometric
;;; number
;;; (/ sun-luminosity (* 4 :pi (square m/AU)))
;;; ;Value: 1360.454914748201
;;; because of the eccentricity of the
;;; Earth's orbit.

(define earth-incident-sunlight
  (& 1370. (/ &watt (expt &meter 2))))


(define vol@stp
  (& 2.24136e-2 (/ (expt &meter 3) &mole)))

(define sound-speed@stp			;c_s
  (& 331.45 (/ &meter &second)))

(define pressure@stp
  (& 101.325e3 &pascal))


(define earth-surface-temperature
  (+ (& 15 &kelvin)
     water-freezing-temperature))



;;; Sun

(define sun-mass
  (& 1.989e30 &kilogram))

(define :m_sun sun-mass)


(define sun-radius
  (& 6.9599e8 &meter))

(define :r_sun sun-radius)


(define sun-luminosity
  (& 3.826e26 &watt))

(define :l_sun sun-luminosity)


(define sun-surface-temperature
  (& 5770.0 &kelvin))

(define sun-rotation-period
  (& 2.14e6 &second))


;;; The Gaussian constant

(define GMsun				;=(* gravitational-constant sun-mass)	
  (& 1.32712497e20
     (/ (* &newton (expt &meter 2)) &kilogram)))


