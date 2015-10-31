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
;;; These math constants and more are in kernel/numeric.scm

(define :pi (* 4 (atan 1 1)))

(define :pi 3.14159265358979323846)

(define :exp 2.71828182845904523536)
|#

;;; Universal Physical constants

(define speed-of-light
  2.99792458e8)				; meter/sec

(define :c speed-of-light)


(define gravitational-constant		; +-128
  6.67259e-11)				; (Newton*meter^2)/kg^2

(define :G gravitational-constant)


(define elementary-charge
  1.60217733e-19)			; Coulomb
  ;;=4.8032068e-10 esu

(define :e elementary-charge)


(define Planck-constant
  6.6260755e-34)			; Joule*sec

(define :h Planck-constant)


(define h-bar
  (/ Planck-constant (* 2 :pi)))	; Joule*sec

(define :h-bar h-bar)


(define Avogadaro-constant
  6.0221367e23)				; 1/mol

(define :N_A Avogadaro-constant)


(define electron-mass			; kg
  9.1093897e-31)

(define :m_e electron-mass)


(define Faraday-constant		; Coulomb/mol
  (* Avogadaro-constant
     elementary-charge))


(define permeability			; A priori
  (* 4e-7 :pi))				; newtons/amp^2 = Henry/meter

(define :mu_0 permeability)


(define permittivity			; Coulomb/(volt*meter) = Farad/meter
  (/ 1
     (* permeability
	(square speed-of-light))))

(define :epsilon_0 permittivity)


(define fine-structure-constant
  (* (/ 1 (* 4 :pi permittivity))
     (/ (square elementary-charge)
	(* h-bar speed-of-light))))

(define :alpha fine-structure-constant) 


(define Rydberg-constant		; 1/meter
  (/ (* electron-mass speed-of-light
	(square fine-structure-constant))
     (* 2 Planck-constant)))

(define :R_infinity Rydberg-constant)

(define classical-electron-radius	; meters
  (* (/ h-bar
	(* electron-mass speed-of-light))
     fine-structure-constant))

(define :r_e classical-electron-radius)


(define electron-Compton-wavelength	; meters
  (/ Planck-constant
     (* electron-mass speed-of-light)))

(define :lambda_C electron-Compton-wavelength)


(define Bohr-radius			; meters
  (/ classical-electron-radius
     (square fine-structure-constant)))

(define :a_0 Bohr-radius)


(define atomic-mass-unit
  1.6605402e-27)			; kg

(define :m_u atomic-mass-unit)


(define proton-mass
  1.6726231e-27)			; kg

(define :m_p proton-mass)


(define neutron-mass
  1.6749286e-27)			; kg

(define :m_n neutron-mass)


(define magnetic-flux-quantum		; Webers
  (/ Planck-constant (* 2 elementary-charge)))

(define :Phi_0 magnetic-flux-quantum)


(define quantum-of-circulation		; meters^2/second
  (/ Planck-constant (* 2 electron-mass)))

(define specific-electron-charge	; Coulomb/kg
  (/ (- elementary-charge) electron-mass))

(define Bohr-magneton			; Joules/Tesla
  (/ (* elementary-charge h-bar)
     (* 2 electron-mass)))

(define :mu_B Bohr-magneton)


(define electron-magnetic-moment	; Joules/Tesla
  9.2847701e-24)

(define :mu_e electron-magnetic-moment)


(define electron-magnetic-moment-ratio
  (/ electron-magnetic-moment Bohr-magneton))


(define nuclear-magneton		; Joules/Tesla
  (/ (* elementary-charge h-bar)
     (* 2 proton-mass)))

(define :mu_N nuclear-magneton)


(define proton-magnetic-moment		; Joules/Tesla
  1.41060761e-26)

(define :mu_p proton-magnetic-moment)


(define proton-gyromagnetic-ratio	; radians/(second * Tesla)
  2.67522128e8)

(define :gamma_p proton-gyromagnetic-ratio)


(define quantum-Hall-resistance		; Ohms
  25812.8056)


(define molar-gas-constant
  8.314510)				; Joule/(mol*Kelvin)

(define :R molar-gas-constant)


(define Boltzmann-constant		; Joule/Kelvin
  1.380658e-23)		

(define :k Boltzmann-constant)


(define radiation
  (/ (* 8 (expt :pi 5) (expt Boltzmann-constant 4))
     (* 15 (expt speed-of-light 3) (expt Planck-constant 3))))

(define Stefan-Boltzmann-constant
  (/ (* speed-of-light radiation) 4))

(define Thomson-cross-section
  6.652440539306967e-29)		;m^2

;;; Observed and measured

(define background-temperature		;Cobe 1994
  2.726)				;+-.005 Kelvin



;;; Thermodynamic

(define water-freezing-temperature
  273.15)				;Kelvin

(define room-temperature
  300.00)				;Kelvin

(define water-boiling-temperature
  373.15)				;Kelvin



;;; Math Units


(define arcsec/radian
  (/ (* 60 60 360) (* 2 :pi)))



;;; Astronomical -- needs revision

(define m/AU
  1.4959787066e11)

(define m/pc
  (/ m/AU (tan (/ 1 arcsec/radian))))

(define AU/pc
  (/ 648000 :pi))			;=(/ m/pc m/AU)


(define sec/sidereal-yr			;1900
  3.1558149984e7)

(define sec/tropical-yr			;1900
  31556925.9747)

(define m/lyr
  (* speed-of-light sec/tropical-yr))

(define AU/lyr (/ m/lyr m/AU))

(define lyr/pc (/ m/pc m/lyr))

(define sec/day
  86400)



;;; Earth


(define earth-orbital-velocity
  29.8e3)				;meter/sec

(define earth-mass
  5.976e24)				;kg

(define earth-radius			
  6371e3)				;meters

(define earth-surface-area
  5.101e14)				;meter^2

(define earth-escape-velocity
  11.2e3)				;meter/sec

(define earth-grav-accel		;g
  9.80665)				;meter/sec^2

(define earth-mean-density
  5.52e3)				;kg/m^3


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
  1370.)				;watts/meter^2


(define vol@stp
  2.24136e-2)				;(meter^3)/mol

(define sound-speed@stp			;c_s
  331.45)				;meter/sec

(define pressure@stp
  101.325)				;kPa


(define earth-surface-temperature
  (+ 15 water-freezing-temperature))



;;; Sun

(define sun-mass
  1.989e30)				;kg

(define :m_sun sun-mass)


(define sun-radius
  6.9599e8)				;meter

(define :r_sun sun-radius)


(define sun-luminosity
  3.826e26)				;watts

(define :l_sun sun-luminosity)


(define sun-surface-temperature
  5770.0)				;Kelvin



(define sun-rotation-period
  2.14e6)				;sec


;;; The Gaussian constant

(define GMsun				;=(* gravitational-constant sun-mass)	
  1.32712497e20)



;;;  Physical Units -- everything in SI...


(define esu/coul
  (* 10 speed-of-light))

(define kg/amu				;=1/Avogadaro-constant*1000 
  (/ 1 (* 1000 Avogadaro-constant)))

#|
(define kg/amu
  1.6605e-27)
|#

(define joule/eV
  1.602e-19)

(define joule/cal
  4.1840)	

;;; should figure out ev/cal to determine the foundation.

