module Weather.Atmosphere.Pressure where

import Temperature

newtype Millibar = Millibar Float
		   deriving (Show, Eq, Ord)

calcAirPressure :: Float -> Kelvin -> Float -> Float -> Millibar
calcAirPressure mslp (Kelvin mtemp) grav alt = 
	Millibar (mslp * (1 - ((0.0065 * alt) / mtemp)) 
		  ** ((grav - 0.0289644) / (8.31447 - 0.0065)))

calcVaporPressure :: Celsius -> Millibar
calcVaporPressure (Celsius temp) =
	Millibar (6.1078 * (10 ** ((7.5 * temp) / (237.3 + temp))))
