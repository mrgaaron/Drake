module Temperature where

newtype Fahrenheit = Fahrenheit Float
         	     deriving (Show, Eq, Ord)

newtype Celsius = Celsius Float
	          deriving (Show, Eq, Ord)

newtype Kelvin = Kelvin Float
                 deriving (Show, Eq, Ord)

convertFtoC :: Fahrenheit -> Celsius
convertFtoC (Fahrenheit temp) = 
	Celsius ((temp - 32.0) * (5.0/9.0))

convertCtoF :: Celsius -> Fahrenheit
convertCtoF (Celsius temp) = 
	Fahrenheit ((temp * (9.0/5.0)) + 32)

convertKtoC :: Kelvin -> Celsius
convertKtoC (Kelvin temp) = 
	Celsius (temp - 273.15)

convertCtoK :: Celsius -> Kelvin
convertCtoK (Celsius temp) = 
	Kelvin (temp + 273.15)
