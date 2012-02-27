module Weather.Monad where

import qualified Data.Map as Map

type Condition = Map.Map String Float

newtype Weather a = Weather (Condition -> (a, Condition))

instance Monad Weather where
	Weather w1 >>= fw2 = Weather (\w0 -> let (r, s1) = w1 w0
						 Weather w2 = fw2 r in
                                             w2 s1)
	return k           = Weather (\s -> (k, s))

updateWeather :: (Condition -> Condition) -> Weather ()
updateWeather w = Weather (\c -> ((), w c))

--runWeather :: Condition -> Weather a -> (a,Condition)
runWeather w0 (Weather w) = w w0   
