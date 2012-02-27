module Weather.Region.RegionMap where

import qualified Data.Map as Map

type SettingsMap = Map.Map String Float

data Region = {
	name		:: String,
	settings	:: SettingsMap,
	neighbors	:: [String]
	}
