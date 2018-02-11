module Copts.Normalizer.DetailedOptions
    (DetailedOptions, all, build, get, none) where


import qualified Copts.Parser as P
import Copts.Normalizer.Usage

import Data.List ((++), nub, map, concatMap)
import qualified Data.Map.Strict as Map
import Prelude (String, (.), ($))
import Data.Tuple (curry, swap)
import Data.Maybe (Maybe(..))
import Data.Functor ((<$>))


type DetailedOptions = Map.Map String Pattern


label (P.Short name) = ['-', name]
label (P.Long name) =  "--" ++ name


get ::  DetailedOptions -> P.Element -> Pattern
get details (P.Option (flag, parameter)) = findOrCreate
    where findOrCreate = Map.findWithDefault newOption (label flag) details
          newOption = Option [label flag] $ (curry swap Nothing) <$> parameter

all :: DetailedOptions -> [Pattern]
all = nub . Map.elems

build :: [P.OptionDetail] -> DetailedOptions
build = Map.fromList . concatMap entries . map toOption
    where entries option@(Option labels _) = map (curry swap option) labels
          toOption (P.Details flags param _) = Option (map label flags) (toTuple <$> param)
          toTuple (P.Parameter name value) = (name, value)

none :: DetailedOptions
none = Map.empty
