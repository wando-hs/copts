module Copts.Normalizer.DetailedOptions
    (DetailedOptions, all, build, get, none) where


import qualified Copts.Parser as P
import Copts.AST

import Data.List (nub, map, concatMap)
import qualified Data.Map.Strict as Map
import Prelude (String, (.), ($), error, show)
import Data.Tuple (curry, swap)
import Data.Maybe (Maybe(..))
import Data.Functor ((<$>))


type DetailedOptions = Map.Map String Pattern


get ::  DetailedOptions -> P.Element -> Pattern
get details (P.Option (flag, parameter)) = findOrCreate
    where findOrCreate = Map.findWithDefault newOption (show flag) details
          newOption = Option [flag] $ (\n ->  Parameter n Nothing) <$> parameter
get _ _ = error ":("

all :: DetailedOptions -> [Pattern]
all = nub . Map.elems

build :: [P.OptionDetail] -> DetailedOptions
build = Map.fromList . concatMap (entries . toOption)
    where entries option@(Option labels _) = map (curry swap option . show) labels
          entries _ = error ":)"
          toOption (P.Details flags param _) = Option flags param

none :: DetailedOptions
none = Map.empty
