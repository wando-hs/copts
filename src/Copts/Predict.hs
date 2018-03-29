module Copts.Predict (predict) where


import Copts.AST

import Control.Monad ((>>=), join)
import Data.Maybe (Maybe(..), catMaybes)
import Data.Either (Either(..), lefts)

import Data.Foldable (foldl, concatMap)
import Data.Traversable (traverse)
import Data.List ((++), isPrefixOf, map, nub)
import Prelude (String, ($), (.), (==), flip, otherwise)

type Path = [String]
type Completion = [String]

match :: Path -> String -> Either (Maybe String) Path
match []          current     = Left $ Just current
match [text]      current
  | text `isPrefixOf` current = Left $ Just current
  | otherwise                 = Left Nothing
match (text:path) current
  | text == current           = Right path
  | otherwise                 = Left Nothing

flagName (Short name) = '-' : [name]
flagName (Long name)  = "--" ++ name

matchParam :: Path -> Maybe Parameter -> Either (Maybe String) Path
matchParam []       _       = Left Nothing
matchParam path     Nothing = Right path
matchParam (_:path) _       = Right path

matchFlag :: Path -> Maybe Parameter -> Flag -> Either (Maybe String) Path
matchFlag []   _     flag = Left $ Just $ flagName flag
matchFlag path param flag = match path (flagName flag) >>= flip matchParam param

matchPattern :: Path -> Pattern -> [Either (Maybe String) Path]
matchPattern []       (Command name)       = [Left $ Just name]
matchPattern path     (Command name)       = [match path name]
matchPattern []       (Argument _)         = [Left Nothing]
matchPattern (_:path) (Argument _)         = [Right path]
matchPattern path     (Option flags param) = map (matchFlag path param) flags
matchPattern path     (Repeated pattern')  = matchPattern path pattern'
matchPattern path     (Required usage)     = matchUsage path usage
matchPattern path     (Optional usage)     = matchUsage path usage
matchPattern path     (Exclusive usages)   = concatMap (matchUsage path) usages

matchUsage :: Path -> Usage -> [Either (Maybe String) Path]
matchUsage _    []     = [Left Nothing]
matchUsage path (u:us) = foldl continue (matchPattern path u) us
    where continue paths pattern' = concatMap (findMatches pattern') paths
          findMatches pattern'    = map join . traverse (flip matchPattern pattern')

predict :: Usage -> Path -> Completion
predict usage = nub . catMaybes . lefts . flip matchUsage usage
