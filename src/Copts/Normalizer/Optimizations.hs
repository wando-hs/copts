module Copts.Normalizer.Optimizations (run) where

import qualified Data.Set as Set

import Copts.AST

joinOptionals = foldr join' []
    where join' (Optional x) (Optional y : us) = Optional (x ++ y) : us
          join' u us = u : us

resolveAmbiguousOptions = foldr resolve []
    where resolve option@(Option _ (Just (Parameter parameter _)))
                  (argument@(Argument argname) : us)
            | parameter == argname = option : us
            | otherwise = option : argument : us
          resolve u us = u : us


run :: [Usage] -> [Usage]
run = map usage'
  where usage' = map pattern' . joinOptionals . resolveAmbiguousOptions

        pattern' (Exclusive p) = Exclusive $ Set.map usage' p
        pattern' (Repeated p) = Repeated $ pattern' p
        pattern' (Optional u) = Optional $ usage' u
        pattern' (Required u) = Required $ usage' u
        pattern' p = p
