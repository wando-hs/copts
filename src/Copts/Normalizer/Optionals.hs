module Copts.Normalizer.Optionals (join, group) where

import Copts.Normalizer.Usage


group _ [] = []
group _ [x] = [[x]]
group pred (x:xs)
  | pred x = (x : takeWhile pred xs) : group pred (dropWhile pred xs)
  | otherwise = [x] : group pred xs

join' = map flatten . group isOption
    where extract (Optional u) = u

          isOption (Optional _) = True
          isOption _ = False

          flatten [p] = p
          flatten patterns = Optional $ concatMap extract patterns


join :: [Usage] -> [Usage]
join = map usage'
    where usage' = map pattern' . join'

          pattern' (Exclusive p) = Exclusive $ map usage' p
          pattern' (Repeated p) = Repeated $ pattern' p
          pattern' (Optional u) = Optional $ usage' u
          pattern' (Required u) = Required $ usage' u
          pattern' p = p

