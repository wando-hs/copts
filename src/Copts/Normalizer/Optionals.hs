module Copts.Normalizer.Optionals (join) where


import Copts.AST

group _ [] = []
group _ [x] = [[x]]
group pred' (x:xs)
  | pred' x = (x : takeWhile pred' xs) : group pred' (dropWhile pred' xs)
  | otherwise = [x] : group pred' xs

joinOptionals = map flatten . group isOption
    where extract (Optional usage) = usage
          extract _ = error ":("

          isOption (Optional _) = True
          isOption _ = False

          flatten [pattern'] = pattern'
          flatten patterns = Optional $ concatMap extract patterns



resolveAmbiguousOptions = join' . map (uncurry resolve) . pairs
  where pairs [] = []
        pairs [p] = [(p, p)]
        pairs list = zip list (tail list)

        join' [] = []
        join' list@((a,b):_)
          | a == b = map snd list
          | otherwise = a : map snd list

        resolve option@(Option _ (Just (Parameter p _))) argument@(Argument a)
          | p == a = (option, option)
          | otherwise = (option, argument)
        resolve pattern1 pattern2 = (pattern1, pattern2)





join :: [Usage] -> [Usage]
join = map usage'
  where usage' = map pattern' . joinOptionals . resolveAmbiguousOptions

        pattern' (Exclusive p) = Exclusive $ map usage' p
        pattern' (Repeated p) = Repeated $ pattern' p
        pattern' (Optional u) = Optional $ usage' u
        pattern' (Required u) = Required $ usage' u
        pattern' p = p
