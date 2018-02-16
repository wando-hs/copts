module Copts.Normalizer.Optionals (join, group) where


import Copts.AST

group _ [] = []
group _ [x] = [[x]]
group pred (x:xs)
  | pred x = (x : takeWhile pred xs) : group pred (dropWhile pred xs)
  | otherwise = [x] : group pred xs

join' = map flatten . group isOption
    where extract (Optional usage) = usage

          isOption (Optional _) = True
          isOption _ = False

          flatten [pattern'] = pattern'
          flatten patterns = Optional $ concatMap extract patterns

bla option@(Option flags (Just (Parameter p _))) argument@(Argument a) =
  if p == a then (option, option) else (option, argument)
bla a b = (a, b)

haha = concat . map (uncurry bla) . pairs
    where pairs list = zip list (tail list)
          concat list = foldr ((:) . fst) [snd $ last list] list


join :: [Usage] -> [Usage]
join = map haha . map usage'
    where usage' = map pattern' . join'

          pattern' (Exclusive p) = Exclusive $ map usage' p
          pattern' (Repeated p) = Repeated $ pattern' p
          pattern' (Optional u) = Optional $ usage' u
          pattern' (Required u) = Required $ usage' u
          pattern' p = p

