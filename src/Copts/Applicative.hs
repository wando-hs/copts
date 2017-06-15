module Copts.Applicative ((<:>), (<++>)) where


import Control.Applicative (Applicative, liftA2)
import Prelude ((++))


(<:>) :: Applicative f => f a -> f [a] -> f [a]
head <:> tail = liftA2 (:) head tail

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
xs <++> ys = liftA2 (++) xs ys
