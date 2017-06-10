module Copts.Applicative ((<:>)) where


import Control.Applicative (liftA2)


(<:>) :: Applicative f => f a -> f [a] -> f [a]
head <:> tail = liftA2 (:) head tail
