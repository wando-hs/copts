module Copts.AST where

import Data.Set (Set)

data Parameter = Parameter String (Maybe String)
    deriving (Show, Eq, Ord)

data Flag = Short Char | Long String
    deriving (Show, Eq, Ord)

data Pattern = Command String
             | Argument String
             | Option [Flag] (Maybe Parameter)
             | Optional Usage
             | Required Usage
             | Exclusive (Set Usage)
             | Repeated Pattern
             deriving (Show, Eq, Ord)


type Usage = [Pattern]

type AST = [Usage]
