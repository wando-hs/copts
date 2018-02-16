module Copts.AST where


data Parameter = Parameter String (Maybe String)
    deriving (Show, Eq)


data Flag = Short Char | Long String
    deriving (Eq)

instance Show Flag where
    show (Short name) = ['-', name]
    show (Long name) = "--" ++ name


data Pattern = Command String
             | Argument String
             | Option [Flag] (Maybe Parameter)
             | Optional Usage
             | Required Usage
             | Exclusive [Usage]
             | Repeated Pattern
             deriving (Show, Eq)


type Usage = [Pattern]

type AST = [Usage]
