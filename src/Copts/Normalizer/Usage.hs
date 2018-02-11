module Copts.Normalizer.Usage where


type Usage = [Pattern]

data Pattern = Command String
             | Argument String
             | Option [String] (Maybe (String, Maybe String))
             | Optional Usage
             | Required Usage
             | Exclusive [Usage]
             | Repeated Pattern
             deriving (Show, Eq)


