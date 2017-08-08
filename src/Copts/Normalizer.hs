module Copts.Normalizer (Usage, Pattern (..), normalize) where

import qualified Copts.Parser as P
import qualified Data.Map.Strict as D
import Data.List (nub)
import Data.Bifunctor

type Usage = [Pattern]

data Pattern = Command String
             | Argument String
             | Option [String] (Maybe (String, Maybe String))
             | Optional Usage
             | Required Usage
             | Exclusive [Usage]
             | Repeated Pattern
             deriving (Show, Eq)

normalize'1 :: D.Map String Pattern -> P.Pattern -> Pattern
normalize'1 d (P.Optional u) = Optional $ normalize' d u
normalize'1 d (P.Required u) = Required $ normalize' d u
normalize'1 d (P.Exclusive p) = Exclusive $ map (normalize' d) p
normalize'1 d (P.Repeated p) = Repeated $ normalize'1 d p
normalize'1 d P.Options =  Optional . nub . D.elems $ d
normalize'1 d (P.A (P.Option opt)) = option d $ bimap label id opt
normalize'1 _ (P.A (P.Command name)) = Command name
normalize'1 _ (P.A (P.Argument name)) = Argument name

option :: D.Map String Pattern -> (String, Maybe String) -> Pattern
option d (name, param) = if D.member name d
                            then d D.! name
                            else Option [name] $ (\ l -> (l, Nothing)) <$> param

normalize' :: D.Map String Pattern -> P.Usage -> Usage
normalize' d = map (normalize'1 d)

normalize :: P.Help -> [Usage]
normalize (P.Simple _ us) = map (normalize' D.empty) us
normalize (P.Complex _ us options) = map (normalize' $ dict options) us
    where dict = D.fromList . concat . map details

label :: P.Flag -> String
label (P.Short name) = [name]
label (P.Long name) = name

details :: P.OptionDetail -> [(String, Pattern)]
details (P.Details flags param _) = map (\ l -> (l, opt)) labels
    where parameter (P.Parameter name value) = (name, value)
          opt = Option labels $ parameter <$> param
          labels = map label flags
