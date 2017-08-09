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

toPattern :: D.Map String Pattern -> P.Pattern -> Pattern
toPattern d (P.Optional u) = Optional $ toUsage d u
toPattern d (P.Required u) = Required $ toUsage d u
toPattern d (P.Exclusive p) = Exclusive $ map (toUsage d) p
toPattern d (P.Repeated p) = Repeated $ toPattern d p
toPattern d P.Options =  Optional . nub . D.elems $ d
toPattern d (P.A (P.Option opt)) = toOption d $ bimap label id opt
toPattern _ (P.A (P.Command name)) = Command name
toPattern _ (P.A (P.Argument name)) = Argument name

toOption :: D.Map String Pattern -> (String, Maybe String) -> Pattern
toOption d (name, param) = D.findWithDefault def name d
    where def = Option [name] $ (\ l -> (l, Nothing)) <$> param

toUsage :: D.Map String Pattern -> P.Usage -> Usage
toUsage d = map (toPattern d)

label :: P.Flag -> String
label (P.Short name) = ['-', name]
label (P.Long name) =  "--" ++ name

toDetails :: P.OptionDetail -> [(String, Pattern)]
toDetails (P.Details flags param _) = map (\ l -> (l, opt)) labels
    where parameter (P.Parameter name value) = (name, value)
          opt = Option labels $ parameter <$> param
          labels = map label flags

normalize :: P.Help -> [Usage]
normalize (P.Simple _ us) = map (toUsage D.empty) us
normalize (P.Complex _ us opts) = map (toUsage $ dict opts) us
    where dict = D.fromList . concat . map toDetails
