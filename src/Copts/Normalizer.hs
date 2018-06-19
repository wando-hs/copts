module Copts.Normalizer (normalize) where


import Copts.Normalizer.DetailedOptions (get, build, none, all)
import qualified Copts.Normalizer.Optionals as Optionals
import qualified Copts.Parser as P
import Copts.AST

import Data.List (head, map, tail)
import qualified Data.Set as Set
import Prelude (($), (.))


toAST details = map usage'
    where usage' = map pattern'

          pattern' (P.Exclusive p)           = Exclusive $ Set.fromList $ map usage' p
          pattern' (P.Repeated p)            = Repeated $ pattern' p
          pattern' (P.Optional u)            = Optional $ usage' u
          pattern' (P.Required u)            = Required $ usage' u
          pattern' (P.A option@(P.Option _)) = get details option
          pattern' (P.A (P.Argument name))   = Argument name
          pattern' (P.A (P.Command name))    = Command name
          pattern' P.Options                 = Optional $ all details

join usages = [root usages, exclusive usages]
    where exclusive = Exclusive .Set.fromList . map tail
          root = head . head


normalize :: P.Help -> Usage
normalize (P.Simple _ us)       = join $ Optionals.join $ toAST none us
normalize (P.Complex _ us opts) = join $ Optionals.join $ toAST (build opts) us
