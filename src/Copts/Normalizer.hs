module Copts.Normalizer (Usage, Pattern (..), normalize) where


import Copts.Normalizer.DetailedOptions
import qualified Copts.Parser as P
import Copts.Normalizer.Usage

import Data.List (map)
import Prelude (($))


toUsage details = map usage'
    where usage' = map pattern'

          pattern' (P.Exclusive p) = Exclusive $ map usage' p
          pattern' (P.Repeated p) = Repeated $ pattern' p
          pattern' (P.Optional u) = Optional $ usage' u
          pattern' (P.Required u) = Required $ usage' u
          pattern' (P.A option@(P.Option _)) = get details option
          pattern' (P.A (P.Argument name)) = Argument name
          pattern' (P.A (P.Command name)) = Command name
          pattern' P.Options = Optional $ all details


normalize :: P.Help -> [Usage]
normalize (P.Simple _ us) = toUsage none us
normalize (P.Complex _ us options) = toUsage (build options) us
