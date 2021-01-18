module Language.Tosa 
    ( Expression(..)
    , Stack
    , Bindings
    , Context
    , emptyContext
    , eval
    , module Language.Tosa.Parser
    ) where

import Language.Tosa.Core
import Language.Tosa.Parser
