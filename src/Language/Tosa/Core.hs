module Language.Tosa.Core 
    ( Expression(..)
    , Stack
    , Bindings
    , Context
    , emptyContext
    , eval
    ) where

import Data.Map (Map, empty, member, insert, delete, (!), findWithDefault, assocs)
import Data.List (intercalate)
import qualified Data.Text as T

data Expression 
    = Name T.Text
    | Quote Expression
    | Program [Expression]
    deriving (Eq, Ord)

instance Show Expression where
    show (Name s) = T.unpack s
    show (Quote o) = "(" ++ show o ++ ")"
    show (Program l) = "{" ++ intercalate " " (map show l) ++ "}"

newtype Stack = MkStack { getStack :: [Expression] }

instance Show Stack where
    show (MkStack s) = "Stack Size: " ++ show (length s) ++ "\n" ++
        (concatMap (++"\n") $ reverse $ zipWith (++) (map ((++": ") . show) [1..]) $ map show $ take 7 s)

type Bindings = Map T.Text Expression

instance {-# OVERLAPPING #-} Show Bindings where
    show b = assocs b >>= \(t, e) -> T.unpack t ++ " := " ++ show e ++ "\n"

type Context = (Bindings, Stack)

instance {-# OVERLAPPING #-} Show Context where
    show (b, s) = show b ++ "-----\n" ++ show s

emptyContext :: Context
emptyContext = (empty, MkStack [])

data Error 
    = IsValue
    | NotApplicatable Expression
    | NotAName Expression
    | StackEmptied
    deriving (Show, Eq)

-- under given Bindings and Stack, evaluate Expression, i.e.
-- if it's a function then apply it, else push it into the stack
eval :: Context -> Expression -> Either Error Context

-- evaluating "EVAL" will pop the stack, evaluate.
eval (b, MkStack [])    (Name "EVAL") = Left StackEmptied
eval (b, MkStack (h:t)) (Name "EVAL") = eval (b, MkStack t) h
 
eval (b, MkStack [])                    (Name "STO") = Left StackEmptied
eval (b, MkStack [_])                   (Name "STO") = Left StackEmptied
eval (b, MkStack (Name n : rhs : rest)) (Name "STO") = Right (insert n rhs b, MkStack rest)
eval (b, MkStack (e : _rhs : _rest))    (Name "STO") = Left $ NotAName e

eval (b, MkStack [])              (Name "PURGE") = Left StackEmptied
eval (b, MkStack (Name n : rest)) (Name "PURGE") = Right (delete n b, MkStack rest)
eval (b, MkStack (e : _rest))     (Name "PURGE") = Left $ NotAName e

eval (b, MkStack [])         (Name "QUOTE") = Left StackEmptied
eval (b, MkStack (e : rest)) (Name "QUOTE") = Right (b, MkStack $ Quote e : rest)

-- evaluating a generic name will match it in Bindings and
-- substitute if it's defined.
eval (b, s) (Name n) = if member n b 
    then eval (b, s) $ b ! n 
    else eval (b, s) $ Quote $ Name n

-- evaluating a Quote will push the Quoted contents
-- onto the stack, *without* evaluating them.
-- evaluate "EVAL" to force.
eval (b, s) (Quote o) = Right (b, MkStack (o : getStack s))


eval (b, s) (Program p) = fmap (\(_, s') -> (b, s')) $ evalProgram (b, s) p

-- evaluating a series of Expressions: evaluate the first step,
-- use the new environment to evaluate the rest.
evalProgram :: Context -> [Expression] -> Either Error Context
evalProgram (b, s) []    = Right (b, s)
evalProgram (b, s) (h:t) = do
    c <- eval (b, s) h
    evalProgram c t

-- >>> :{
-- >>> do c <- eval emptyContext $ Quote $ Program [(Name "QUOTE"), (Name "$1"), (Name "STO"), (Name "$1"), (Name "$1")]
-- >>>    c <- eval c $ Name "DUP"
-- >>>    c <- eval c $ Name "STO"
-- >>>    c <- eval c $ Name "x"
-- >>>    c <- eval c $ Name "DUP"
-- >>>    c <- eval c $ Name "y"
-- >>>    c <- eval c $ Name "DUP"
-- >>>    return c
-- >>> :}
-- Right DUP := {QUOTE $1 STO $1 $1}
-- -----
-- Stack Size: 4
-- 4: x
-- 3: x
-- 2: y
-- 1: y
-- <BLANKLINE>
--

