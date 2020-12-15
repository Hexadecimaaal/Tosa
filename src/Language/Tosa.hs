module Language.Tosa 
( Expression (..)
) where

import Data.Map (Map, empty, member, insert, (!), findWithDefault)
import Data.List (intercalate)
import qualified Data.Text as T

data Expression 
    = Name T.Text
    | Lambda T.Text Expression
    | Quote Expression
    | Program [Expression]
    deriving (Eq, Ord, Show)

-- instance Show Expression where
--     show (Name s) = T.unpack s
--     show (Lambda b o) = "\\" ++ T.unpack b ++ ". " ++ show o
--     show (Quote o) = "(" ++ show o ++ ")"
--     show (Program l) = "<<" ++ intercalate " " (map show l) ++ ">>"

type Stack = [Expression]

instance {-# OVERLAPPING #-} Show Stack where
    show [] = "(Empty Stack)"
    show s = concatMap (++"\n") $ reverse $ zipWith (++) (map ((++": ") . show) [1..]) $ map show s

type Bindings = Map T.Text Expression

type Context = (Bindings, Stack)

data Error 
    = IsValue
    | NotApplicatable
    | StackEmptied
    deriving (Show, Eq)

-- under given Bindings and Stack, evaluate Expression, i.e.
-- if it's a function then apply it, else push it into the stack
eval :: Context -> Expression -> Either Error Context

-- evaluating "EVAL" will pop the stack, evaluate.
eval (b, [])    (Name "EVAL") = Left StackEmptied
eval (b, (h:t)) (Name "EVAL") = eval (b, t) h

-- evaluating a generic name will match it in Bindings and
-- substitute if it's defined.
eval (b, s) (Name n) = Right $ (b,) $ (:s) $ findWithDefault (Name n) n b

-- evaluating a Lambda will pop the stack, and
-- apply to the given Lambda.
eval (b, [])    (Lambda _ _) = Left StackEmptied
eval (b, (h:t)) (Lambda f o) = eval ((insert f h b), t) o

-- evaluating a Quote will push the Quoted contents
-- onto the stack, *without* evaluating them.
-- evaluate "EVAL" to force.
eval (b, s) (Quote o) = Right $ (b, o : s)

-- evaluating a series Program: evaluate the first step,
-- use the new environment to evaluate the rest.
eval (b, s) (Program [])    = Right (b, s)
eval (b, s) (Program (h:t)) = do
    c <- eval (b, s) h
    eval c $ Program t
