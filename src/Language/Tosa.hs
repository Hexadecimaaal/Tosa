module Language.Tosa (

) where

import Data.Map (Map, empty, member, insert, (!))

data Expression =
    Name [Char]
    | Lambda [Char] Expression
    | Quote Expression
    deriving (Show, Eq, Ord)

type Stack = [Expression]

type Context = Map [Char] Expression

data Error =
    IsValue
    | NotApplicatable
    | StackEmptied
    deriving (Show, Eq)

-- under given Context and Stack, evaluate Expression, i.e.
-- if it's a function then apply it, else push it into the stack
eval :: Context -> Stack -> Expression -> Either Error Stack

-- evaluating "EVAL" will pop the stack, evaluate.
eval c [] (Name "EVAL") = Left StackEmptied
eval c (h:t) (Name "EVAL") = eval c t h

-- evaluating a generic name will match it in Context and
-- substitute if it's defined.
eval c s (Name n) = if member n c then Right $ (c ! n) : s else Right $ (Name n) : s

-- evaluating a Lambda will pop the stack, and
-- apply to the given Lambda.
eval c [] (Lambda _ _) = Left StackEmptied
eval c (h:t) (Lambda b o) = eval (insert b h c) t o

-- evaluating a Quote will push the Quoted contents
-- onto the stack, *without* evaluating them.
-- evaluate "EVAL" to force.
eval c s (Quote o) = Right $ o : s
