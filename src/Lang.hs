module Lang where

type Ident = String

type Binding = ([Ident], Expr)

data Val = Int
         | Nil

data Expr = Letrec [Binding] Expr
          | Let Binding Expr
          | App2 BinOp Expr Expr
          | App1 UnOp Expr
          | Cond Expr Expr Expr
          | Lit Val
          | Var Ident

data BinOp = Add
           | Sub
           | Mul
           | Div
           | Eq
           | Gt
           | GtE
           | Cons

data UnOp = Car
          | Cdr
          | IsNil
            
           

