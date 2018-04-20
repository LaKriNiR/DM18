module GCLTypesAST

open System

type A =
  | Number of int
  | Name of String
  | Add of (A * A)
  | Subtr of (A * A)
  | Mult of (A * A)
  | Div of (A * A)
  | Power of (A * A)
  | UMinus of (A)

type B = 
  | True
  | False
  | And of (B * B)
  | Or of (B * B)
  | AndShort of (B * B)
  | OrShort of (B * B)
  | Not of B
  | Equality of (A * A)
  | Inequality of (A * A)
  | GreaterThan of (A * A)
  | GreaterOrEqual of (A * A)
  | LessThan of (A * A)
  | LessOrEqual of (A * A)

type C =
  | Assign of (A * A)
  | Skip
  | CList of (C * C)
  | If of GC
  | Do of GC
and GC =
  | Guarded of (B * C)
  | GCList of (GC * GC)
