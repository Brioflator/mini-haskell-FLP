
module Printing (showExp) where

import Exp
import Data.List (intercalate)

showVar :: Var -> String
showVar v = getVar v 

showExp :: ComplexExp -> String
showExp (CX v) = showVar v
showExp (Nat n) = show n
showExp(CLam v cexp) = "\\" ++ showVar v ++ " -> " ++ showExp cexp
showExp(Let v cexp1 cexp2) = "let " ++ showVar v ++ " := " ++ showExp cexp1 ++ " in " ++ showExp cexp2
showExp(LetRec v cexp1 cexp2) = "letrec " ++ showVar v ++ " := " ++ showExp cexp1 ++ " in " ++ showExp cexp2
showExp(List cexps) = "[" ++ intercalate ", " (map showExp cexps) ++ "]"
showExp(CApp cexp1 cexp2) = "(" ++ showExp cexp1 ++ " " ++ showExp cexp2 ++ ")"


