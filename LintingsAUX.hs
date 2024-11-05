module Lintings where

import AST
import LintTypes
import Parser

-- Función auxiliar que se fija si la variable pasada por parámetro pertenece a la lista que se pasa por parámetro
existeVarList :: Name -> [Name] -> Bool
existeVarList nom [] = False
existeVarList nom (x:xs) 
  | nom == x = True
  |otherwise = existeVarList nom xs;

  
-- Función auxiliar que arma la lista de variables de la expresión sin variables repetidas
variables' :: Expr -> [Name] -> [Name]
variables' e varList = case e of
  Var nom -> if existeVarList nom varList then varList else (nom : varList)
  Lit l -> varList
  App e1 e2 -> do
      let varListE1 = variables' e1 varList 
      let varList = variables' e2 varListE1
      varList
  Lam nom e1 -> variables' e1 varList
  Case e1 e2 (nom1, nom2, e3) -> do 
      let varListE1 = variables' e1 varList 
      let varListE2 = variables' e2 varListE1 
      let varList = variables' e3 varListE2
      varList
  If e1 e2 e3 -> do
      let varListE1 = variables' e1 varList
      let varListE2 = variables' e2 varListE1
      let varList = variables' e3 varListE2
      varList
  Infix op e1 e2 -> do
      let varListE1 = variables' e1 varList 
      let varList = variables' e2 varListE1
      varList;

-- Computa la lista de variables de una expresión
variables :: Expr -> [Name]
variables e = variables' e [] 

-- Función auxiliar que arma la lista de variables libres de una expresión sin repetir}
freeVariables' :: [Name] -> Expr -> [Name] -> [Name]
freeVariables' varLigList e varFreeList = case e of
  Var nom -> if existeVarList nom varLigList then varFreeList 
    else if existeVarList nom varFreeList then varFreeList
    else (nom : varFreeList)
  Lit l -> varFreeList
  App e1 e2 -> do
    let varFreeList' = freeVariables' varLigList e2 varFreeList
    let varFreeListResult = freeVariables' varLigList e1 varFreeList'
    varFreeListResult
  Lam nom e1 -> let
    varLigList' = (nom : varLigList)
    in freeVariables' varLigList' e1 varFreeList
  Case e1 e2 (nom1, nom2, e3) -> do
    let varLigList'= (nom1 : nom2 : varLigList)
    let varFreeListE1 = freeVariables' varLigList' e1 varFreeList 
    let varFreeListE2 = freeVariables' varLigList' e2 varFreeListE1 
    let varFreeList = freeVariables' varLigList' e3 varFreeListE2
    varFreeList
  If e1 e2 e3 -> do 
    let varFreeListE1 = freeVariables' varLigList e1 varFreeList
    let varFreeListE2 = freeVariables' varLigList e2 varFreeListE1
    let varFreeList = freeVariables' varLigList e3 varFreeListE2
    varFreeList
  Infix op e1 e2 -> do
    let varFreeListE1 = freeVariables' varLigList e1 varFreeList
    let varFreeList = freeVariables' varLigList e2 varFreeListE1
    varFreeList

-- Computa la lista de variables libres de una expresión
freeVariables :: Expr -> [Name]
freeVariables e = freeVariables' [] e []


--------------------------------------------------------------------------------
-- LINTINGS
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Computación de constantes
--------------------------------------------------------------------------------

-- Función auxiliar que toma un Lit LintInt y devuelve valor		
evaluarInt :: Lit -> Integer
evaluarInt (LitInt e) = e
evaluarInt (_) = 0
  
-- Función auxiliar que toma un Lit LintBool y devuelve valor		
evaluarBool :: Lit -> Bool
evaluarBool (LitBool e) = e
evaluarBool (_) = False

-- Función auxiliar para evaluar las operaciones sobre lints  
evaluar :: Op -> Lit -> Lit -> Lit
evaluar Add e1 e2 = do
  let v1 = evaluarInt e1
  let v2 = evaluarInt e2
  let v3 = v1 + v2
  LitInt v3
evaluar Sub e1 e2 = do
  let v1 = evaluarInt e1
  let v2 = evaluarInt e2
  let v3 = v1 - v2 
  LitInt v3
evaluar Mult e1 e2 = do
  let v1 = evaluarInt e1
  let v2 = evaluarInt e2
  let v3 = v1 * v2 
  LitInt v3
evaluar Div e1 e2 = do
  let v1 = evaluarInt e1
  let v2 = evaluarInt e2
  let v3 = div v1 v2 
  LitInt v3
evaluar Eq e1 e2 = do
  let v1 = evaluarInt e1
  let v2 = evaluarInt e2
  let v3 = v1 == v2 
  LitBool v3
evaluar NEq e1 e2 = do
  let v1 = evaluarInt e1
  let v2 = evaluarInt e2
  let v3 = v1 /= v2 
  LitBool v3
evaluar GTh e1 e2 = do
  let v1 = evaluarInt e1
  let v2 = evaluarInt e2
  let v3 = v1 > v2 
  LitBool v3
evaluar LTh e1 e2 = do
  let v1 = evaluarInt e1
  let v2 = evaluarInt e2
  let v3 = v1 < v2 
  LitBool v3
evaluar GEq e1 e2 = do
  let v1 = evaluarInt e1
  let v2 = evaluarInt e2
  let v3 = v1 >= v2 
  LitBool v3
evaluar LEq e1 e2 = do
  let v1 = evaluarInt e1
  let v2 = evaluarInt e2
  let v3 = v1 <= v2 
  LitBool v3
evaluar And e1 e2 = do
  let v1 = evaluarBool e1
  let v2 = evaluarBool e2
  let v3 = v1 && v2 
  LitBool v3
evaluar Or e1 e2 = do
  let v1 = evaluarBool e1
  let v2 = evaluarBool e2
  let v3 = v1 || v2 
  LitBool v3

-- Función auxiliar que evalúa una Expr
evalComputeConstant :: Expr -> (Expr, [LintSugg])
evalComputeConstant e = case e of
  Var nom -> (Var nom, [])
  Lit lit -> (Lit lit, [])
  App e1 e2 -> do
    let (eResult1, listSug1)= evalComputeConstant e1
    let (eResult2, listSug2)= evalComputeConstant e2
    (App eResult1 eResult2, listSug1 ++ listSug2)
  Lam nom e1 -> do
    let (result, listSug) = evalComputeConstant e1
    (Lam nom result, listSug)
  Case e1 e2 (nom1, nom2, e3) -> do
    let (eResult1, listSug1) = evalComputeConstant e1
    let (eResult2, listSug2) = evalComputeConstant e2
    let (eResult3, listSug3) = evalComputeConstant e3
    (Case eResult1 eResult2 (nom1, nom2, eResult3), listSug3 ++ listSug2 ++ listSug1)
  If e1 e2 e3 -> do
    let (eResult1, listSug1) = evalComputeConstant e1
    let (eResult2, listSug2) = evalComputeConstant e2
    let (eResult3, listSug3) = evalComputeConstant e3
    (If eResult1 eResult2 eResult3, listSug2 ++ listSug3 ++ listSug1)
  Infix op e1 e2 -> do
    let (eResult1, listSug1)= evalComputeConstant e1
    let (eResult2, listSug2)= evalComputeConstant e2
    case eResult1 of
      Lit l1 -> case eResult2 of
        Lit l2 -> do
          let sugg = evaluar op l1 l2
          let expSugg = LintCompCst (Infix op (Lit l1) (Lit l2)) (Lit sugg)
          ((Lit sugg), listSug1 ++ listSug2 ++ expSugg : [])
        otherwise -> (Infix op eResult1 eResult2, listSug1 ++ listSug2)
      otherwise -> (Infix op eResult1 eResult2, listSug1 ++ listSug2)

--------------------------------------------------------------------------------

-- Reduce expresiones aritméticas/booleanas
-- Construye sugerencias de la forma (LintCompCst e r)
lintComputeConstant :: Linting Expr
lintComputeConstant e = do
  let (result, listSug) = evalComputeConstant e
  (result, listSug)

--------------------------------------------------------------------------------
-- Eliminación de neutros
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- Función auxiliar que evalúa una Expr y si es una suma realiza el cálculo en caso de que se sume cero
esCero :: Lit -> Bool
esCero(LitInt i) = (i==0)
esCero (_) = False

-- Función auxiliar que evalúa una Expr y elimina el neutro de la suma
evalLintAdd0 :: Expr -> (Expr, [LintSugg])
evalLintAdd0 e = case e of
  Var nom -> (Var nom, [])
  Lit lit -> (Lit lit, [])
  App e1 e2 -> do
    let (eResult1, listSug1)= evalLintAdd0 e1
    let (eResult2, listSug2)= evalLintAdd0 e2
    (App eResult1 eResult2, listSug2 ++ listSug1)
  Lam nom e1 -> do
    let (result, listSug) = evalLintAdd0 e1
    (Lam nom result, listSug)
  Case e1 e2 (nom1, nom2, e3) -> do
    let (eResult1, listSug1) = evalLintAdd0 e1
    let (eResult2, listSug2) = evalLintAdd0 e2
    let (eResult3, listSug3) = evalLintAdd0 e3
    (Case eResult1 eResult2 (nom1, nom2, eResult3), listSug3 ++ listSug2 ++ listSug1)
  If e1 e2 e3 -> do
    let (eResult1, listSug1) = evalLintAdd0 e1
    let (eResult2, listSug2) = evalLintAdd0 e2
    let (eResult3, listSug3) = evalLintAdd0 e3
    (If eResult1 eResult2 eResult3, listSug3 ++ listSug2 ++ listSug1)
  Infix op e1 e2 -> do
    let (eResult1, listSug1)= evalLintAdd0 e1
    let (eResult2, listSug2)= evalLintAdd0 e2
    case op of
      Add -> case eResult1 of
        Lit l1 -> if esCero l1 then do
            let expSugg = LintNeut op (Infix op (Lit l1) eResult2) (eResult2)
            (eResult2, listSug2 ++ listSug1 ++ expSugg : [])
          else
            case eResult2 of
              Lit l2 -> if esCero l2 then do
                  let expSugg = LintNeut op (Infix op (Lit l1) (Lit l2)) (Lit l1)  
                  (Lit l1, listSug2 ++ listSug1 ++ expSugg : [])
                else (Infix op eResult1 eResult2, listSug2 ++ listSug1)
              otherwise -> (Infix op eResult1 eResult2, listSug2 ++ listSug1)
        otherwise -> case eResult2 of
              Lit l2 -> if esCero l2 then do
                  let expSugg = LintNeut op (Infix op (eResult1) (Lit l2)) (eResult1)  
                  (eResult1, listSug2 ++ listSug1 ++ expSugg : [])
                else (Infix op eResult1 eResult2, listSug2 ++ listSug1)
              otherwise -> (Infix op eResult1 eResult2, listSug1 ++ listSug2)
      otherwise -> (Infix op eResult1 eResult2, listSug2 ++ listSug1)

-- Elimina neutro de la suma
-- Construye sugerencias de la forma (LintNeut Add e r)
lintAdd0 :: Linting Expr
lintAdd0 e = do
  let (result, listSug) = evalLintAdd0 e
  (result, listSug)

--------------------------------------------------------------------------------

-- Función auxiliar que evalúa una Expr y si es una multiplicación realiza el cálculo en caso de que se multiplique 1
esUno :: Lit -> Bool
esUno(LitInt i) = (i==1)
esUno (_) = False

-- Función auxiliar que evalúa una Expr y elimina el neutro de la mult
evalLintMult1 :: Expr -> (Expr, [LintSugg])
evalLintMult1 e = case e of
  Var nom -> (Var nom, [])
  Lit lit -> (Lit lit, [])
  App e1 e2 -> do
    let (eResult1, listSug1)= evalLintMult1 e1
    let (eResult2, listSug2)= evalLintMult1 e2
    (App eResult1 eResult2, listSug1 ++ listSug2)
  Lam nom e1 -> do
    let (result, listSug) = evalLintMult1 e1
    (Lam nom result, listSug)
  Case e1 e2 (nom1, nom2, e3) -> do
    let (eResult1, listSug1) = evalLintMult1 e1
    let (eResult2, listSug2) = evalLintMult1 e2
    let (eResult3, listSug3) = evalLintMult1 e3
    (Case eResult1 eResult2 (nom1, nom2, eResult3), listSug1 ++ listSug2 ++ listSug3)
  If e1 e2 e3 -> do
    let (eResult1, listSug1) = evalLintMult1 e1
    let (eResult2, listSug2) = evalLintMult1 e2
    let (eResult3, listSug3) = evalLintMult1 e3
    (If eResult1 eResult2 eResult3, listSug1 ++ listSug2 ++ listSug3)
  Infix op e1 e2 -> do
    let (eResult1, listSug1)= evalLintMult1 e1
    let (eResult2, listSug2)= evalLintMult1 e2
    case op of
      Mult -> case eResult1 of
        Lit l1 -> if esUno l1 then do
            let expSugg = LintNeut op (Infix op (Lit l1) eResult2) (eResult2)
            (eResult2, expSugg : listSug1 ++ listSug2)
          else do
            case eResult2 of
              Lit l2 -> if esUno l2 then do
                  let expSugg = LintNeut op (Infix op (Lit l1) (Lit l2)) (Lit l1)  
                  (Lit l1, expSugg : listSug1 ++ listSug2)
                else (Infix op eResult1 eResult2, listSug1 ++ listSug2)
              otherwise -> (Infix op eResult1 eResult2, listSug1 ++ listSug2)
        otherwise -> case eResult2 of
              Lit l2 -> if esUno l2 then do
                  let expSugg = LintNeut op (Infix op (eResult1) (Lit l2)) (eResult1)  
                  (eResult1, expSugg : listSug1 ++ listSug2)
                else (Infix op eResult1 eResult2, listSug1 ++ listSug2)
              otherwise -> (Infix op eResult1 eResult2, listSug1 ++ listSug2)
      otherwise -> (Infix op eResult1 eResult2, listSug1 ++ listSug2)

-- Elimina neutros del producto
-- Construye sugerencias de la forma (LintNeut Mult e r)
lintMult1 :: Linting Expr
lintMult1 e = do
  let (result, listSug) = evalLintMult1 e
  (result, listSug)

--------------------------------------------------------------------------------

-- Función auxiliar que evalúa una Lit y se fija si es un booleano True
esTrue :: Lit -> Bool
esTrue(LitBool i) = (i==True)
esTrue (_) = False

-- Función auxiliar que evalúa una Expr y elimina el neutro de la conjunción
evalLintConjAnd :: Expr -> (Expr, [LintSugg])
evalLintConjAnd e = case e of
  Var nom -> (Var nom, [])
  Lit lit -> (Lit lit, [])
  App e1 e2 -> do
    let (eResult1, listSug1) = evalLintConjAnd e1
    let (eResult2, listSug2) = evalLintConjAnd e2
    (App eResult1 eResult2, listSug1 ++ listSug2)
  Lam nom e1 -> do
    let (result, listSug) = evalLintConjAnd e1
    (Lam nom result, listSug)
  Case e1 e2 (nom1, nom2, e3) -> do
    let (eResult1, listSug1) = evalLintConjAnd e1
    let (eResult2, listSug2) = evalLintConjAnd e2
    let (eResult3, listSug3) = evalLintConjAnd e3
    (Case eResult1 eResult2 (nom1, nom2, eResult3), listSug1 ++ listSug2 ++ listSug3)
  If e1 e2 e3 -> do
    let (eResult1, listSug1) = evalLintConjAnd e1
    let (eResult2, listSug2) = evalLintConjAnd e2
    let (eResult3, listSug3) = evalLintConjAnd e3
    (If eResult1 eResult2 eResult3, listSug1 ++ listSug2 ++ listSug3)
  Infix op e1 e2 -> do
    let (eResult1, listSug1)= evalLintConjAnd e1
    let (eResult2, listSug2)= evalLintConjAnd e2
    case op of
      And -> case eResult1 of
        Lit l1 -> if esTrue l1 then do
            let expSugg = LintNeut op (Infix op (Lit l1) eResult2) (eResult2)
            (eResult2, expSugg : listSug1 ++ listSug2)
          else do
            case eResult2 of
              Lit l2 -> if esTrue l2 then do
                  let expSugg = LintNeut op (Infix op (Lit l1) (Lit l2)) (Lit l1)  
                  (Lit l1, expSugg : listSug1 ++ listSug2)
                else (Infix op eResult1 eResult2, listSug1 ++ listSug2)
              otherwise -> (Infix op eResult1 eResult2, listSug1 ++ listSug2)
        otherwise -> case eResult2 of
              Lit l2 -> if esTrue l2 then do
                  let expSugg = LintNeut op (Infix op (eResult1) (Lit l2)) (eResult1)  
                  (eResult1, expSugg : listSug1 ++ listSug2)
                else (Infix op eResult1 eResult2, listSug1 ++ listSug2)
              otherwise -> (Infix op eResult1 eResult2, listSug1 ++ listSug2)
      otherwise -> (Infix op eResult1 eResult2, listSug1 ++ listSug2)

-- Elimina neutros de la conjunción
-- Construye sugerencias de la forma (LintNeut And e r)
lintAndTrue :: Linting Expr
lintAndTrue e =  do
  let (result, listSug) = evalLintConjAnd e
  (result, listSug)

--------------------------------------------------------------------------------

-- Función auxiliar que evalúa una Lit y se fija si es un booleano False
esFalse :: Lit -> Bool
esFalse(LitBool i) = (i==False)
esFalse (_) = False

-- Función auxiliar que evalúa una Expr y elimina el neutro de la disyunción
evalLintDisyOr :: Expr -> (Expr, [LintSugg])
evalLintDisyOr e = case e of
  Var nom -> (Var nom, [])
  Lit lit -> (Lit lit, [])
  App e1 e2 -> do
    let (eResult1, listSug1) = evalLintDisyOr e1
    let (eResult2, listSug2) = evalLintDisyOr e2
    (App eResult1 eResult2, listSug1 ++ listSug2)
  Lam nom e1 -> do
    let (result, listSug) = evalLintDisyOr e1
    (Lam nom result, listSug)
  Case e1 e2 (nom1, nom2, e3) -> do
    let (eResult1, listSug1) = evalLintDisyOr e1
    let (eResult2, listSug2) = evalLintDisyOr e2
    let (eResult3, listSug3) = evalLintDisyOr e3
    (Case eResult1 eResult2 (nom1, nom2, eResult3), listSug1 ++ listSug2 ++ listSug3)
  If e1 e2 e3 -> do
    let (eResult1, listSug1) = evalLintDisyOr e1
    let (eResult2, listSug2) = evalLintDisyOr e2
    let (eResult3, listSug3) = evalLintDisyOr e3
    (If eResult1 eResult2 eResult3, listSug1 ++ listSug2 ++ listSug3)
  Infix op e1 e2 -> do
    let (eResult1, listSug1)= evalLintDisyOr e1
    let (eResult2, listSug2)= evalLintDisyOr e2
    case op of
      Or -> case eResult1 of
        Lit l1 -> if esFalse l1 then do
            let expSugg = LintNeut op (Infix op (Lit l1) eResult2) (eResult2)
            (eResult2, expSugg : listSug1 ++ listSug2)
          else
            case eResult2 of
              Lit l2 -> if esFalse l2 then do
                  let expSugg = LintNeut op (Infix op (Lit l1) (Lit l2)) (Lit l1)  
                  (Lit l1, expSugg : listSug1 ++ listSug2)
                else (Infix op eResult1 eResult2, listSug1 ++ listSug2)
              otherwise -> (Infix op eResult1 eResult2, listSug1 ++ listSug2)
        otherwise -> case eResult2 of
              Lit l2 -> if esFalse l2 then do
                  let expSugg = LintNeut op (Infix op (eResult1) (Lit l2)) (eResult1)  
                  (eResult1, expSugg : listSug1 ++ listSug2)
                else (Infix op eResult1 eResult2, listSug1 ++ listSug2)
              otherwise -> (Infix op eResult1 eResult2, listSug1 ++ listSug2)
      otherwise -> (Infix op eResult1 eResult2, listSug1 ++ listSug2)

-- Elimina neutros de la disyunción
-- Construye sugerencias de la forma (LintNeut Or e r)
lintOrFalse :: Linting Expr
lintOrFalse e = do
  let (result, listSug) = evalLintDisyOr e
  (result, listSug)


--------------------------------------------------------------------------------
-- Eliminación de if redundantes
--------------------------------------------------------------------------------

-- Función auxiliar que evalúa una Expr y elimina el neutro de la disyunción
evalLintIfCond :: Expr -> (Expr, [LintSugg])
evalLintIfCond e = case e of
  Var nom -> (Var nom, [])
  Lit lit -> (Lit lit, [])
  App e1 e2 -> do
    let (eResult1, listSug1) = evalLintIfCond e1
    let (eResult2, listSug2) = evalLintIfCond e2
    (App eResult1 eResult2, listSug2 ++ listSug1)
  Lam nom e1 -> do
    let (result, listSug) = evalLintIfCond e1
    (Lam nom result, listSug)
  Case e1 e2 (nom1, nom2, e3) -> do
    let (eResult1, listSug1) = evalLintIfCond e1
    let (eResult2, listSug2) = evalLintIfCond e2
    let (eResult3, listSug3) = evalLintIfCond e3
    (Case eResult1 eResult2 (nom1, nom2, eResult3), listSug3 ++ listSug2 ++ listSug1)
  Infix op e1 e2 -> do
    let (eResult1, listSug1) = evalLintIfCond e1
    let (eResult2, listSug2) = evalLintIfCond e2
    (Infix op eResult1 eResult2, listSug1 ++ listSug2)
  If e1 e2 e3-> do
    let (eResult1, listSug1)= evalLintIfCond e1
    let (eResult2, listSug2)= evalLintIfCond e2
    let (eResult3, listSug3)= evalLintIfCond e3
    case eResult1 of
        Lit l1 -> if esFalse l1 then do
            let expSugg = LintRedIf (If (Lit l1) eResult2 eResult3) (eResult3)
            (eResult3, listSug1 ++ listSug2 ++ listSug3 ++ expSugg : [])
          else if esTrue l1 then do
            let expSugg = LintRedIf (If (Lit l1) eResult2 eResult3) (eResult2)
            (eResult2, listSug1 ++ listSug2 ++ listSug3 ++ expSugg : [])
          else (If eResult1 eResult2 eResult3, listSug1 ++ listSug2 ++ listSug3 )
        otherwise -> (If eResult1 eResult2 eResult3, listSug1 ++ listSug2 ++ listSug3)

--------------------------------------------------------------------------------
-- Sustitución de if con literal en la condición por la rama correspondiente
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfCond :: Linting Expr
lintRedIfCond e = do
  let (result, listSug) = evalLintIfCond e
  (result, listSug)

--------------------------------------------------------------------------------

-- Función auxiliar que evalúa una Expr y elimina el neutro de la disyunción
evalLintIfRedAnd :: Expr -> (Expr, [LintSugg])
evalLintIfRedAnd e = case e of
  Var nom -> (Var nom, [])
  Lit lit -> (Lit lit, [])
  App e1 e2 -> do
    let (eResult1, listSug1) = evalLintIfRedAnd e1
    let (eResult2, listSug2) = evalLintIfRedAnd e2
    (App eResult1 eResult2, listSug1 ++ listSug2)
  Lam nom e1 -> do
    let (result, listSug) = evalLintIfRedAnd e1
    (Lam nom result, listSug)
  Case e1 e2 (nom1, nom2, e3) -> do
    let (eResult1, listSug1) = evalLintIfRedAnd e1
    let (eResult2, listSug2) = evalLintIfRedAnd e2
    let (eResult3, listSug3) = evalLintIfRedAnd e3
    (Case eResult1 eResult2 (nom1, nom2, eResult3), listSug1 ++ listSug2 ++ listSug3)
  Infix op e1 e2 -> do
    let (eResult1, listSug1) = evalLintIfRedAnd e1
    let (eResult2, listSug2) = evalLintIfRedAnd e2
    (Infix op eResult1 eResult2, listSug1 ++ listSug2)
  If e1 e2 e3-> do
    let (eResult1, listSug1)= evalLintIfRedAnd e1
    let (eResult2, listSug2)= evalLintIfRedAnd e2
    let (eResult3, listSug3)= evalLintIfRedAnd e3
    case eResult3 of
        Lit l3 -> if esFalse l3 then do
            let expSugg = LintRedIf (If eResult1 eResult2 (Lit l3)) (Infix And eResult1 eResult2)
            (Infix And eResult1 eResult2, expSugg : listSug1 ++ listSug2 ++ listSug3)
          else (If eResult1 eResult2 eResult3, listSug1 ++ listSug2 ++ listSug3)
        otherwise -> (If eResult1 eResult2 eResult3, listSug1 ++ listSug2 ++ listSug3)

-- Sustitución de if por conjunción entre la condición y su rama _then_
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfAnd :: Linting Expr
lintRedIfAnd e = do
  let (result, listSug) = evalLintIfRedAnd e
  (result, listSug)
--------------------------------------------------------------------------------

-- Función auxiliar que evalúa una Expr y elimina el neutro de la disyunción
evalLintIfRedOr :: Expr -> (Expr, [LintSugg])
evalLintIfRedOr e = case e of
  Var nom -> (Var nom, [])
  Lit lit -> (Lit lit, [])
  App e1 e2 -> do
    let (eResult1, listSug1) = evalLintIfRedOr e1
    let (eResult2, listSug2) = evalLintIfRedOr e2
    (App eResult1 eResult2, listSug1 ++ listSug2)
  Lam nom e1 -> do
    let (result, listSug) = evalLintIfRedOr e1
    (Lam nom result, listSug)
  Case e1 e2 (nom1, nom2, e3) -> do
    let (eResult1, listSug1) = evalLintIfRedOr e1
    let (eResult2, listSug2) = evalLintIfRedOr e2
    let (eResult3, listSug3) = evalLintIfRedOr e3
    (Case eResult1 eResult2 (nom1, nom2, eResult3), listSug1 ++ listSug2 ++ listSug3)
  Infix op e1 e2 -> do
    let (eResult1, listSug1) = evalLintIfRedOr e1
    let (eResult2, listSug2) = evalLintIfRedOr e2
    (Infix op eResult1 eResult2, listSug1 ++ listSug2)
  If e1 e2 e3-> do
    let (eResult1, listSug1)= evalLintIfRedOr e1
    let (eResult2, listSug2)= evalLintIfRedOr e2
    let (eResult3, listSug3)= evalLintIfRedOr e3
    case eResult2 of
        Lit l2 -> if esTrue l2 then do
            let expSugg = LintRedIf (If eResult1 (Lit l2) eResult3) (Infix Or eResult1 eResult3)
            (Infix Or eResult1 eResult3, expSugg : listSug1 ++ listSug2 ++ listSug3)
          else (If eResult1 eResult2 eResult3, listSug1 ++ listSug2 ++ listSug3)
        otherwise -> (If eResult1 eResult2 eResult3, listSug1 ++ listSug2 ++ listSug3)

-- Sustitución de if por disyunción entre la condición y su rama _else_
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfOr :: Linting Expr
lintRedIfOr e = do
  let (result, listSug) =  evalLintIfRedOr e
  (result, listSug)

--------------------------------------------------------------------------------
-- Composición
--------------------------------------------------------------------------------

-- Función auxiliar que evalúa una Expr y elimina el neutro de la disyunción
evaluarComp :: Expr -> (Expr, [LintSugg])
evaluarComp e = case e of
  Var nom -> (Var nom, [])
  Lit lit -> (Lit lit, [])
  Lam nom e1 -> do
    let (result, listSug) = evaluarComp e1
    (Lam nom result, listSug)
  Case e1 e2 (nom1, nom2, e3) -> do
    let (eResult1, listSug1) = evaluarComp e1
    let (eResult2, listSug2) = evaluarComp e2
    let (eResult3, listSug3) = evaluarComp e3
    (Case eResult1 eResult2 (nom1, nom2, eResult3), listSug3 ++ listSug2 ++ listSug1)
  Infix op e1 e2 -> do
    let (eResult1, listSug1) = evaluarComp e1
    let (eResult2, listSug2) = evaluarComp e2
    (Infix op eResult1 eResult2, listSug1 ++ listSug2)
  If e1 e2 e3-> do
    let (eResult1, listSug1)= evaluarComp e1
    let (eResult2, listSug2)= evaluarComp e2
    let (eResult3, listSug3)= evaluarComp e3
    (If eResult1 eResult2 eResult3, listSug2 ++ listSug3 ++ listSug1)
  App e1 e2 -> do
    let (eResult1, listSug1) = evaluarComp e1
    let (eResult2, listSug2) = evaluarComp e2
    case eResult1 of
        Var nom -> case eResult2 of
          App e3 e4 -> do
              let expSugg = LintComp (App eResult1 eResult2) (App (Infix Comp eResult1 e3) e4)
              (App (Infix Comp eResult1 e3) e4, listSug1 ++ listSug2 ++ expSugg : [])
          otherwise -> (App eResult1 eResult2, listSug1 ++ listSug2)
        Lam nom e3 -> case eResult2 of
          App e3 e4 -> do
              let expSugg = LintComp (App eResult1 eResult2) (App (Infix Comp eResult1 e3) e4)
              (App (Infix Comp eResult1 e3) e4, listSug1 ++ listSug2 ++ expSugg : [])
          otherwise -> (App eResult1 eResult2, listSug1 ++ listSug2)
        Infix Comp e8 e9 -> case eResult2 of
          App e3 e4 -> do
              let expSugg = LintComp (App eResult1 eResult2) (App (Infix Comp eResult1 e3) e4)
              (App (Infix Comp eResult1 e3) e4, listSug1 ++ listSug2 ++ expSugg : [])
          otherwise -> (App eResult1 eResult2, listSug1 ++ listSug2)
        App e8 e9 -> case eResult2 of
          App e3 e4 -> do
              let expSugg = LintComp (App eResult1 eResult2) (App (Infix Comp eResult1 e3) e4)
              (App (Infix Comp eResult1 e3) e4, listSug1 ++ listSug2 ++ expSugg : [])
          otherwise -> (App eResult1 eResult2, listSug1 ++ listSug2)
        otherwise -> (App eResult1 eResult2, listSug1 ++ listSug2)

-- se aplica en casos de la forma (f g t), reemplazando por (f . g) t

lintComp :: Linting Expr
lintComp e = do
  let (result,listSug) = evaluarComp e
  (result, listSug)


--------------------------------------------------------------------------------
-- Eta Redución
--------------------------------------------------------------------------------

-- Función auxiliar que evalúa una Expr y elimina el neutro de la disyunción
evaluarEta :: Expr -> (Expr, [LintSugg])
evaluarEta e = case e of
  Var nom -> (Var nom, [])
  Lit lit -> (Lit lit, [])
  Case e1 e2 (nom1, nom2, e3) -> do
    let (eResult1, listSug1) = evaluarEta e1
    let (eResult2, listSug2) = evaluarEta e2
    let (eResult3, listSug3) = evaluarEta e3
    (Case eResult1 eResult2 (nom1, nom2, eResult3), listSug3 ++ listSug2 ++ listSug1)
  Infix op e1 e2 -> do
    let (eResult1, listSug1) = evaluarEta e1
    let (eResult2, listSug2) = evaluarEta e2
    (Infix op eResult1 eResult2, listSug2 ++ listSug1)
  If e1 e2 e3-> do
    let (eResult1, listSug1)= evaluarEta e1
    let (eResult2, listSug2)= evaluarEta e2
    let (eResult3, listSug3)= evaluarEta e3
    (If eResult1 eResult2 eResult3, listSug3 ++ listSug2 ++ listSug1)
  App e1 e2 -> do
    let (eResult1, listSug1) = evaluarEta e1
    let (eResult2, listSug2) = evaluarEta e2
    (App eResult1 eResult2, listSug2 ++ listSug1)
  Lam nom e1 -> do
    let (result, listSug) = evaluarEta e1
    case result of
      App e2 e3 -> do
        let freeVars = freeVariables e2
        if existeVarList nom freeVars then (Lam nom result, listSug)
        else case e3 of
          Var nom2 -> if (nom == nom2) then do
              let expSugg = LintEta (Lam nom result) (e2)
              (e2, listSug ++ expSugg : [])
            else (Lam nom result, listSug)
          otherwise -> (Lam nom result, listSug)
      otherwise -> (Lam nom result, listSug)

-- se aplica en casos de la forma \x -> f x, reemplazando por f
lintEta :: Linting Expr
lintEta e = do
  let (result, listSug) = evaluarEta e
  (result, listSug)


--------------------------------------------------------------------------------
-- Eliminación de recursión estructural
--------------------------------------------------------------------------------

existenLibreSoloEnExp :: [Name] -> Expr -> String -> String -> Bool
existenLibreSoloEnExp varLigList e nameFunc name = case e of
  Var nom -> if (nom == nameFunc || nom == name) && (not (existeVarList nom varLigList)) then False
    else True  
  Lit l -> True
  App e1 e2 -> do
    let noExisteLibre1 = existenLibreSoloEnExp varLigList e1 nameFunc name
    let noExisteLibre2 = existenLibreSoloEnExp varLigList e2 nameFunc name
    case e1 of
      Var nom -> if (nom == nameFunc) then case e2 of
          Var nom2 -> if nom2 == name then True
            else (existeVarList nom varLigList) && noExisteLibre1 && noExisteLibre2
          otherwise -> (existeVarList nom varLigList) && noExisteLibre1 && noExisteLibre2
        else case e2 of
          Var nom2 -> if nom2 == name then (existeVarList nom2 varLigList) && noExisteLibre1 && noExisteLibre2
            else True && noExisteLibre1 && noExisteLibre2
          otherwise -> True && noExisteLibre1 && noExisteLibre2
      otherwise -> True  && noExisteLibre1 && noExisteLibre2
  Lam nom e1 -> do
    let varLigList' = (nom : varLigList)
    existenLibreSoloEnExp varLigList' e1 nameFunc name
  If e1 e2 e3 -> do 
    let noExisteLibre1 = existenLibreSoloEnExp varLigList e1 nameFunc name
    let noExisteLibre2 = existenLibreSoloEnExp varLigList e2 nameFunc name
    let noExisteLibre3 = existenLibreSoloEnExp varLigList e3 nameFunc name
    noExisteLibre1 && noExisteLibre2 && noExisteLibre3
  Infix op e1 e2 -> do
    let noExisteLibre1 = existenLibreSoloEnExp varLigList e1 nameFunc name
    let noExisteLibre2 = existenLibreSoloEnExp varLigList e2 nameFunc name
    noExisteLibre1 && noExisteLibre2
  Case e1 e2 (nom1, nom2, e3) -> do 
    let noExisteLibre1 = existenLibreSoloEnExp varLigList e1 nameFunc name
    let noExisteLibre2 = existenLibreSoloEnExp varLigList e2 nameFunc name
    let noExisteLibre3 = existenLibreSoloEnExp varLigList e3 nameFunc name
    noExisteLibre1 && noExisteLibre2 && noExisteLibre3


buscarReemplazarExp :: String -> String -> String -> Expr -> Expr
buscarReemplazarExp nameFunc name nomNew e4 = case e4 of
  Var nom -> (Var nom)
  Lit lit -> (Lit lit)
  If e1 e2 e3 -> do
    let expNew1 = buscarReemplazarExp nameFunc name nomNew e1
    let expNew2 = buscarReemplazarExp nameFunc name nomNew e2
    let expNew3 = buscarReemplazarExp nameFunc name nomNew e3
    (If expNew1 expNew2 expNew3)
  Lam nom e1 -> do
    let expNew1 = buscarReemplazarExp nameFunc name nomNew e1
    (Lam nom expNew1)
  Infix op e1 e2 -> do
    let expNew1 = buscarReemplazarExp nameFunc name nomNew e1
    let expNew2 = buscarReemplazarExp nameFunc name nomNew e2
    Infix op expNew1 expNew2
  Case e1 e2 (nom1, nom2, e3) -> do
    let expNew1 = buscarReemplazarExp nameFunc name nomNew e1
    let expNew2 = buscarReemplazarExp nameFunc name nomNew e2
    let expNew3 = buscarReemplazarExp nameFunc name nomNew e3
    (Case expNew1 expNew2 (nom1, nom2, expNew3))
  App e1 e2 -> do
    let expNew1 = buscarReemplazarExp nameFunc name nomNew e1
    let expNew2 = buscarReemplazarExp nameFunc name nomNew e2
    case expNew1 of
      Var nom -> if nom == nameFunc then do
        case e2 of
          Var nom2 -> if nom2 == name then (Var nomNew)
            else (App expNew1 expNew2)
          otherwise -> (App expNew1 expNew2)
        else (App expNew1 expNew2)
      otherwise -> (App expNew1 expNew2)

buscarPrimeroNoLibre :: String -> String -> [Name] -> String
buscarPrimeroNoLibre nom2 num list4 = do
  let numFirst = nom2 ++ num
  if (existeVarList numFirst list4) then do
    let varNum = read num :: Int
    let sigNum = varNum + 1
    let textNum = show sigNum
    buscarPrimeroNoLibre nom2 textNum list4
  else numFirst

-- Función auxiliar que evalúa una Expr y elimina el neutro de la disyunción
evaluarFoldr :: FunDef -> (FunDef, [LintSugg])
evaluarFoldr e = do
  let nameFun = obtenerName e
  let expFun = obtenerExp e
  case expFun of
    Lam nom e1 -> do
      case e1 of
        Case e2 e3 (nom1, nom2, e4) -> case e2 of
          Var nom3 -> if (nom3 == nom) then do
              let freeVarCase = freeVariables e4
              let existeNameFree = existeVarList nameFun freeVarCase
              let existeNom2Free = existeVarList nom2 freeVarCase
              if existeNameFree && (not existeNom2Free) then (e, [])
              else if existeNom2Free && (not existeNameFree)then (e, [])
              else if existeNom2Free && existeNameFree then do
                let existeSoloRecursion = existenLibreSoloEnExp [] e4 nameFun nom2
                if existeSoloRecursion then do
                  let varNum = "0"
                  let toFind = nameFun ++ "_" ++ nom2
                  let nomNew = buscarPrimeroNoLibre toFind varNum freeVarCase
                  let expNew = buscarReemplazarExp nameFun nom2 nomNew e4
                  let exprSugg = LintFoldr e (FunDef nameFun (App (App (Var "foldr") (Lam nom1 (Lam nomNew (expNew)))) (e3)))
                  let funNew = (FunDef nameFun (App (App (Var "foldr") (Lam nom1 (Lam nomNew (expNew)))) (e3)))
                  (funNew, exprSugg : [])
                else (e, [])
              else do
                let varNum = "0"
                let toFind = nameFun ++ "_" ++ nom2
                let nomNew = buscarPrimeroNoLibre toFind varNum freeVarCase
                let exprSugg = LintFoldr e (FunDef nameFun (App (App (Var "foldr") (Lam nom1 (Lam nomNew e4))) (e3)))
                let funNew = (FunDef nameFun (App (App (Var "foldr") (Lam nom1 (Lam nomNew e4))) (e3)))
                (funNew, exprSugg : [])
            else (e, [])
        otherwise -> (e, [])
    otherwise -> (e, [])

-- Sustituye recursión estructural sobre listas por `foldr`
-- Construye sugerencias de la forma (LintFoldr f r)
lintFoldr :: Linting FunDef
lintFoldr func = do
  let (resul, listSug) = evaluarFoldr func
  (resul, listSug)
  
--------------------------------------------------------------------------------
-- Combinación de Lintings
--------------------------------------------------------------------------------

-- Función auxiliar que obtiene el nombre de la FunDef
obtenerName :: FunDef -> Name
obtenerName (FunDef x y) = x;

-- Función auxiliar que obtiene la expresión de la FunDef
obtenerExp :: FunDef -> Expr
obtenerExp (FunDef x y) = y;

-- Dada una transformación a nivel de expresión, se construye
-- una transformación a nivel de función
liftToFunc :: Linting Expr -> Linting FunDef
liftToFunc lintFuncDef f = do
  let (result, listSug) = lintFuncDef (obtenerExp f)
  let funDefResult = FunDef (obtenerName f) result
  (funDefResult, listSug)  

-- encadenar transformaciones:
(>==>) :: Linting a -> Linting a -> Linting a
lint1 >==> lint2 = \f -> do 
  let (funResult, listSug1) = lint1 f
  let (finalResult, listSug2) = lint2 funResult
  (finalResult, listSug1 ++ listSug2)

-- aplica las transformaciones 'lints' repetidas veces y de forma incremental,
-- hasta que ya no generen más cambios en 'func'
lintRec :: Linting a -> Linting a
lintRec lintFunDef func = do
  let (func2, listSug) = lintFunDef func
  if (length listSug == 0) then (func, [])
  else do 
    let (funcResult, listSug2) = lintRec lintFunDef func2
    (funcResult, listSug ++ listSug2)
