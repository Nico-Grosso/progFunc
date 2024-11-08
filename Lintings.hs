module Lintings where

import AST
import LintTypes


--------------------------------------------------------------------------------
-- AUXILIARES
--------------------------------------------------------------------------------

-- Computa la lista de variables libres de una expresión
freeVariables :: Expr -> [Name]
freeVariables = undefined

--------------------------------------------------------------------------------
-- LINTINGS
--------------------------------------------------------------------------------

-- parseo del case
-- case e1 of
--     [] -> e2
--     (n1 : n2) -> e3

-- Arbol
-- Case e1 e2 (n1, n2, e3)

-- En MiniHaskell el case est´a restringido al pattern matching sobre la lista vacia []
-- y la lista no vac´ıa de la forma (〈ident〉:〈ident〉)


--------------------------------------------------------------------------------
-- Computación de constantes
--------------------------------------------------------------------------------

evalBool :: Lit -> Bool
evalBool (LitBool x) = x

evalInt :: Lit -> Integer
evalInt (LitInt lit) = lit

evalOp :: Op -> Lit -> Lit -> (Lit, Bool)
evalOp Add lit1 lit2 =
  let res1 = evalInt lit1
      res2 = evalInt lit2
      result = res1 + res2
  in (LitInt result, True)
evalOp Sub lit1 lit2 = 
  let res1 = evalInt lit1
      res2 = evalInt lit2
      result = res1 - res2
  in if (res1 >= res2) then (LitInt result, True) else (LitNil, False)
evalOp Mult lit1 lit2 =
  let res1 = evalInt lit1
      res2 = evalInt lit2
      result = res1*res2
  in (LitInt result, True)
evalOp Div lit1 lit2 =
  let res1 = evalInt lit1
      res2 = evalInt lit2
  in if (res2 == 0) then (LitNil, False) else (LitInt (div res1 res2), True)
evalOp And lit1 lit2 =
  let res1 = evalBool lit1
      res2 = evalBool lit2
      res = res1 && res2
  in (LitBool res, True)
evalOp Or lit1 lit2 =
  let res1 = evalBool lit1
      res2 = evalBool lit2
      res = res1 || res2
  in (LitBool res, True)

evalConstant :: Expr -> (Expr, [LintSugg])
evalConstant expr = case expr of
  Lit lit -> (Lit lit, [])
  Var variable -> (Var variable, [])
  Case expr1 expr2 (x, xs, expr3) ->
    let (result1, sugg1) = evalConstant expr1
        (result2, sugg2) = evalConstant expr2
        (result3, sugg3) = evalConstant expr3
    in (Case result1 result2 (x, xs, result3), sugg1 ++ sugg2 ++ sugg3)
  App expr1 expr2 ->
    let (result1, sugg1) = evalConstant expr1
        (result2, sugg2) = evalConstant expr2
    in (App result1 result2, sugg1 ++ sugg2)
  Lam nom expr ->
    let (result, sugg) = evalConstant expr
    in (Lam nom result, sugg)
  If cond expr1 expr2 ->
    let (result1, sugg1) = evalConstant cond
        (result2, sugg2) = evalConstant expr1
        (result3, sugg3) = evalConstant expr2
    in (If result1 result2 result3, sugg2 ++ sugg3 ++ sugg1)
  Infix op expr1 expr2 ->
    let (result1, sugg1) = evalConstant expr1
        (result2, sugg2) = evalConstant expr2
    in case result1 of 
      Lit lit1 -> case result2 of
        Lit lit2 -> 
          let (res, changed) = evalOp op lit1 lit2
              exprSugg = LintCompCst (Infix op (Lit lit1) (Lit lit2)) (Lit res)
          in case changed of
            True -> ((Lit res), sugg1 ++ sugg2 ++ exprSugg:[])
            otherwise -> (Infix op (Lit lit1) (Lit lit2), sugg1 ++ sugg2)
        otherwise -> (Infix op result1 result2, sugg1 ++ sugg2)
      otherwise -> (Infix op result1 result2, sugg1 ++ sugg2)


--------------------------------------------------------------------------------
-- Reduce expresiones aritméticas/booleanas
-- Construye sugerencias de la forma (LintCompCst e r)
lintComputeConstant :: Linting Expr
lintComputeConstant expr = evalConstant expr

--------------------------------------------------------------------------------
-- Eliminación de chequeos redundantes de booleanos
--------------------------------------------------------------------------------

-- Función auxiliar que evalúa una Lit y se fija si es un booleano True
isTrue :: Lit -> Bool
isTrue(LitBool i) = (i==True)
isTrue (_) = False

-- Función auxiliar que evalúa una Lit y se fija si es un booleano False
isFalse :: Lit -> Bool
isFalse(LitBool i) = (i==False)
isFalse (_) = False

-- Función auxiliar que evalúa una Expr y elimina chequeos redundantes de booleanos
evalChequeo :: Expr -> (Expr, [LintSugg])
evalChequeo e = case e of
  Var nom -> (Var nom, [])
  Lit lit -> (Lit lit, [])
  App e1 e2 -> do
    let (eResult1, listSug1) = evalChequeo e1
    let (eResult2, listSug2) = evalChequeo e2
    (App eResult1 eResult2, listSug1 ++ listSug2)
  Lam nom e1 -> do
    let (result, listSug) = evalChequeo e1
    (Lam nom result, listSug)
  Case e1 e2 (nom1, nom2, e3) -> do
    let (eResult1, listSug1) = evalChequeo e1
    let (eResult2, listSug2) = evalChequeo e2
    let (eResult3, listSug3) = evalChequeo e3
    (Case eResult1 eResult2 (nom1, nom2, eResult3), listSug1 ++ listSug2 ++ listSug3)
  If e1 e2 e3 -> do
    let (eResult1, listSug1) = evalChequeo e1
    let (eResult2, listSug2) = evalChequeo e2
    let (eResult3, listSug3) = evalChequeo e3
    (If eResult1 eResult2 eResult3, listSug1 ++ listSug2 ++ listSug3)
  Infix op e1 e2 -> do
    let (eResult1, listSug1)= evalChequeo e1
    let (eResult2, listSug2)= evalChequeo e2
    case op of
      Eq -> case eResult1 of
        Lit l1 -> if isTrue l1 then do
            let expSugg = LintBool (Infix op (Lit l1) eResult2) (eResult2)
            (eResult2, expSugg : listSug1 ++ listSug2)
          else if isFalse l1 then do
            let expSugg = LintBool (Infix op (Lit l1) eResult2) (App (Var "not") eResult2)
            (App (Var "not") eResult2, expSugg : listSug1 ++ listSug2)
          else (Infix op eResult1 eResult2, listSug1 ++ listSug2)
        otherwise -> case eResult2 of
              Lit l2 -> if isTrue l2 then do
                  let expSugg = LintBool (Infix op (eResult1) (Lit l2)) (eResult1)  
                  (eResult1, expSugg : listSug1 ++ listSug2)
                else if isFalse l2 then do
                  let expSugg = LintBool (Infix op (eResult1) (Lit l2)) (App (Var "not") eResult1)
                  (App (Var "not") eResult1, expSugg : listSug1 ++ listSug2)
                else (Infix op eResult1 eResult2, listSug1 ++ listSug2)
              otherwise -> (Infix op eResult1 eResult2, listSug1 ++ listSug2)
      otherwise -> (Infix op eResult1 eResult2, listSug1 ++ listSug2)

--------------------------------------------------------------------------------
-- Elimina chequeos de la forma e == True, True == e, e == False y False == e
-- Construye sugerencias de la forma (LintBool e r)
lintRedBool :: Linting Expr
lintRedBool expr = evalChequeo expr


--------------------------------------------------------------------------------
-- Eliminación de if redundantes
--------------------------------------------------------------------------------

evalIfCond :: Expr -> (Expr, [LintSugg])
evalIfCond expr = case expr of
  Var n -> (Var n, [])
  Lit lit -> (Lit lit, [])
  Lam nom expr2 ->
    let (result2, sugg2) = evalIfCond expr2
    in (Lam nom result2, sugg2)
  App expr1 expr2 ->
    let (result1, sugg1) = evalIfCond expr1
        (result2, sugg2) = evalIfCond expr2
    in (App result1 result2, sugg1 ++ sugg2)
  Case expr1 expr2 (nom1, nom2, expr3) ->
    let (result1, sugg1) = evalIfCond expr1
        (result2, sugg2) = evalIfCond expr2
        (result3, sugg3) = evalIfCond expr3
    in (Case result1 result2 (nom1, nom2, result3), sugg1 ++ sugg2 ++ sugg3)
  Infix op expr1 expr2 ->
    let (result1, sugg1) = evalIfCond expr1
        (result2, sugg2) = evalIfCond expr2
    in (Infix op result1 result2, sugg1 ++ sugg2)
  If cond expr1 expr2 ->
    let (result1, sugg1) = evalIfCond expr1
        (result2, sugg2) = evalIfCond expr2
        (resultCond, suggCond) = evalIfCond cond
    in case resultCond of
      Lit lit -> if isTrue lit then
          let exprSugg = LintRedIf (If (Lit lit) result1 result2) (result1)
          in (result1, suggCond ++ sugg1 ++ sugg2 ++ exprSugg : [])
        else if isFalse lit then
          let exprSugg = LintRedIf (If cond result1 result2) (result2)
          in (result2, suggCond ++ sugg1 ++ sugg2 ++ exprSugg : [])
        else
          (If resultCond result1 result2, suggCond ++ sugg1 ++ sugg2)
      otherwise -> (If resultCond result1 result2, suggCond ++ sugg1 ++ sugg2)


--------------------------------------------------------------------------------
-- Sustitución de if con literal en la condición por la rama correspondiente
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfCond :: Linting Expr
lintRedIfCond expr = evalIfCond expr


evalIfAnd :: Expr -> (Expr, [LintSugg])
evalIfAnd expr = case expr of
  Var n -> (Var n, [])
  Lit lit -> (Lit lit, [])
  Lam nom expr2 ->
    let (result2, sugg2) = evalIfAnd expr2
    in (Lam nom result2, sugg2)
  App expr1 expr2 ->
    let (result1, sugg1) = evalIfAnd expr1
        (result2, sugg2) = evalIfAnd expr2
    in (App result1 result2, sugg1 ++ sugg2)
  Case expr1 expr2 (nom1, nom2, expr3) ->
    let (result1, sugg1) = evalIfAnd expr1
        (result2, sugg2) = evalIfAnd expr2
        (result3, sugg3) = evalIfAnd expr3
    in (Case result1 result2 (nom1, nom2, result3), sugg1 ++ sugg2 ++ sugg3)
  Infix op expr1 expr2 ->
    let (result1, sugg1) = evalIfAnd expr1
        (result2, sugg2) = evalIfAnd expr2
    in (Infix op result1 result2, sugg1 ++ sugg2)
  If cond expr1 expr2 ->
    let (result1, sugg1) = evalIfAnd expr1
        (result2, sugg2) = evalIfAnd expr2
        (resultCond, suggCond) = evalIfAnd cond
    in case result2 of
      Lit lit -> if isFalse lit then
          let res = Infix And resultCond result1
              exprSugg = LintRedIf (If resultCond result1 (Lit lit)) (res)
          in (res, suggCond ++ sugg1 ++ sugg2 ++ exprSugg : [])
        else
          (If resultCond result1 result2, suggCond ++ sugg1 ++ sugg2)
      otherwise -> (If resultCond result1 result2, suggCond ++ sugg1 ++ sugg2)

--------------------------------------------------------------------------------
-- Sustitución de if por conjunción entre la condición y su rama _then_
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfAnd :: Linting Expr
lintRedIfAnd expr = evalIfAnd expr 


evalIfOr :: Expr -> (Expr, [LintSugg])
evalIfOr expr = case expr of
  Var n -> (Var n, [])
  Lit lit -> (Lit lit, [])
  Lam nom expr2 ->
    let (result2, sugg2) = evalIfOr expr2
    in (Lam nom result2, sugg2)
  App expr1 expr2 ->
    let (result1, sugg1) = evalIfOr expr1
        (result2, sugg2) = evalIfOr expr2
    in (App result1 result2, sugg1 ++ sugg2)
  Case expr1 expr2 (nom1, nom2, expr3) ->
    let (result1, sugg1) = evalIfOr expr1
        (result2, sugg2) = evalIfOr expr2
        (result3, sugg3) = evalIfOr expr3
    in (Case result1 result2 (nom1, nom2, result3), sugg1 ++ sugg2 ++ sugg3)
  Infix op expr1 expr2 ->
    let (result1, sugg1) = evalIfOr expr1
        (result2, sugg2) = evalIfOr expr2
    in (Infix op result1 result2, sugg1 ++ sugg2)
  If cond expr1 expr2 ->
    let (result1, sugg1) = evalIfOr expr1
        (result2, sugg2) = evalIfOr expr2
        (resultCond, suggCond) = evalIfOr cond
    in case result1 of
      Lit lit -> if isTrue lit then
          let res = Infix Or resultCond result2
              exprSugg = LintRedIf (If resultCond (Lit lit) result2) (res)
          in (res, suggCond ++ sugg1 ++ sugg2 ++ exprSugg : [])
        else
          (If resultCond result1 result2, suggCond ++ sugg1 ++ sugg2)
      otherwise -> (If resultCond result1 result2, suggCond ++ sugg1 ++ sugg2)


--------------------------------------------------------------------------------
-- Sustitución de if por disyunción entre la condición y su rama _else_
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfOr :: Linting Expr
lintRedIfOr expr = evalIfOr expr 

--------------------------------------------------------------------------------
-- Chequeo de lista vacía
--------------------------------------------------------------------------------

-- Función auxiliar que evalúa una Lit y se fija si es una lista vacía
isEmptyList :: Lit -> Bool
isEmptyList(LitNil) = True
isEmptyList (_) = False

--------------------------------------------------------------------------------
-- Sugiere el uso de null para verificar si una lista es vacía
-- Construye sugerencias de la forma (LintNull e r)

lintNull :: Linting Expr
lintNull = undefined

--------------------------------------------------------------------------------
-- Eliminación de la concatenación
--------------------------------------------------------------------------------
-- se aplica en casos de la forma (e:[] ++ es), reemplazando por (e:es)
-- Construye sugerencias de la forma (LintAppend e r)

evalAppend :: Expr -> (Expr, [LintSugg])
evalAppend expr = case expr of
  Var n -> (Var n, [])
  Lit lit -> (Lit lit, [])
  App expr1 expr2 ->
    let (result1, sugg1) = evalAppend expr1
        (result2, sugg2) = evalAppend expr2
    in (App result1 result2, sugg1 ++ sugg2)
  Lam nom expr1 ->
    let (result1, sugg1) = evalAppend expr1
    in (Lam nom result1, sugg1)
  Case expr1 expr2 (nom1, nom2, expr3) ->
    let (result1, sugg1) = evalAppend expr1
        (result2, sugg2) = evalAppend expr2
        (result3, sugg3) = evalAppend expr3
    in (Case result1 result2 (nom1, nom2, result3), sugg1 ++ sugg2 ++ sugg3)
  If cond expr1 expr2 ->
    let (resultCond, suggCond) = evalAppend cond
        (result1, sugg1) = evalAppend expr1
        (result2, sugg2) = evalAppend expr2
    in (If resultCond result1 result2, suggCond ++ sugg1 ++ sugg2)
  Infix op expr1 expr2 ->
    let (result1, sugg1) = evalAppend expr1
        (result2, sugg2) = evalAppend expr2
    in case op of
      Append -> case result1 of
        Infix Cons subExpr1 subExpr2 ->
          case subExpr2 of
            Lit lit2 ->
              if isEmptyList lit2 then
                let exprSugg = LintAppend (Infix op result1 result2) (Infix Cons subExpr1 result2)
                in (Infix Cons subExpr1 result2, sugg1 ++ sugg2 ++ exprSugg : [])
              else
                (Infix op result1 result2, sugg1 ++ sugg2)
            otherwise -> (Infix op result1 result2, sugg1 ++ sugg2)
        otherwise ->
          (Infix op result1 result2, sugg1 ++ sugg2)
      otherwise -> 
        (Infix op result1 result2, sugg1 ++ sugg2)

lintAppend :: Linting Expr
lintAppend expr = evalAppend expr

--------------------------------------------------------------------------------
-- Composición
--------------------------------------------------------------------------------
-- se aplica en casos de la forma (f (g t)), reemplazando por (f . g) t
-- Construye sugerencias de la forma (LintComp e r) 

evalComp :: Expr -> (Expr, [LintSugg])
evalComp expr = case expr of
  Var n -> (Var n, [])
  Lit lit -> (Lit lit, [])
  Lam n expr1 ->
    let (result1, sugg1) = evalComp expr1
    in (Lam n result1, sugg1)
  Case expr1 expr2 (nom1, nom2, expr3) ->
    let (result1, sugg1) = evalComp expr1
        (result2, sugg2) = evalComp expr2
        (result3, sugg3) = evalComp expr3
    in (Case result1 result2 (nom1, nom2, result3), sugg1 ++ sugg2 ++ sugg3)
  If cond expr1 expr2 ->
    let (resultCond, suggCond) = evalComp cond
        (result1, sugg1) = evalComp expr1
        (result2, sugg2) = evalComp expr2
    in (If resultCond result1 result2, suggCond ++ sugg1 ++ sugg2)
  Infix op expr1 expr2 ->
    let (result1, sugg1) = evalComp expr1
        (result2, sugg2) = evalComp expr2
    in (Infix op result1 result2, sugg1 ++ sugg2)
  App expr1 expr2 ->
    let (result1, sugg1) = evalComp expr1
        (result2, sugg2) = evalComp expr2
    in case result1 of 
      Var nom -> case result2 of
        App subExpr1 subExpr2 -> 
          let result = App (Infix Comp result1 subExpr1) subExpr2
              exprSugg = LintComp (App result1 result2) (result)
          in (result, sugg1 ++ sugg2 ++ exprSugg : [])
        otherwise -> (App result1 result2, sugg1 ++ sugg2)
      Lam nom exprLam -> case result2 of
        App subExpr1 subExpr2 ->
          let result = App (Infix Comp result1 subExpr1) (subExpr2)
              exprSugg = LintComp (App result1 result2) (result)
          in (result, sugg1 ++ sugg2 ++ exprSugg : [])
        otherwise -> (App result1 result2, sugg1 ++ sugg2)
      Infix Comp exprInf1 exprInf2 -> case result2 of 
        App subExpr1 subExpr2 ->
          let result = App (Infix Comp result1 subExpr1) (subExpr2)
              exprSugg = LintComp (App result1 result2) (result)
          in (result, sugg1 ++ sugg2 ++ exprSugg : [])
        otherwise -> (App result1 result2, sugg1 ++ sugg2) 
      App exprApp1 exprApp2 -> case result2 of
        App subExpr1 subExpr2 ->
          let result = App (Infix Comp result1 subExpr1) (subExpr2)
              exprSugg = LintComp (App result1 result2) (result)
          in (result, sugg1 ++ sugg2 ++ exprSugg : [])
        otherwise -> (App result1 result2, sugg1 ++ sugg2) 
      otherwise -> (App result1 result2,  sugg1 ++ sugg2)

-- (f(g t)) -> App (Var "f") (App (Var "g") t)

lintComp :: Linting Expr
lintComp expr = evalComp expr


--------------------------------------------------------------------------------
-- Eta Redución
--------------------------------------------------------------------------------
-- se aplica en casos de la forma \x -> e x, reemplazando por e
-- Construye sugerencias de la forma (LintEta e r)

lintEta :: Linting Expr
lintEta = undefined


--------------------------------------------------------------------------------
-- Eliminación de recursión con map
--------------------------------------------------------------------------------

evalMap :: Expr -> (Expr, [LintSugg])
evalMap expr = case expr of
  Var nom -> (Var nom, [])
  Lit lit -> (Lit lit, [])
  Infix op expr1 expr1 ->
    let (result1, sugg1) = evalMap expr1
        (result2, sugg2) = evalMap expr2
    in (Infix op result1 result2, sugg1 ++ sugg2)
  App expr1 expr2 ->
    let (result1, sugg1) = evalMap expr1
        (result2, sugg2) = evalMap expr2
    in (App result1 result2, sugg1 ++ sugg2)
  Case expr1 expr2 (nom1, nom2, expr3) ->
    let (result1, sugg1) = evalMap expr1
        (result2, sugg2) = evalMap expr2
        (result3, sugg3) = evalMap expr3
    in (Case result1 result2 (nom1, nom2, result3), sugg1 ++ sugg2 ++ sugg3)
  If cond expr1 expr2 ->
    let (result1, sugg1) = evalMap expr1
        (result2, sugg2) = evalMap expr2
        (resultCond, suggCond) = evalMap cond
    in (If resultCond result1 result2, suggCond ++ sugg1 ++ sugg2)
  Lam nom exprLam ->
    let (resultLam, suggLam) = evalMap exprLam
    in case resultLam of 
      Case expr1 expr2 (nom1, nom2, expr3) -> case expr3 of
        Infix op expr1 expr2 -> case op of
          Cons ->
        otherwise -> (Lam nom resultLam, suggLam)
      otherwise -> (Lam nom resultLam, suggLam)

-- parseo del case
-- case e1 of
--     [] -> e2
--     (n1 : n2) -> e3

-- Arbol
-- Case e1 e2 (n1, n2, e3)


-- Sustituye recursión sobre listas por `map`
-- Construye sugerencias de la forma (LintMap f r)
lintMap :: Linting FunDef
lintMap expr = evalMap expr


--------------------------------------------------------------------------------
-- Combinación de Lintings
--------------------------------------------------------------------------------

getExpr :: FunDef -> Expr
getExpr (FunDef n e) = e

getName :: FunDef -> Name 
getName (FunDef n e) = n

-- Dada una transformación a nivel de expresión, se construye
-- una transformación a nivel de función
liftToFunc :: Linting Expr -> Linting FunDef
liftToFunc lintExpr func = 
    let (resultExpr, suggExpr) = lintExpr (getExpr func)
        funcResult = FunDef (getName func) resultExpr
        in (funcResult, suggExpr)

-- encadenar transformaciones:
(>==>) :: Linting a -> Linting a -> Linting a
lint1 >==> lint2 = \expr ->
    let (result1, sugg1) = lint1 expr
        (resultFinal, sugg2) = lint2 result1
    in (resultFinal, sugg1 ++ sugg2)

-- aplica las transformaciones 'lints' repetidas veces y de forma incremental,
-- hasta que ya no generen más cambios en 'func'
lintRec :: Linting a -> Linting a
lintRec lints func = 
    let (resultLint, listSugg) = lints func
        in if (null listSugg) then (resultLint, listSugg)
            else 
                let (resultLintNuevo, listSugg2) = lintRec lints resultLint
                in (resultLintNuevo, listSugg ++ listSugg2)
