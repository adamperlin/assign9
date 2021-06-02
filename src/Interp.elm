module Interp exposing (..)
import Parse exposing (..)
import Dict exposing (..)

{- 
values:
NumV
StringV
BoolV
CloV
PrimopV
NullV
-}

type alias Env = Dict String Value

type Value = NumV Float 
    | StringV String
    | BoolV Bool
    | CloV (List String) Expr Env
    | PrimopV String

type InterpError = InterpError String
show: InterpError -> String
show (InterpError err) = "InterpError: " ++ err

lookup: String -> Env -> Result InterpError Value
lookup name env = case Dict.get name env of
                    Just val -> Ok val
                    Nothing -> Err (InterpError ("unbound identifier " ++ name))

returnResult: Value -> Result InterpError Value
returnResult val = Ok val

bindResult: Result e a -> (a -> Result e b) -> Result e b
bindResult r1 fn = case r1 of
    Ok v -> fn v
    Err e -> Err e

mapResult: List Expr -> (Expr -> Result InterpError Value) -> Result InterpError (List Value)
mapResult exprs evalFn = case exprs of
                            [] -> Ok []
                            expr :: remainExprs -> bindResult (evalFn expr)
                                (\curVal -> bindResult (mapResult remainExprs evalFn)
                                    (\nextVals -> 
                                        Ok (curVal :: nextVals)))

extendEnv: Env -> (List (String, Value)) -> Env
extendEnv env newBindings = Dict.union (Dict.fromList newBindings) env
interp: Expr -> Env -> Result InterpError Value
interp expr env = 
    case expr of
        StringC s -> Ok (StringV s)
        IdC id -> lookup id env
        NumC n -> Ok (NumV n)
        LamC args body ->
            Ok (CloV args body env)
        IfC cond thn els -> bindResult
            (interp cond env)
            (\condVal -> case condVal of
                            BoolV True -> interp thn env 
                            BoolV False -> interp els env
                            _ -> Err (InterpError "invalid type for condition cond"))
        AppC fnExpr fnArgs -> bindResult
                (interp fnExpr env)
                (\fnVal -> case fnVal of 
                            CloV cloArgs cloBody cloEnv -> bindResult 
                                        (mapResult fnArgs (\e -> interp e env))
                                            (\evaledArgs -> 
                                                let newEnv = extendEnv cloEnv (List.map2 Tuple.pair cloArgs evaledArgs) in
                                                    interp cloBody newEnv)
                            PrimopV op -> Ok (NumV 0)
                            _ -> Err (InterpError "Invalid function call"))

serialize: Value -> String
serialize val =
    case val of
       StringV str -> str
       NumV num -> String.fromFloat num
       BoolV b -> if b then "true" else "false"
       CloV _ _ _ -> "#<procedure>"
       PrimopV _ -> "#<primop>"


topInterp: String -> Result InterpError String
topInterp expr =
    case parse expr of
       Ok ast -> bindResult (interp ast Dict.empty) (serialize >> Ok)
       Err _ -> Err (InterpError "Parse Error")
