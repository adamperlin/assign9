module Interp exposing (..)
import Parse exposing (..)
import Dict exposing (..)
import Result exposing (andThen)

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

mapResult: List Expr -> (Expr -> Result InterpError Value) -> Result InterpError (List Value)
mapResult exprs evalFn = case exprs of
                            [] -> Ok []
                            expr :: remainExprs -> (evalFn expr) |> andThen
                                (\curVal -> (mapResult remainExprs evalFn) |> andThen
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
        IfC cond thn els -> 
            (interp cond env) |> andThen
            (\condVal -> case condVal of
                            BoolV True -> interp thn env 
                            BoolV False -> interp els env
                            _ -> Err (InterpError "invalid type for condition cond"))
        AppC fnExpr fnArgs -> (interp fnExpr env) |> andThen
                (\fnVal -> case fnVal of 
                            CloV cloArgs cloBody cloEnv ->  (mapResult fnArgs (\e -> interp e env)) |> andThen
                                            (\evaledArgs -> 
                                                let newEnv = extendEnv cloEnv (List.map2 Tuple.pair cloArgs evaledArgs) in
                                                    interp cloBody newEnv)
                            PrimopV op -> (mapResult fnArgs (\e -> interp e env)) |> andThen (interpPrimop op)
                            _ -> Err (InterpError "Invalid function call"))

serialize: Value -> String
serialize val =
    case val of
       StringV str -> str
       NumV num -> String.fromFloat num
       BoolV b -> if b then "true" else "false"
       CloV _ _ _ -> "#<procedure>"
       PrimopV _ -> "#<primop>"

topEnv: Env 
topEnv = Dict.fromList [
    ("+", PrimopV "+"),
    ("-", PrimopV "-"),
    ("*", PrimopV "*"),
    ("/", PrimopV "/"),
    ("<=", PrimopV "<="),
    ("equal?", PrimopV "equal?"),
    ("true", BoolV True),
    ("false", BoolV False),
    ("error", PrimopV "error")]

topInterp: String -> Result InterpError String
topInterp expr =
    case parse expr of
       Ok ast -> (interp ast topEnv) |> andThen (serialize >> Ok)
       Err _ -> Err (InterpError "Parse Error")

-- Primop Code
type alias PrimitiveFunc = (List Value) -> Result InterpError Value

interpPrimop: String -> (List Value) -> Result InterpError Value
interpPrimop op args = case Dict.get op builtInFuncs of
   Just primFunc -> primFunc args
   Nothing -> Err (InterpError ("Invalid primop " ++ op))

builtInFuncs: Dict String PrimitiveFunc
builtInFuncs = Dict.fromList [
  ("+", primAdd),
  ("-", primSub),
  ("*", primMult),
  ("/", primDiv),
  ("<=", primLeq),
  ("equal?", primEq),
  ("error", primError)]

typecheckBinaryNumPrimop: (List Value) -> (Float -> Float -> Float) -> Result InterpError Value
typecheckBinaryNumPrimop vals op = 
  case vals of
    v1 :: v2 :: [] -> 
      case (v1, v2) of
        (NumV n1, NumV n2) -> Ok (NumV (op n1 n2))
        _ -> Err (InterpError ("Invalid Arg Type:" ++ (serialize v1) ++ " " ++ (serialize v2)))
    _ -> Err (InterpError "Wrong number of args")

primAdd: PrimitiveFunc
primAdd args = typecheckBinaryNumPrimop args (+)

primSub: PrimitiveFunc
primSub args = typecheckBinaryNumPrimop args (-)

primMult: PrimitiveFunc
primMult args = typecheckBinaryNumPrimop args (*)

primDiv: PrimitiveFunc
primDiv args = case typecheckBinaryNumPrimop args (/) of
   Ok (NumV val) -> if (isInfinite val) then Err (InterpError "Division by Zero") else (Ok (NumV val))
   Ok (_) -> Err (InterpError "Wrong return type")
   Err error -> Err error

primLeq: PrimitiveFunc
primLeq args = case typecheckBinaryNumPrimop args (\a b -> if (a <= b) then 1 else 0) of
   Ok (NumV val) -> if (val == 0) then Ok (BoolV False) else Ok (BoolV True)
   Ok (_) -> Err (InterpError "Wrong return type")
   Err error -> Err error

primEq: PrimitiveFunc
primEq args = case args of
   v1 :: v2 :: [] -> Ok (BoolV ((v1 == v2) && (canBeEqual v1) && (canBeEqual v2)))
   _ -> Err (InterpError "Wrong number of args for ==")

-- Closures and Primops cannot be equal
canBeEqual: Value -> Bool
canBeEqual val = case val of
    PrimopV _ -> False
    CloV _ _ _ -> False
    _ -> True

primError: PrimitiveFunc
primError args = case args of
   v1 :: [] -> Err (InterpError ("User Error" ++ (serialize v1)))
   _  -> Err (InterpError "Wrong number of args for equal?")
