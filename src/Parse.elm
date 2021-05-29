module Parse exposing (..)

import Parser exposing (
    Parser, (|.), (|=), 
    succeed, number,  symbol, 
    float, spaces, getChompedString, 
    token, chompWhile, chompIf, oneOf,
    run, 
    lazy,
    keyword,andThen)
import Parser exposing (DeadEnd)
import List exposing (concat)
import List exposing (concatMap)
import Set

type Expr = NumC Float
    | StringC String
    | IdC String
    | IfC Expr Expr Expr
    | LamC (List String) Expr
    | AppC Expr (List Expr)

expr :  Parser Expr
expr = succeed identity
    |. spaces
    |= oneOf [
        Parser.backtrackable num,
        string,
        Parser.backtrackable idExpr,
        Parser.backtrackable ifExpr,
        Parser.backtrackable lambda,
        Parser.backtrackable local,
        app]
    |. spaces

parse: String -> Result (List DeadEnd) Expr
parse = run expr

isWhiteSpace: Char -> Bool
isWhiteSpace c = c == ' ' || c == '\r' || c == '\n' || c == '\t'
positiveNum: Parser Float
positiveNum = succeed identity
                |= number {
                int  = Just toFloat
                , hex = Nothing
                , float = Just identity
                , binary = Nothing
                , octal = Nothing} |> andThen (\n -> 
                ((getChompedString <| chompWhile (\c -> not (isWhiteSpace c) && c /= '}')) |> andThen (\s -> if String.length s > 0 then Parser.problem "invalid number" else succeed n)))

num: Parser Expr
num = succeed identity
    |. spaces
    |= oneOf
     [
         succeed (\v -> NumC (negate v))
         |. symbol "-"
         |= positiveNum,
         succeed NumC
         |= positiveNum]
    |. spaces
string : Parser Expr
string = succeed StringC
        |= rawString

rawString : Parser String
rawString = succeed identity
            |. token (String.fromChar '\"')
            |= (Parser.getChompedString <| chompWhile (\c -> c /= '\"'))
            |. chompIf (\c -> c == '\"')

ifExpr: Parser Expr
ifExpr = succeed IfC
        |. token "{"
        |. spaces
        |. keyword "if"
        |. spaces
        |= lazy (\_ -> expr)
        |. spaces
        |= lazy (\_ -> expr)
        |. spaces
        |= lazy (\_ -> expr)
        |. spaces
        |. token "}"

allowedChars: Set.Set Char
allowedChars = Set.fromList ['#', '$', '<', '>', '=', '!', '@', '$', '%', '^', '&', '*', '-', '_', '~', '+', '-', '*', '/', '?', '&']

id: Parser String
id = succeed identity
    |= Parser.variable {
        start = \c -> Char.isAlpha c || Set.member c allowedChars,
        inner = \c -> Char.isAlphaNum c || Set.member c allowedChars,
        reserved = Set.fromList ["if", "local", "in", "=>"]
    }

idExpr: Parser Expr
idExpr = succeed IdC
         |= id

idList: Parser (List String)
idList = Parser.sequence {
    start = "("
    , separator = ""
    , end = ")"
    , spaces = spaces
    , item = id
    , trailing = Parser.Forbidden
    }

lambda: Parser Expr
lambda = succeed LamC
        |. token "{"
        |. spaces
        |= idList
        |. spaces
        |. keyword "=>"
        |. spaces
        |= lazy (\_ -> expr)
        |. spaces
        |. token "}"

app: Parser Expr
app = Parser.sequence {
          start = "{",
          separator = "",
          end = "}",
          spaces = spaces,
          item = lazy (\_ -> expr),
          trailing = Parser.Optional
      } |> Parser.andThen (\exprs -> case exprs of 
                                x :: xs -> succeed (AppC x xs)
                                [] -> Parser.problem "bad function application")

type Binding = Binding String Expr
binding: Parser (String, Expr)
binding = (succeed Binding
          |. token "{"
          |. spaces
          |= id
          |. spaces
          |= lazy (\_ -> expr)
          |. spaces
          |. token "}") |> andThen (\b -> case b of 
                                        Binding name val -> succeed (name, val))

bindingPairs: Parser (List String, List Expr)
bindingPairs = Parser.sequence {
    start = "",
    separator = "",
    end = "",
    spaces = spaces,
    item = binding,
    trailing = Parser.Optional
    } |> andThen (\bindings -> succeed (List.unzip bindings))

type LocalS = LocalS (List String, List Expr) Expr
local: Parser Expr
local = succeed LocalS
        |. token "{"
        |. spaces
        |. keyword "local"
        |. spaces
        |= bindingPairs
        |. spaces 
        |. keyword "in"
        |. spaces
        |= lazy (\_ -> expr)
        |. token "}" |> andThen (\localS -> 
        case localS of
            LocalS (names, vals) body -> succeed (AppC (LamC names body) vals))

exprToString: Expr -> String
exprToString exp = case exp of
    IdC x -> "IdC (" ++ x ++ ")"
    StringC s -> "StringC (\"" ++ s ++ String.fromChar('\"') ++ ")"
    NumC n -> "NumC (" ++ String.fromFloat(n) ++ ")"
    IfC test thn els -> "IfC (" ++ exprToString test ++ " " ++ exprToString thn ++ " " ++ exprToString els ++ ")"
    LamC args body -> "LamC (" ++ String.join " " args ++ " " ++ exprToString body ++ ")"
    AppC fn args -> "AppC (" ++ " " ++ exprToString fn ++ " " ++ String.join " " (List.map exprToString args) ++ ")"