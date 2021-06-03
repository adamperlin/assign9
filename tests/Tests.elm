module Tests exposing (interpTests, parserTests, topInterpTests)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Parse exposing (..)
import Parser exposing (DeadEnd)
import Interp exposing (..) 
import Dict exposing (..)
import Debug

bigTest : String
bigTest = """
{local
  {countToZeroPos {(countToZeroPos x) =>
                                       {if {<= x 0}
                                               {if {equal? x 0}
                                                       0
                                                       -1}
                                               {+ 1 {countToZeroPos countToZeroPos {- x 1}}}} } }

   {countToZeroNeg {(countToZeroNeg x) =>
                                      {if {<= x 0}
                                              {if {equal? 0 x}
                                                      0
                                                      {+ 1 {countToZeroNeg countToZeroNeg {+ x 1}}}}
                                              0}}}

  in {local {floor {(x) =>
                        {if {<= x 0}
                            {* -1 {countToZeroNeg countToZeroNeg x}}
                            {countToZeroPos countToZeroPos x}}}}
       in
       {local {ceil {(x) =>
                         {local {floor-x {floor x}} in
                           {if {<= x floor-x}
                               x
                               {+ 1 floor-x}}}}}
         in
         {{(x) =>
               {if {<= {- x {/ {+ {floor x} {ceil x}} 2}} 0}
                   {floor x}
                   {ceil x}}} 2.51}}}}
"""

parserTests : Test
parserTests = describe "tests for parser module"
        [describe "string parsing" [
            test "basic strings parse correctly" <|
                \_ -> let got = parse "\"abcdef\""
                          want = Ok (StringC "abcdef") in
                    Expect.equal got want,
            test "unmatched quotes parse correctly" <|
            \_ -> let got1 = parse "\"blah1234"
                      want1 = Err [DeadEnd 1 10 (Parser.UnexpectedChar)] in
                      Expect.equal got1 want1
           ],
           describe "number testing" [
               test "int parses correctly" <|
               \_ -> Expect.equal (Ok (NumC 3)) (parse "3"),

               test "float parses correctly" <|
               \_ -> Expect.equal (Ok (NumC 3.25)) (parse "3.25"),

               test "negative parses correctly" <|
               \_ -> Expect.equal (Ok (NumC -1.75)) (parse "-1.75"),
               test "trailing characters do not parse" <|
               \_ -> Expect.err (parse "123abc")

           ],
           describe "idC testing" [
               test "alpha+special chars+num id parses correctly" <|
               \_ -> Expect.equal (Ok (IdC "abc123@#$%")) (parse "abc123@#$%"),
               test "operator names parse correctly" <|
               \_ -> Expect.equal (Ok (IdC ("<="))) (parse "<="),
               test "reserved name doesn't parse" <|
               \_ -> Expect.err (parse "if")
           ],
           describe "appC parsing" [
               test "single arg application parses" <|
               \_ -> Expect.equal (Ok (AppC (IdC "f") [NumC 3])) (parse "{f 3}"),
               test "multiple arg application parses" <|
               \_ -> Expect.equal (Ok (AppC (IdC "+") [(AppC (IdC "+") [NumC 2, NumC 2]), NumC 4])) (parse "{+ {+ 2 2} 4}"),
               test "zero arg application parses" <|
               \_ -> Expect.equal (Ok (AppC (IdC "g") [])) (parse "{g}")
           ],
           describe "ifC parsing" [
               test "if with simple condition parses" <|
               \_ -> Expect.equal (Ok (IfC (IdC "true") (IdC "a") (IdC "b"))) (parse "{if true a b}"),
               test "if with nested expressions parses" <|
               \_ -> Expect.equal (Ok (IfC (AppC (IdC "<=") [IdC "a", IdC "b"]) (AppC (IdC "+") [IdC "a", IdC "b"]) (AppC (IdC "/") [IdC "a", IdC "b"])))
                    (parse "{if {<= a b} {+ a b} {/ a b}}")],
            describe "lamC parsing" [
                test "lambda with no args parses" <|
                \_ -> Expect.equal (Ok (LamC [] (AppC (IdC "+") [(NumC 3), (NumC 4)]))) (parse "{() => {+ 3 4}}"),
                test "lambda with single args parses" <|
                \_ -> Expect.equal (Ok (LamC ["x"] (IdC "x"))) (parse "{(x) => x}"),
                test "lambda with multiple args parses" <|
                \_ -> Expect.equal (Ok (LamC ["a", "b"] (IfC (AppC (IdC "<=") [(IdC "a"), (IdC "b")]) (IdC "a") (IdC "b")))) (parse "{(a b) => {if {<= a b} a b}}")
            ],
            describe "local parsing" [
                test "local with single binding parses" <|
                \_ -> Expect.equal (Ok (AppC (LamC ["x"] (AppC (IdC "+") [IdC "x",NumC 3])) [NumC 7])) (parse "{local {x 7} in {+ x 3}}"),
                test "local with function in binding parses" <|
                \_ -> Expect.equal (Ok (AppC (LamC ["f"] 
                                        (AppC (IdC "f") [(NumC 3)])) [(LamC ["a", "b"] (AppC (IdC "+") [IdC "a", IdC "b"]))]))
                                        (parse "{local {f {(a b) => {+ a b}}} in {f 3}}")
            ],
            describe "many combined rules" [
                test "big boy" <|
                \_ -> Expect.ok (parse bigTest)
            ]]

flip: (a -> b -> c) -> b -> a -> c
flip f = \b a -> f a b
interpTests: Test
interpTests  = describe "tests for interp module" [
    describe "test interp numC" [
        test "eval numC" <|
        \_ -> Expect.equal (Ok (NumV 5)) (interp (NumC 5) Dict.empty)],
    describe "test interp stringC" [
        test "eval stringC" <|
        \_ -> Expect.equal (Ok (StringV "blah")) (interp (StringC "blah") Dict.empty)
    ],
    describe "test interp cloV" [
        test "interp cloV" <|
        \_ ->  case parse "{local {z 7} in {() => z}}" of
                    Ok expr -> Expect.equal (Ok (CloV [] (IdC "z") (Dict.fromList [("z", NumV 7)])))
                                (interp expr Dict.empty)
                    Err _ -> Expect.fail ("unexpected parse error")
    ]]

fibTest : String
fibTest = """ {local {fib {(rec n) =>
                       {if {<= n 1}
                               n
                               {+ {rec rec {- n 1}} {rec rec {- n 2}}}}}} in {fib fib 7}}"""

topInterpTests: Test
topInterpTests = describe "tests for top-interp" [
    describe "test num interp" [
        test "top interp 5" <|
        \_ -> Expect.equal (Ok "5") (topInterp "5")
    ],
    describe "test closure top interp" [
        test "top interp closure" <|
        \_ -> Expect.equal (Ok "#<procedure>") (topInterp "{local {z 7} in {() => z}}")
    ],
    describe "equals? tests" [
        test "closures" <|
        \_ -> Expect.equal (Ok "false") (topInterp "{equal? {() => 5} {() => 5}}"),
        test "primops" <|
        \_ -> Expect.equal (Ok "false") (topInterp "{equal? + +}"),
        test "strings" <|
        \_ -> Expect.equal (Ok "true") (topInterp "{equal? \"string1\" \"string1\"}"),
        test "numbers" <|
        \_ -> Expect.equal (Ok "true") (topInterp "{equal? 3 3})"),
        test "numers2" <|
        \_ -> Expect.equal (Ok "false") (topInterp "{equal? 0 1}"),
        test "booleans" <|
        \_ -> Expect.equal (Ok "true") (topInterp "{equal? true true}")
    ],
    describe "leq tests" [
        test "less than" <|
        \_ -> Expect.equal (Ok "true") (topInterp "{<= 3 4}"),
        test "equal to" <|
        \_ -> Expect.equal (Ok "true") (topInterp "{<= 10 10}"),
        test "greater than" <|
        \_ -> Expect.equal (Ok "false") (topInterp "{<= 11 10}")
    ],
    describe "Error tests" [
        test "invalid type leq" <|
        \_ -> Expect.err (topInterp "{<= \"s\" 3}"),
        test "wrong num args plus" <|
        \_ -> Expect.err (topInterp "{+ 3 4 5}"),
        test "wrong num args equal" <|
        \_ -> Expect.err (topInterp "{equal? 2 3 7}"),
        test "div by zero" <|
        \_ -> Expect.err (topInterp "{\\ 5 0}"),
        test "user error" <|
        \_ -> Expect.err (topInterp "{error \"something bad happened\"}"),
        test "error wrong num args" <|
        \_ -> Expect.err (topInterp "{error 2 3}")
    ],
    describe "big test" [
        test "topInterp big test" <|
        \_ -> Expect.equal (Ok "3") (topInterp bigTest),
        test "fib test" <|
        \_ -> Expect.equal (Ok "13") (topInterp fibTest)
    ],
    describe "primop binding" [
        test "bind + to id" <|
        \_ -> Expect.equal (Ok "4") (topInterp "{local {add +} in {add 2 2}}")
    ],
    describe "serializing tests" [
        test "primop serialize" <|
        \_ -> Expect.equal (Ok "#<primop>") (topInterp "+"),
        test "closure serialize" <|
        \_ -> Expect.equal (Ok "#<procedure>") (topInterp "{() => 5}")
    ]]