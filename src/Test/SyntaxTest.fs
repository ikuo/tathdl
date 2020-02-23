module Tests

open Expecto
open Expecto.Flip
open FParsec
open Totohdl.Syntax
open Swensen.Unquote

[<Tests>]
let tests =
  testList "Syntax" [
    testCase "expr" <| fun _ ->
      let actual = run expr "16T-a"
      let result =
        match actual with
            Success(Expr(Factor(ScaledVariable(Some(_), "T"), _), _), _, _) -> true
          | _ -> false
      test <@ result @>
  ]