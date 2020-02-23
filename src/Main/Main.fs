module Tathdl.Main

open System
open FParsec
open Tathdl.Syntax
open Tathdl.Semantics
open Tathdl.Vhdl

let getResult = function
  | Success(result, _, _) -> result
  | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

let parseAutomaton fileName =
  runParserOnFile graph () fileName System.Text.Encoding.UTF8
  |> getResult |> Automaton.Read

[<EntryPoint>]
let main argv =
  let atmtn = argv.[0] |> parseAutomaton
  let clockFreq = run clockFrequency argv.[1] |> getResult
  Vhdl.codegen System.Console.Out atmtn clockFreq
  0
