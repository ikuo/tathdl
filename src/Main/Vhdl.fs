module Tathdl.Vhdl

open System
open System.Text.RegularExpressions
open Syntax
open Semantics

type Counter(id: int, numBits: int, divider: Counter) =
  class end

type FrequencyDivider =
  | CounterDiv of Counter
  | ClockDiv of decimal<MHz>

let flatMap lst =
  let reducer state = function Some(v) -> v :: state | None -> state
  lst |> List.fold reducer [] |> List.rev

let put out str =
  let str' = Regex.Replace(str, "^\\S*\\|", "", RegexOptions.Multiline)
  fprintfn out "%s" str'

let stateSpec (stateId: string) = sprintf "s%s" stateId
let counterName id = sprintf "counter_%s" id
let counterMaxName id = sprintf "counter_%s_max" id

let codegen out (automaton: Automaton) (clockFreq: decimal<MHz>) =
  let aut = automaton
  let outports = aut.ActionVariables
  let outSpec = outports |> List.map (sprintf "  %s: OUT STD_LOGIC") |> String.concat ";\n"

  let emitN = fprintfn out
  let emit0 (str: string) = out.WriteLine(str)
  let emit0' (str: string) = out.Write(str)

  let emitEntity f =
    emitN "ENTITY %s IS" aut.Name; f()
    emitN "END %s;\n" aut.Name

  let emitCounter (counterId: string) (numBits: int) =
    let ctrName = counterName counterId
    let ctrMaxName = counterMaxName counterId
    fprintfn out "  CONSTANT %s : INTEGER := 2 ** %d;" ctrMaxName numBits
    fprintfn out "  SIGNAL %s : UNSIGNED (%d downto 0) := (others => '0');" ctrName numBits

  let emitIncrement (counterId: string) _ =
    let ctrName = counterName counterId
    let ctrMaxName = counterMaxName counterId
    fprintfn out "      %s <= %s + 1;" ctrName ctrName

  let emitArchitecture f =
    let statesSpec = aut.States |> List.map (fun s -> stateSpec s.Id) |> String.concat ","
    emitN "ARCHITECTURE rtl OF %s IS" aut.Name
    emitN "  TYPE StateType IS (%s);" statesSpec
    emitN "  SIGNAL state: StateType := %s;" (stateSpec aut.States.[0].Id)
    aut.CounterNumBits clockFreq |> Map.iter emitCounter
    emit0 "BEGIN"
    f()
    emit0 "END rtl;"

  let emitProcess f =
    emit0 "  PROCESS (clock,reset)"
    sprintf "\
      |  BEGIN\n\
      |    IF (reset = '1') THEN\n\
      |      state <= %s;" (stateSpec aut.States.[0].Id) |> put out
    emit0 "    ELSIF rising_edge(clock) THEN"
    aut.CounterNumBits clockFreq |> Map.iter emitIncrement
    emit0 "      CASE state IS"
    f()
    emit0 "      END CASE;"
    emit0 "    END IF;"
    emit0 "  END PROCESS;"

  let integerOf = function | Zero -> 0 | One -> 1

  let emitOutput' = function
    | Assign (variable: string, value: StdLogic) ->
      sprintf "          %s <= '%d';" variable (integerOf value)

  let emitComparator' = function
    LT -> "<" | LTE -> "<=" | GT -> ">" | GTE -> ">="

  let emitConstraint (constr: Semantics.Constraint) =
    let cntName = counterName constr.Clock.Id
    let ignBits = constr.IgnoreBits clockFreq
    let valueCount = constr.ComparisonValue clockFreq
    let comparator = emitComparator' constr.Comparator
    sprintf "shift_right(%s, %d) %s %d" cntName ignBits comparator valueCount

  let emitConstraints' cs =
    cs |> List.map emitConstraint |> String.concat " AND "

  let emitCondition' = function
    | Specific (None, cs) -> emitConstraints' cs
    | Specific (Some(input), []) -> sprintf "input = '%d'" (integerOf input)
    | Specific (Some(input), cs) -> sprintf "input = '%d' AND %s" (integerOf input) (emitConstraints' cs)
    | Else -> failwith "Unexpected 'else'"

  let emitClockReset' indent (clk: Semantics.Clock) =
    sprintf "%s%s <= (others => '0');" indent (counterName clk.Id)

  let mapToOption (lst: 'a list) f =
    if lst.Length > 0 then
      lst |> List.map f |> String.concat "\n" |> Some
    else None

  let emitTransition' (t: Transition) =
    let cond = t.Condition
    let indent = "          "
    let ifElseHeader =
      if cond = Else then "E"
      else (sprintf "IF %s THEN" (emitCondition' cond));
    let clockResets = mapToOption t.Clocks (emitClockReset' indent)
    let outOption = mapToOption t.Outputs emitOutput'

    [ ifElseHeader |> Some;
      clockResets;
      outOption;
      sprintf "%sstate <= %s;" indent (stateSpec t.Dst.Id) |> Some
    ] |> flatMap |> String.concat "\n"

  let emitTransitions () =
    aut.TransitionsByState |> Map.iter (fun stateId ts ->
      emitN "      WHEN %s =>" (stateSpec stateId)
      emit0' "        "
      ts |> List.map emitTransition' |> String.concat "\n        ELS" |> emit0
      emit0 "        END IF;")

  sprintf "\
    |LIBRARY ieee;\n\
    |USE ieee.std_logic_1164.ALL;\n\
    |USE ieee.numeric_std.ALL;\n" |> put out

  emitEntity <| fun _ ->
    sprintf "\
      |PORT(clock: IN STD_LOGIC;\n\
      |  reset: IN STD_LOGIC;\n\
      |  input: IN STD_LOGIC;\n\
      |%s);" outSpec |> put out

  emitArchitecture <| fun _ -> emitProcess emitTransitions
