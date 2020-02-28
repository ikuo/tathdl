module Tathdl.Semantics

open Tathdl.Syntax

let uniq lst = lst |> Set.ofList |> Set.toList

let rec eval (props: Map<string,Expr>, expr) =
  match expr with
  | Expr(Factor(PrimaryTime(v), None), None) -> v
  | Expr(Factor(ScaledVariable(coeff, propName), None), additive) ->
      let leftVal =
        match coeff with
        | None -> eval(props, props.Item(propName))
        | Some(v) -> v * eval(props, props.Item(propName))
      // TODO: leftVal = leftVal * middleVal
      match additive with
      | None -> leftVal
      | Some(Plus,  expr') -> leftVal + (eval(props, Expr(expr', None)))
      | Some(Minus, expr') -> leftVal - (eval(props, Expr(expr', None)))
  | _ -> failwith (sprintf "Unsupported expression %A" expr)

type State(id: string, label: string option) =
  member x.Id = id
  override x.GetHashCode() = hash(id)
  override x.Equals(o) =
    match o with | :? State as s -> s.Id = id | _ -> false
  override x.ToString() = sprintf "st:%s" id

type Clock(id: string) =
  member x.Id = id
  override x.ToString() = id
  static member Read = function Syntax.Clock(id) -> Clock(id)
  override x.GetHashCode() = hash(id)
  override x.Equals(o) = match o with :? Clock as o -> o.Id = id | _ -> false
  interface System.IComparable with
    member x.CompareTo(o) =
      match o with :? Clock as c -> c.Id.CompareTo(x.Id) | _ -> 1

type Constraint
  ( clock: Clock,
    granularity: decimal<μs>,
    comparator: Comparator,
    value: decimal<μs>,
    valueFilter: decimal -> decimal
  ) =

  member x.Clock = clock
  member x.Granularity = granularity
  member x.Comparator = comparator
  member x.Value = value
  member x.IgnoreBits (clockFreq: decimal<MHz>) =
    floor (log (float (granularity * clockFreq)) / log 2.0) |> int
  member x.ComparisonValue (clockFreq: decimal<MHz>) =
    (value * clockFreq) / (pown 2.0m (x.IgnoreBits clockFreq)) |> valueFilter |> int

  override x.ToString() =
    let grtySpec = timeString granularity
    sprintf "%s@%s %A %s" clock.Id grtySpec comparator (timeString value)

  static member Read(props: Map<string,Expr>) = function
    Syntax.Constraint(clkId, cExpr) ->
      match cExpr with
      | ComparatorExpr((op, grty), expr) ->
        let (GrtyId gid) = grty
        let grty = eval(props, props.Item(gid))
        let value = eval(props, expr)
        let filter = match op with | LT | LTE -> floor | GT | GTE -> ceil
        Constraint(Clock clkId, grty, op, value, filter) :: []
      | RangeExpr((equality, GrtyId gid), expr, rangeExpr) ->
        let grty = eval(props, props.Item(gid))
        let range = eval(props, rangeExpr)
        let pivot = eval(props, expr)
        let left = pivot - range
        let right = pivot + range
        let trios =
          match equality with
          | EQ    -> [ (GTE, left, floor); (LTE, right, ceil) ]
          | NotEQ -> [ (LTE, left, floor); (GTE, right, ceil) ]
        trios |> List.map (fun (op, v, f) -> Constraint(Clock clkId, grty, op, v, f))

type Condition = Else | Specific of (StdLogic option * Constraint list)

type Transition
  ( src: State,
    dst: State,
    condition: Condition,
    clocks: Clock list,
    outputs: Output list
  ) =

  member x.Src = src
  member x.Dst = dst
  member x.Condition = condition
  member x.Clocks = clocks
  member x.Outputs = outputs

  override x.ToString() =
    let clocksStr = clocks |> List.map (fun s -> s.ToString()) |> String.concat ","
    let condSpec =
      match condition with
      | Else -> "else"
      | Specific(input, cs) ->
        let inputSpec =
          match input with | Some(Zero) -> "0 " | Some(One) -> "1 " | _ -> ""
        sprintf "%s%A" inputSpec cs
    let outputsSpec =
      if outputs.Length = 0 then ""
      else sprintf " / %A" outputs
    sprintf "%A->%A %s {%s}%s" src dst condSpec clocksStr outputsSpec

  member x.ActionVariables =
    outputs |> List.map (function (Assign(variable, _)) -> variable) |> uniq

  static member Read(states: Map<string, State>, props, clocks: Set<Clock>) = function
    (src, dst, cond, cs, os) ->
      let cond': Condition =
        match cond with
        | Syntax.Else -> Else
        | Syntax.Specific(input, cs) ->
          Specific(input, cs |> List.collect (Constraint.Read(props)))
      Transition(states.Item(src), states.Item(dst), cond', cs, os)

type Automaton
  ( name: string,
    states: State list,
    start: State,
    transitions: Transition list
  ) =
  member x.Name = name
  member x.States = states
  member x.Start = start

  member x.CounterNumBits (clockFreq: decimal<MHz>): Map<string, int> =
    let makePair (constr: Constraint) =
      (constr.Clock.Id, (constr.Value * clockFreq) |> ceil |> int)
    let reducer (map: Map<string, int>) (clkId: string, value: int) =
      let v = if (Map.containsKey clkId map) then (Map.find clkId map) else 0
      Map.add clkId (max v value) map
    let map: Map<string, int> =
      transitions |> List.collect (fun t ->
        match t.Condition with
          | Else -> []
          | Specific(_, cs) -> cs |> List.map makePair)
        |> List.fold reducer Map.empty
    Map.map (fun k v -> (log (float (v + 1))) / (log 2.0) |> ceil |> int) map

  member x.TransitionsByState: Map<string, Transition list> =
    let reducer map (transition: Transition) =
      let key = transition.Src.Id
      let ts = if Map.containsKey key map then Map.find key map else []
      Map.add key (transition :: ts) map
    List.fold reducer Map.empty transitions
    // TODO: reorder by "else"

  member x.ActionVariables =
    transitions |> List.collect (fun t -> t.ActionVariables) |> uniq

  override x.ToString () =
    sprintf "Automaton(%s):\nstates=%A\ntransitions=%A)" name states transitions

  static member addImplicits (states: Map<string, State>) ts =
    let add (id: string) (ss: Map<string, State>) =
      if ss.ContainsKey id then ss
      else ss.Add(id, State(id, None))
    let reducer ss = function (src, dst, _, _, _) -> ss |> (add src) |> (add dst)
    ts |> List.fold reducer states

  static member Read = function
    Graph(name, stmts) ->
      let reducer (ps, ss, ts, cs) = function
        | Properties(lst) -> (ps @ lst, ss, ts, cs)
        | Node(NodeId(id), label) -> (ps, State(id, label) :: ss, ts, cs)
        | Edge(NodeId(src), NodeId(dst), (cond, clks, os)) ->
          let clocks = clks |> List.map Clock.Read
          let trans = (src, dst, cond, clocks, os)
          let newCs = Set.union cs (Set.ofList clocks)
          (ps, ss, trans :: ts, newCs)
      let (ps, ss, ts, cs) = List.fold reducer ([],[],[],Set.empty) stmts
      let props = ps |> List.map (function Property(name, expr) -> (name, expr)) |> Map.ofList
      let states' = ss |> List.map (fun s -> (s.Id, s)) |> Map.ofList
      let states = Automaton.addImplicits states' ts
      let transs = ts |> List.map (Transition.Read(states, props, cs))
      let ss' = Map.toList states |> List.map (fun (k,v) -> v)
      Automaton(name, ss', (List.rev ss).Head, transs)
