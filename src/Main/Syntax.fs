module Totohdl.Syntax

open System
open FParsec

// Enums
type StdLogic = Zero | One
type PlusMinus = Plus | Minus
type MulDiv = Mul | Div
type Comparator = LT | LTE | GT | GTE
type Equality = EQ | NotEQ
type ConstraintOp = Comparator | Equality

// Markers
[<Measure>] type μs
[<Measure>] type MHz = μs^-1
type Parser<'t> = Parser<'t, unit>
type Time = decimal<μs>
type NodeId = NodeId of string
type GrtyId = GrtyId of string
let microSec v = (decimal v) * 1m<μs>

// Discriminators
type Primary =
    ScaledVariable of decimal option * string
  | PrimaryTime of decimal<μs>
  | PrimaryNumber of decimal
  | PrimaryExpr of Expr
and MulExpr = MulExpr of (string * Primary)
and Factor = Factor of Primary * MulExpr option
and Expr = Expr of Factor * (PlusMinus * Factor) option
type ConstraintExpr =
    ComparatorExpr of (Comparator * GrtyId) * Expr
  | RangeExpr of (Equality * GrtyId) * Expr * Expr
type Constraint = Constraint of (string * ConstraintExpr)
type Condition = Else | Specific of (StdLogic option * Constraint list)
type Output = Assign of (string * StdLogic)
type Clock = Clock of string
type Property = Property of string * Expr
type Stmt =
  | Properties of Property list
  | Node of NodeId * string
  | Edge of NodeId * NodeId * (Condition * Clock list * Output list)
type Graph = Graph of string * Stmt list

// Printing
let timeString t = sprintf "%.0fus" (t / 1m<μs>)

let print result =
  match result with
  | Success(result, _, _) -> printfn "Success: %A" result
  | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

// Basic parsers
let testFile p fileName = runParserOnFile p () fileName System.Text.Encoding.UTF8 |> print
let str p = pstring p
let ws = spaces
let (~%) s = pstring s .>> ws
let isLetter c = isAsciiLetter c || c = '_'
let isGreek c = String.exists (fun d -> c = d) "αβγδε_"
let isLetterOrDigits c = isLetter c || isDigit c
let isGreekOrDigits c = isGreek c ||  isDigit c
let variable = many1Satisfy2L isLetter isLetterOrDigits "variable" .>> ws
let nodeId = many1SatisfyL isLetterOrDigits "node ID" .>> ws |>> NodeId
let grtyId = (many1Satisfy2L isGreek isGreekOrDigits "granularity ID") <|>% "α" .>> ws
let ident: Parser<_> = variable <|> grtyId
let clockFrequency: Parser<decimal<MHz>> = pfloat .>> %"MHz" |>> (fun v -> (decimal v) * 1m<MHz>)

// Tokens
let stringLiteral =
  let unescape c =
    match c with | 'n' -> "\n" | 'r' -> "\r" | 't' -> "\t" | c -> string c
  let normal = manySatisfy (fun c -> c <> '"' && c <> '\\')
  let escaped = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
  stringsSepBy normal escaped
let label p = between %"[" %"]" (%"label" >>. %"=" >>. p)
let stdLogic = (stringReturn "0" Zero <|> stringReturn "1" One) .>> ws
let timeUnit = choice [ %"s" >>% 6; %"ms" >>% 3; %"us" >>% 0; %"ns" >>% -3 ] .>> ws
let dquotes p = between %"\"" %"\"" p
let comparator: Parser<_> =
  (choice [ %">=" >>% GTE; %"<=" >>% LTE; %"<" >>% LT; %">" >>% GT ])
  .>>.? (grtyId |>> GrtyId)
let equalityOp = choice [ %"!=" >>% NotEQ; %"=" >>% EQ ] .>>.? (grtyId |>> GrtyId)

// Expressions
let expr, exprRef = createParserForwardedToRef<Expr, unit>()
let primary, primaryRef = createParserForwardedToRef<Primary, unit>()
let number: Parser<decimal> = pfloat |>> decimal
let makePrimaryTime (coeff, scale) =
  (float coeff) * (10.0 ** (float scale)) |> microSec |> PrimaryTime
let primaryTime = (pint32 .>>? ws) .>>.? timeUnit |>> makePrimaryTime
let scaledVariable = (opt number .>>? ws) .>>.? ident |>> ScaledVariable
let mulExpr = (%"*" <|> %"/") .>>. primary |>> MulExpr
let factor = primary .>>. (opt mulExpr) |>> Factor
let addExpr = choice [ %"+" >>% Plus; %"-" >>% Minus ] .>>. factor
do exprRef := factor .>>. opt addExpr |>> Expr
do primaryRef := choice [
  primaryTime; scaledVariable;
  number |>> decimal |>> PrimaryNumber;
  between %"(" %")" expr |>> PrimaryExpr
]

// Nodes and edges
let property = (ident .>> %"=") .>>. expr |>> Property
let properties = sepBy property %","
let comparatorExpr = comparator .>>.? expr |>> ComparatorExpr
let rangeExpr = (tuple3 equalityOp expr (%"±" >>. expr)) |>> RangeExpr
let aConstraint = ident .>>. (comparatorExpr <|> rangeExpr) |>> Constraint
let constraints = between %"[" %"]" (sepBy aConstraint %",")

let action = ident .>> %":=" .>>. stdLogic |>> Assign
let actions = (%"/" >>. (sepBy action %",")) <|>% []

let clocks = (between %"{" %"}" (sepBy ident %",") |>> List.map Clock) <|>% []
let specific = (opt stdLogic) .>>. (constraints <|>% []) |>> Specific
let condition = (stringReturn "else" Else) .>> ws <|> specific
let transitionAttrs = dquotes (tuple3 condition clocks actions)

let node = nodeId .>>.? (label (dquotes stringLiteral)) |>> Node
let edge = tuple3 (nodeId .>> %"->") nodeId (label transitionAttrs) |>> Edge

// Containers
let comment = %"." .>> stringLiteral
let graphAttr = %"graph" >>. label (dquotes (properties |>> Properties .>> comment))
let stmtList = many1 (graphAttr <|> node <|> edge)
let body = between %"{" %"}" stmtList .>> ws
let graph = (ws >>. %"digraph" >>. ident .>>. body .>> eof) |>> Graph