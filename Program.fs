
open System
open System.Text.RegularExpressions

type Expression = 
    | Add
    | Substract
    | Multiply
    | Divide
    | Power
    | OpenBraket
    | CloseBraket
    | Number of float
    | Symbol of string
    | BadToken
    | EOF
type Node = 
    | BinaryNode of lhs:Node*op:(float->float->float)*rhs:Node
    | UnaryNode of op:(float->float)*hs:Node
    | FunctionNode of func:(float->float)*hs:Node
    | LeafNumber of float
    | LeafVariable of string
    | EmptyLeaf
let operator = 
    let unaryOperator:(string*(float->float->float))List = [("^",( ** ));("*",(*));("/",(/));("+",(+));("-",(-))]
    unaryOperator |> Map.ofList
let Function = 
    let negate x:float = -x
    let rec fact (x:float) = 
        let n = x |> Math.Floor 
        match n with
        | 0.0 -> 1.0
        | _ -> n * fact (n - 1.0)
    let unaryOperator:(string*(float->float))List = [("neg",(negate));("sin",(Math.Sin));("cos",(Math.Cos));("tan",(Math.Tan));
                                                     ("exp",(Math.Exp));("log",(Math.Log));("abs",(Math.Abs));("sqrt",(Math.Sqrt));
                                                     ("sinh",(Math.Sinh));("cosh",(Math.Cosh));("tanh",(Math.Tanh));("acos",(Math.Acos));
                                                     ("asinh",(Math.Asinh));("acosh",(Math.Acosh));("atanh",(Math.Atanh));("fact",(fact));
                                                     ("atan",(Math.Atan));("asin",(Math.Asin));("cein",(Math.Ceiling));("floor",(Math.Floor))]
    unaryOperator |> Map.ofList
let constants (s:string) =
    match s.ToLower() with 
    | "e" -> Math.E
    | "p" | "pi" | "π" -> Math.PI
   
let rec evaluate (node:Node)=
    match node with 
    | LeafNumber(value) -> value 
    | BinaryNode(lhs,op,rhs) -> (op) (evaluate lhs) (evaluate rhs) 
    | FunctionNode(f,rhs) -> (f)(evaluate rhs) 
    | UnaryNode(op,hs) -> op (evaluate hs)
    | LeafVariable(name) -> constants name
let rec traverse (root:Node)= 
    match root with 
    | BinaryNode(lhs,op,rhs) ->
        printf "((%A)" (traverse lhs) 
        printf "%A" op
        printf "(%A))"(traverse rhs)
    | LeafNumber(value) -> printf "(%A)" value
    | LeafVariable(value) -> printf "(%A)" value
    | FunctionNode(name,param) -> 
        printf "(%A" name
        printf "(%A))" (traverse param)
    | UnaryNode(op,rhs) -> 
        printf "(%A" op
        printf "(%A))" (traverse rhs)
    | _ -> printfn ""

let getInput = Console.ReadLine() + "#"

let Lex s = 
    let (|Symbol|Operator|Braket|Space|BadToken|EOF|) input = 
        if Regex.IsMatch(input,"[*\-+\/\^]") then Operator
        elif Regex.IsMatch(input,"\(|\)") then Braket
        elif Regex.IsMatch(input,"[a-zA-Z0-9\.]") then Symbol
        elif Regex.IsMatch(input," ") then Space
        elif input = "#" then EOF
        else BadToken
    let (|Plus|Minus|Star|Slash|Hat|) input = 
        if input="+" then Plus
        elif input="-" then Minus
        elif input="*" then Star
        elif input="/" then Slash
        else Hat
    let (|Open|Close|) input = 
        if Regex.IsMatch(input,"\(") then Open else Close
    let (|Number|Variable|None|) input = 
        if Regex.IsMatch(input,"[0-9.]") then 
            Number 
        elif Regex.IsMatch(input,"[a-zA-Z]") then 
            Variable
        else None
    let rec Tokenize (t:List<Expression>,i) (s:string) = 
        let getChar (s:string) i = 
            if i < s.Length then
                s.Chars i |> string
            else "#"
        let isDigit s = 
            match s with Number -> true |  _ -> false
        let isVariable s = 
            match s with Variable -> true | _ -> false
        let rec getIdentifier (s,i) (f:(string->bool)) currentString:string=
            if f (getChar s i) then
                getIdentifier (s,i+1) (f) (currentString + getChar s i)
            else
                currentString
        let CurrentChar = getChar s i
        match CurrentChar with 
        | Operator -> 
            match CurrentChar with 
            | Plus -> Tokenize (Add::t,i+1) s
            | Star -> Tokenize (Multiply::t,i+1) s
            | Slash -> Tokenize (Divide::t,i+1) s
            | Minus -> Tokenize (Substract::t,i+1) s
            | Hat -> Tokenize (Power::t,i+1) s
        | Braket -> 
            match CurrentChar with 
            | Open -> Tokenize (OpenBraket::t,i+1) s
            | Close -> Tokenize (CloseBraket::t,i+1) s
        | Symbol ->
            match CurrentChar with
            | Number ->
                let Identifier = getIdentifier (s,i) isDigit ""
                Tokenize (Number(Identifier|>float)::t,i + Identifier.Length) s
             | Variable ->
                 let Identifier = getIdentifier (s,i) isVariable ""
                 Tokenize (Symbol(Identifier)::t,i + Identifier.Length) s
        | BadToken | Space -> 
            Tokenize (t,i + 1) s
        | EOF -> EOF::t
    s |> Tokenize([],0) |> List.rev
let Parse (token:Expression list) = 
    let mutable index = 0
    let GetToken (token:Expression list) i = token.Item i 
    let mutable CurrentToken = GetToken token index
    let NextToken() = 
        index <- index + 1
        CurrentToken<- GetToken token index
    let rec parseLeaf():Node =  
        match CurrentToken with 
        | Number(value) -> 
            NextToken()
            LeafNumber(value)
        | OpenBraket ->
            NextToken()
            let value = parseSecond(EmptyLeaf)
            match CurrentToken with
            | CloseBraket -> 
                NextToken()
                value
            | _ -> failwith "Expression Not Balanced" 
        | Symbol(name) ->
            NextToken()
            match CurrentToken with
            | OpenBraket -> 
                NextToken()
                let value = parseSecond(EmptyLeaf)
                match CurrentToken with 
                | CloseBraket -> 
                    NextToken()
                    FunctionNode(Function.Item name,value)
                | _ -> failwith "Expression Not Balanced" 
            | _ -> LeafVariable(name)
    and parseUnit():Node= 
        match CurrentToken with
        | Substract -> 
            NextToken()
            UnaryNode(Function.Item "neg",parseUnit())
        | Add -> 
            NextToken()
            parseUnit()
        | _ -> parseLeaf()
    and parseFirst(lhs:Node):Node= //do it recursivly
        match lhs with 
        | EmptyLeaf -> parseFirst(parseUnit())
        | _ ->
            match CurrentToken with 
            | Multiply-> 
                NextToken()
                parseFirst(BinaryNode(lhs,operator.Item "*",parseUnit()))
            | Divide -> 
                NextToken()
                parseFirst(BinaryNode(lhs,operator.Item "/",parseUnit()))
            | Power -> 
                NextToken()
                parseFirst(BinaryNode(lhs,operator.Item "^",parseUnit()))
            | _ -> lhs
    and parseSecond(lhs:Node):Node= 
        match lhs with 
            | EmptyLeaf -> parseSecond(parseFirst(EmptyLeaf))
            | _ ->
                match CurrentToken with 
                | Add-> 
                    NextToken()
                    parseSecond(BinaryNode(lhs,operator.Item "+",parseFirst(EmptyLeaf)))
                | Substract -> 
                    NextToken()
                    parseSecond(BinaryNode(lhs,operator.Item "-",parseFirst(EmptyLeaf)))
                | _ -> lhs
    parseSecond(EmptyLeaf)

[<EntryPoint>] 
let main args =
    getInput|> Lex |> Parse |> evaluate |> printfn "%A"
    0