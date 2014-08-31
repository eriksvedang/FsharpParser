module Parser

let (~%) (s: string) = s.ToCharArray() |> Array.toList
let charListToStr cs = String.concat "" (List.map (fun c -> c.ToString()) cs)

let ($) a b = a b

// 'r is the type the parser produces, like the AST
// The function stored in the parser takes a list of chars
// and returns a list of pairs containing the result and
// the remaining chars
type Parser<'r> = Parser of (char list -> ('r * char list) list)

let parse (Parser p) = p

let (>>=) p f = 
    Parser(fun chars -> 
        List.concat [for (r, remaining) in parse p chars -> parse (f r) remaining])

let (>>) p q = p >>= fun _ -> q

// Consume nothing, create a result 'r'
let mreturn r = Parser(fun chars -> [(r, chars)])

let lambda = Parser(fun _ -> [])

// Parse one char unconditionally
let item = Parser(fun chars -> match chars with
                               | [] -> []
                               | (c::cs) -> [(c,cs)])

let sat cond = item >>= fun c -> if cond c then mreturn c else lambda

let ch c = sat ((=) c)




