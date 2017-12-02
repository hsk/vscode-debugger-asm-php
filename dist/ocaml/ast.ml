type reg = string
type imm =
  | Int of int
  | Reg of reg
type label = string
type c =
  | Add of imm * imm * reg
  | Sub of imm * imm * reg
  | Mul of imm * imm * reg
  | Div of imm * imm * reg
  | Call of label * imm list * reg
  | Enter of reg list
  | Print of imm
  | Ret of imm
type code = c * int
type program = (label * code) list

let rec update a b = function
  | [] -> [a,b]
  | (a1,_)::xs when a=a1 -> (a,b)::xs
  | x::xs -> x:: update a b xs
