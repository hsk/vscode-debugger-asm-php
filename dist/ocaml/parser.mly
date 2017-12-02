%{ open Ast
  let rec call a l = function
  | [Reg x] -> Call(a,List.rev l,x)
  | x::xs -> call a (x::l) xs
%}
%token ADD SUB MUL DIV CALL ENTER PRINT RET COLON
%token <string> ID %token <int> INT EOL EOF
%start program
%type <Ast.program> program
%start imm
%type <Ast.imm> imm
%%
program:| insts EOF             { $1 }
insts:  | label                 { [] }
        | label inst EOL insts  { ($1,($2,$3))::$4 }
label:  | ID COLON eols         { $1 }
        | eols                  { "" }
eols:   | /* empty */           { () }
        | EOL eols              { () }
inst:   | ADD imm imm reg       { Add($2,$3,$4) }
        | SUB imm imm reg       { Sub($2,$3,$4) }
        | MUL imm imm reg       { Mul($2,$3,$4) }
        | DIV imm imm reg       { Div($2,$3,$4) }
        | CALL ID imms          { call $2 [] $3 }
        | ENTER regs            { Enter($2) }
        | PRINT imm             { Print($2) }
        | RET imm               { Ret($2) }
imms:   | imm                   { [$1] }
        | imm imms              { $1::$2 }
regs:   |                       { [] }
        | reg regs              { $1::$2 }
reg:    | ID                    { $1 }
imm:    | reg                   { Reg($1) }
        | INT                   { Int($1) }
