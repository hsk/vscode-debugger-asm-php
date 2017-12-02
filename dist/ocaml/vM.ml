type code = {data:Ast.c;line:int}
type prog = {codes: code array; labels:(Ast.label*int)list}
type frame = {mutable vars: (string * int) list;p:string;pos:int;nm:Ast.label}
let _currentPos = ref 0
let _codes = ref [||]
let _vars = ref []
let _labels = ref []
let frames = ref []
let logger = ref (fun str -> Printf.fprintf stderr "%s\n%!" str)
let log str = !logger str
let error str = log str; failwith str

(* ファイルをパースして構文木データを返す *)
let parseFile filename:prog =
  Lexer.init();
  let inp = open_in filename in
  let codes = Parser.program Lexer.token (Lexing.from_channel inp) in
  let (_,labels) = codes |> List.fold_left(fun (i,l) (label,_)-> (i+1,(label,i)::l)) (0,[]) in
  let codes = codes |> List.map(fun (l,(data,line))->{data;line}) |> Array.of_list in
  {codes; labels}

let parseImm src =
  Lexer.init();
  Parser.imm Lexer.token (Lexing.from_string src)
  
let frame label p = { p; vars= !_vars; pos= !_currentPos; nm=label }

let loadFile filename =
  let a = parseFile filename in
  _codes := a.codes;
  _labels := a.labels;
  _currentPos := List.assoc "main" a.labels;
  frames := [frame "main" "a"]

let getValue = function
  | Ast.Int i -> i
  | Ast.Reg a -> try List.assoc a !_vars with Not_found -> 0
let getLine () = !_codes.(!_currentPos).line
let getCode pos = !_codes.(pos)
let getCode0 () = !_codes.(!_currentPos)
let setValue (reg, v) = _vars := Ast.update reg v !_vars
let step() =
  let next () =
    incr _currentPos;
    (Array.length !_codes) <= !_currentPos
  in
  let code = !_codes.(!_currentPos) in
  match code.data with
  | Ast.Add(a,b,c) -> setValue(c, getValue a + getValue b);next ()
  | Ast.Sub(a,b,c) -> setValue(c, getValue a - getValue b);next ()
  | Ast.Mul(a,b,c) -> setValue(c, getValue a * getValue b);next ()
  | Ast.Div(a,b,c) -> setValue(c, getValue a / getValue b);next ()
  | Ast.Print(a) -> log(string_of_int (getValue a));next ()
  | Ast.Ret(a) ->
    begin match !frames with
    | [_] | [] -> _currentPos := Array.length !_codes; true
    | frame::frames1 ->
      frame.vars <- Ast.update frame.p (getValue(a)) frame.vars;
      _vars := frame.vars;
      _currentPos := frame.pos;
      frames := frames1;
      next ()
    end
  | Ast.Call(label,params,p) ->
    begin try
      let pos = List.assoc label !_labels in
      match !_codes.(pos).data with
      | Ast.Enter(enter) ->
        frames := (frame label p) :: !frames;
        _vars := params |> List.map getValue |> List.combine enter;
        _currentPos := pos;
        next ()
      | c -> assert false
    with Not_found -> error("not found label " ^ label)
    end
  | Ast.Enter _ -> assert false
