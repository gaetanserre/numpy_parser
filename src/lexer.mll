{
  open Parser

  let check_numpy_func =

    let module S = Set.Make(String) in

    let rec read chan set =
      try
        let s = S.add (input_line chan) set in
        read chan s
      with End_of_file -> set
    in

    let in_channel = open_in Sys.argv.(1) in

    try
      let m = read in_channel S.empty in
      close_in in_channel;
      fun fn -> S.mem fn m
    with e -> (
      close_in_noerr in_channel;
      raise e
    )

  let fail s =
    failwith (Printf.sprintf "Unexpected token: %s" s)

}

let digit  = ['0'-'9']
let number = digit+
let alpha  = ['a'-'z' 'A'-'Z']
let space  = [' ' '\t' '\r']

let float   = number ('.' number)?
let cst     = float | "np.pi" | "np.e" | "10e" '-'? number
let var     = "x"
let np_func = "np." alpha+
let param   = "axis" | "ord" | "p"


rule token = parse
  | space+             { token lexbuf }
  | "+"                { BINOP }
  | "-"                { MINUS }
  | "*"                { BINOP }
  | "/"                { BINOP }
  | "**"               { BINOP }
  | ","                { COMMA }
  | "("                { LPAR }
  | "["                { LBRACE }
  | ")"                { RPAR }
  | "]"                { RBRACE }
  | "="                { EQUAL }
  | cst                { CST }
  | var                { VAR }
  | np_func as np_func { if check_numpy_func np_func then NUMPY_FUNC else fail np_func }
  | param              { PARAM }
  | eof                { EOF }
  | _ as c             { fail String.(make 1 c)}
