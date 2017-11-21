open Ast

let expr_to_tex expr = match expr with
| Text s -> s ^ "/n"
| Comment s -> "\\begin{comment}" ^ s ^ "\\end{comment}"
| _ -> failwith "Unimplemented"

let head_to_tex head_list = failwith "Unimplemented"

let body_to_tex expr_list = 
  List.fold_left 
    (fun acc exp -> acc ^ (expr_to_tex exp))
    "" expr_list

let write_to_file doc = let output_str = match doc with
  | (_, body) -> body_to_tex body in
  
  
  
  "output.tex"