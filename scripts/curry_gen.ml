

(* no mli file emitted, since mli is not very meaningful 
   except which identifiers are exposed 
   exported functions:

   - [ app f args]
      called when apply a curried function with a list of arugments
   - [ _1 o arg ]  .. [ _n]
      called when apply a curried function with [n] argument
   - [ __1 o] .. [ __n o]
*)
let prelude ={|
(* Copyright (C) 2015 -  Hongbo Zhang, Authors of ReScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(* Generated by scripts/curry_gen.ml *)  
external function_length : 'a -> int = "#function_length"
external apply_args : ('a -> 'b) -> _ array -> 'b = "#apply"
external sub : 'a array -> int -> int -> 'a array = "?array_sub"

(* Public *)
let rec app f args = 
  let init_arity = function_length f in
  let arity = if init_arity = 0 then 1 else init_arity in (* arity fixing *)
  let len = Caml_array_extern.length args in
  let d = arity - len in 
  if d = 0 then 
    apply_args f  args (* f.apply (null,args) *)
  else if d < 0 then 
    (* TODO: could avoid copy by tracking the index *)
    app (Obj.magic (apply_args f (sub args 0 arity)))
      (sub args arity (-d))
  else 
    Obj.magic (fun x -> app f (Caml_array_extern.append args [|x|] ))
  
|}

let list_init n  fn = Array.to_list (Array.init n fn)
    
let generate_case
    ?arity
    ~args_number args_array args =
  match arity with
  | None ->
    Printf.sprintf " _ -> Obj.magic (app o [|%s|])"
      (String.concat ";"args)      
  | Some arity ->     
    Printf.sprintf " %d -> %s" arity
      (if arity >= args_number then
         Printf.sprintf "apply%d (Obj.magic o) %s"
           arity (String.concat " " args)
       else
         Printf.sprintf
           "app (apply%d (Obj.magic o) %s) [|%s|]"
           arity
           (String.concat " "(Array.to_list (Array.sub args_array 0 arity)))          
           (String.concat ";"
              (Array.to_list (Array.sub args_array arity (args_number - arity)))))



let number = 8

let generate_apply arity =
  let vars =
    list_init (arity + 1)
      (fun i -> Printf.sprintf "'a%d" i) in
  let ty =
    match vars with
    | [] -> assert false
    | x::xs
      ->       
      List.fold_left
        (fun acc x -> acc ^ " -> " ^ x )
        x xs in         
  Printf.sprintf
    "(* Internal use *)\n\
    external apply%d : (%s) -> %s = \"#apply%d\""
    arity ty ty arity
    


let generate_fun args_number =
  let args_array =
    Array.init args_number (fun i -> Printf.sprintf "a%d" i) in
  let args =  Array.to_list args_array in
  let args_string =  (String.concat " " args) in

  Printf.sprintf {|

let %%private curry_%d o %s arity =
  match arity with
  |%s    

(* Public *)  
let _%d o %s =
  let arity = function_length o in
  if arity = %d then apply%d o %s
  else curry_%d o %s arity     

(* Public *)
let __%d o =
  let arity = function_length o in
  if arity = %d then o
  else fun %s -> _%d o %s
|}
    args_number
    args_string
    (String.concat "\n  |"

       (list_init (number - 1) 
          (fun arity -> generate_case ~arity:(arity + 1) ~args_number args_array args)
        @ [ generate_case ~args_number args_array args]          
       )
    )

    args_number
    args_string
    args_number
    args_number
    args_string
    args_number
    args_string
    

    args_number
    args_number
    args_string
    args_number
    args_string
    
let () =
  print_endline
  @@ Printf.sprintf
    "%s\n%s\n%s"
    prelude
    (String.concat "\n"
       (list_init number (fun i -> generate_apply (i + 1)))       
    )    
    (String.concat "\n"
       (list_init 8 (fun i -> generate_fun (i + 1))))

(* local variables: *)
(* compile-command: "ocaml curry_gen.ml > ../jscomp/runtime/curry.ml" *)
(* end: *)
