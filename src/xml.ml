exception Ses_error of string * string * string

type t = | El of (string * t list) | Data of string

let of_string s =
  let el ((_,local),_) t_list = El (local,t_list) in
  let data s = Data s in
  let input = Xmlm.make_input ~strip:true (`String (0,s)) in
  let _, d = Xmlm.input_doc_tree ~el ~data input in
  d

let rec fetch_nodes xml l =
  match l with
    | h::t ->
      begin
        match xml with
          | El (h_, xml)::_ when h = h_ -> fetch_nodes xml t
          | _::xml -> fetch_nodes xml l
          | _ -> raise Not_found
      end
    | _ -> xml

let nodes_of_string s xml =
  let l = Str.split (Str.regexp "\\.") s in
  fetch_nodes xml l

let data_of_string s xml =
  let l = Str.split (Str.regexp "\\.")  s in
  match fetch_nodes xml l with
    | [ Data d ] -> d
    | _ -> raise Not_found

let check_error xml =
  try
    let e = nodes_of_string "ErrorResponse.Error" [ xml ] in
    let type_ = data_of_string "Type" e in
    let code = data_of_string "Code" e in
    let message = data_of_string "Message" e in
    raise (Ses_error (type_,code,message))
  with Not_found -> ()
