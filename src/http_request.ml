open Lwt
open Ocsigen_http_frame
open Ocsigen_stream

let generate_headers headers =
  List.fold_left (
    fun headers (label, value) ->
      let name = Http_headers.name label in
      Http_headers.add name value headers
  ) (Http_headers.empty) headers

let fragment_url url =
  let (https, host, port, uri, _, _, _) = Ocsigen_lib.Url.parse url in
  let host = match host with None -> "localhost" | Some h -> h in
  (https, host, port, "/" ^ uri)

let rec read_response frame =
  let rec read_stream acc = function
    | Ocsigen_stream.Finished (None) -> Lwt.return acc
    | Ocsigen_stream.Finished (Some stream) -> Ocsigen_stream.next stream >>= read_stream acc
    | Ocsigen_stream.Cont (s, next_stream) -> Ocsigen_stream.next next_stream >>= read_stream (acc ^ s)
  in
  match frame.frame_content with
    | None -> Lwt.return ""
    | Some content ->
      let st = Ocsigen_stream.get content in
      lwt s = Ocsigen_stream.next st in
      lwt s = read_stream "" s in
      lwt _ = Ocsigen_stream.finalize content `Success in
      Lwt.return s

(***** GET REQUEST *****)
let get ?https ?port ?(headers=[]) ~host ~uri () =
  let headers = generate_headers headers in
  Ocsigen_http_client.get ~headers ?https ?port ~host ~uri () >>= read_response

let get_url ?headers url () =
  let (https, host, port, uri) = fragment_url url in
  get ?https ?port ?headers ~host ~uri ()


(***** POST REQUEST *****)
let post_string ?https ?port ?(headers=[]) ~host ~uri ~content ?(content_type=("application","x-www-form-urlencoded")) () =
  let headers = generate_headers headers in
  let content = Netencoding.Url.mk_url_encoded_parameters content in
  Ocsigen_http_client.post_string ?https ?port ~headers ~host ~uri ~content ~content_type () >>= read_response

let post_string_url ?headers ?content_type ~content url () =
  let (https, host, port, uri) = fragment_url url in
  post_string ?https ?port ?headers ~host ~uri ~content ?content_type ()

