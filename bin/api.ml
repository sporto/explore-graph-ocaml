open Lwt.Infix
open Graphql_lwt
(* open Lib *)

module Graphql_cohttp_lwt = Graphql_cohttp.Make (Graphql_lwt.Schema) (Cohttp_lwt_unix.IO) (Cohttp_lwt.Body)

(* let () =
  Server.start ~ctx:(fun _req -> ()) Graph.schema
    |> Lwt_main.runss *)

let () =
  let on_exn =
  	function
	    | Unix.Unix_error (error, func, arg) ->
	      Logs.warn (fun m ->
	        m  "Client connection error %s: %s(%S)"
	          (Unix.error_message error) func arg
	      )
	    | exn ->
        Logs.err (fun m -> m "Unhandled exception: %a" Fmt.exn exn)
  in
  let callback =
  	Graphql_cohttp_lwt.make_callback
      (fun _req -> ())
      Lib.Graph.schema
  in
  let server =
  	Cohttp_lwt_unix.Server.make_response_action ~callback () 
  in
  let mode =
  	`TCP (`Port 8080) 
  in
  Cohttp_lwt_unix.Server.create ~on_exn ~mode server
  	|> Lwt_main.run