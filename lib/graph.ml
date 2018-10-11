module C = Cohttp_lwt_unix

open Graphql_lwt

let connection_url = "postgresql://Sebastian@localhost/save_up_dev"

type role 
	= User
	| Admin

type user = {
	id : int;
	name : string;
}

let pool =
	match Caqti_lwt.connect_pool ~max_size:10 (Uri.of_string connection_url) with
	| Ok pool -> pool
	| Error err -> failwith (Caqti_error.show err)

type error =
	| Database_error of string

(* Helper method to map Caqti errors to our own error type. 
   val or_error : ('a, [> Caqti_error.t ]) result Lwt.t -> ('a, error) result Lwt.t *)
let or_error m =
	match%lwt m with
	| Ok a -> Ok a |> Lwt.return
	| Error e -> Error (Caqti_error.show e) |> Lwt.return
	(* | Error e -> Error (Database_error (Caqti_error.show e)) |> Lwt.return *)

let get_all_query =
	Caqti_request.collect
		Caqti_type.unit
		Caqti_type.(tup2 int string)
		"SELECT id, name FROM users"

let get_all () =
	let get_all' (module C : Caqti_lwt.CONNECTION) =
		C.fold get_all_query (fun (id, name) acc ->
        	{ id; name } :: acc
      	) () []
	in
	Caqti_lwt.Pool.use get_all' pool |> or_error

(* let role = Schema.(enum "role"
  ~values: [
    enum_value "USER" ~value:User ~doc:"A regular user";
    enum_value "ADMIN" ~value:Admin ~doc:"An admin user";
  ]
) *)

let user = Schema.(obj "user"
	~fields: (fun _user -> [
		field "id"
			~args: Arg.[]
			~typ: (non_null int)
			~resolve: (fun () p -> p.id)
		;
		field "name"
			~args: Arg.[]
			~typ: (non_null string)
			~resolve: (fun () p -> p.name)
		;
	])
)

let schema = Schema.(schema [
	io_field "users"
		~args: Arg.[]
		~typ: (non_null (list (non_null user)))
		~resolve: (fun () () -> get_all ())
	;
	field "greeter"
		~typ:string
		~args:Arg.[
			arg "config" ~typ:(non_null (obj "greeter_config" ~coerce:(fun greeting name -> (greeting, name)) ~fields: [
				arg' "greeting" ~typ:string ~default:"hello";
				arg "name" ~typ:(non_null string)
			]))
		]
		~resolve: (fun () () (greeting, name) ->
			Some (Format.sprintf "%s, %s" greeting name)
		)
		;
	]
)
