open Yojson.Basic.Util
open Ologs


let get_ name content = 
	[content]
		|> filter_member "objects"
		|> flatten
		|> filter_member "aspects"
		|> flatten
		|> filter_member name
		|> filter_string

let () = 
	let content_file = Yojson.Basic.from_file "parente.json" in 
	(*List.iter print_endline (get_ "aspect_type" content_file);;*)
	pretty_to_string to_string content_file;;
