(try
	failwith "oops!"
with
	| e ->
		print_endline (Printexc.to_string e);
		Printexc.print_backtrace stderr;
		Out_channel.flush stderr;
);
Unix.sleep 1
