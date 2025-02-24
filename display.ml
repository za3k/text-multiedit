open Types
open Color
open Utils
open Constants
open Common

(* Step 8: Update the UI
    8a: Calculate the view, based on the last view, the current terminal size,
        and the cursor position.
    8b: Display the text (or the help)
    8c: Display the active users
    8d: Display the shortcuts
*)

let rec lookup_cursor_color (users: user_state list) (pos: int) : background_color option = 
    (* { user="zachary"; cursor=2; color=Red;  }; *)
    List.map (function
        | { cursor; color } when cursor = pos -> Some color
        | _ -> None
    ) users |> any

let display_viewport (width: int) (text: string) (color_of: int -> background_color option) 
                     (visible_range: int * int) (debug: bool): string list =
    let colorize_char (index: int) (c: char) =
        if (not debug) &&
            (* Restrict to the viewport -- this is a bad way to do this *)
           (index < (fst visible_range) || index > (snd visible_range))
        then "" else
        let s = match c with
            | '\n' -> " "
            | _ -> String.make 1 c
        in match color_of index with
            | Some color -> colorize color s
            | None -> s in

    let open Seq in
    let lines = String.split_on_char '\n' text |> except_last |> List.to_seq
                |> map (fun x -> x ^ "\n") in
    let start_indices = lines |> map String.length |> scan (+) 0 in
    zip start_indices lines |> zip (ints 0) |> map (function
        | (line_no, (line_start, line)) ->
            let slines = line |> unfold (function
                | "" -> None
                | s when String.length s <= width -> Some (s, "")
                | s -> let first = String.sub s 0 width
                       and rest = String.sub s width ((String.length s)-width) in
                       Some (first, rest)) in
            let start_indices = map String.length slines |> scan (+) line_start in
            zip start_indices slines |> zip (ints 0) |> zip (repeat line_no))
    |> concat |> filter_map (function
        | (line_no, (sline_no, (sline_start, sline))) ->
            let colorize = fun i c -> colorize_char (i+sline_start) c in
            let line_number = if sline_no = 0 then Printf.sprintf "%4d " (line_no + 1 - Bool.to_int debug) else "     " and
                sline_num = if debug then Printf.sprintf "%2d " sline_no else "" and
                colorized_sline = (sline |> String.to_seq |> mapi colorize |> List.of_seq |> String.concat "") and
                (* We have to pad horizontally here because of ANSI escape codes mucking with string lengths *)
                padding = String.make (max 0 (width - (String.length sline))) ' ' in
            if colorized_sline = "" then None else (* This is needed because of the visible_range colorizining hack, and the illegal document ''. *)
            line_number ^ sline_num ^ colorized_sline ^ padding |> Option.some)
    |> List.of_seq

let display_document width text color_of debug : string list =
    display_viewport width text color_of (0, Text.document_end text) debug

let spacer_lines width num =
    copies num (String.make width ' ')

let debug_cursors state : string =
    state.per_user |> List.map (function
        | {user; cursor; color} ->
            Printf.sprintf " %s: %d " user cursor |> colorize color)
    |> String.concat "  "

let debug_view vp state local_state : string =
    let (vs, ve) = vp in
    Printf.sprintf " Viewport %d-%d [%d] " vs ve local_state.view |> colorize Blue

let display_help width : string list =
    let second = function (a, b, c) -> b in
    let rec consecutive_pairs default = function
        | [] -> []
        | [a] -> [(a, default)]
        | a :: b :: l -> (a, b) :: consecutive_pairs default l in
    let pad_shorter s1 s2 = 
        match (String.length s1, String.length s2) with
        | (a, b) when a = b -> (s1, s2)
        | (a, b) when a < b -> (s1 ^ (String.make (b-a) ' '), s2)
        | (a, b)            -> (s1, s2 ^ (String.make (a-b) ' ')) in

    let equalize = function ((s1, n1), (s2, n2)) ->
        let (s1, s2) = pad_shorter s1 s2 and
            (n1, n2) = pad_shorter n1 n2 in
        ((s1, n1), (s2, n2)) in
    let bit_for = function
        | (shortcut, name) -> 
            (((String.length shortcut) + (String.length name) + 1), 
             (colorize Blue shortcut) ^ " " ^ name) in

    let shortcuts = List.filter_map second shortcuts in
    let sublists = prefixes @@ List.map equalize @@ consecutive_pairs ("","") shortcuts in
    let display_all cuts : string list option =
        (* Assume we can display every shortcut, then try to do it. *)
        let min_padding = 2 and
            count = List.length cuts and
            line1 = List.map fst cuts |> List.map bit_for and
            line2 = List.map snd cuts |> List.map bit_for in
        let min_size = List.map (fun x -> (fst x)+min_padding) line1 |> sum and
            line1 = List.map snd line1 and
            line2 = List.map snd line2 in
        (* Check the line lengths -- are they too long? If so, return None *)
        if min_size > width then None else
        let padding = String.make ((width - min_size) / count) ' ' in
        List.map (String.concat padding) [line1; line2]
        |> Option.some
    in
    List.find_map display_all sublists |> Option.value ~default:["";""]

let title_line width doc =
    let open String in
    let program = Printf.sprintf " textmu %s " version and
        docname = Printf.sprintf "[%s]" doc in
    let slack = (width - (length program) - (length docname)) in
    let text = program ^ (make slack ' ') ^ docname in
    [colorize Blue text]

let error_line width = function
    | None -> [String.make width ' ']
    | Some error ->
        let error = Printf.sprintf "[ %s ]" error in
        let e = width - (String.length error) in
        let left = String.make (e/2) ' ' and
            right = String.make (e-e/2) ' ' in
        [left ^ error ^ right]

let status_line width state local_state =
    let max_user_len = 20 in
    let for_cursor = function
        | {user; cursor; color} -> (user, color) in
    let bits : (string * background_color) list = List.map for_cursor state.per_user in
    let resize target_size s =
        match String.length s with
        | l when l = target_size -> s
        | l when l < target_size -> s ^ (String.make (target_size - l) ' ')
        | l -> (String.sub s 0 (target_size-3)) ^ "..." in
    let display_all bits = 
        let bit_len = function (s, c) -> min max_user_len @@ String.length s in
        let lengths = List.map bit_len bits and
            num_bits = List.length bits in
        let max_len = List.fold_left max 0 lengths and
            bit_width = (width / (max 2 num_bits)) - 2 in
        if bit_width < max_len then None else

        let slack = width - (num_bits * bit_width) in
        let string_of_bit = function (s, c) -> colorize c @@ " " ^ (resize bit_width s) ^ " " in
        
        (String.concat "" @@ List.map string_of_bit bits) ^ (String.make slack ' ') 
        |> Option.some in

    List.find_map display_all (prefixes bits) |> Option.value ~default:""
    |> (fun x -> [x])

let print_lines lines = (* Print 25x 80-column lines *)
    let print_line i line =
        print_string (Printf.sprintf "\027[%d;0H%s" (i+1) line) in
    List.iteri print_line lines; flush stdout

let display_debug (state: state) (local_state: local_state) : unit =
    let viewport = viewport state local_state and
        visible = in_viewport (viewport state local_state) and
        text_width = (avail_cols local_state.terminal_size) in
    let color p = any [
        lookup_cursor_color state.per_user p;
        if visible p then Some Blue else None
    ] in

    let lines = [""] @ display_document text_width state.text color true
    @ [debug_cursors state ^ debug_view viewport state local_state]
    @ display_help text_width in

    print_string (String.concat "\n" lines); flush stdout

let display (state: state) (local_state: local_state) : unit =
    let viewport = viewport state local_state and
        color = lookup_cursor_color state.per_user and
        text_width = avail_cols local_state.terminal_size and
        status_width = status_width local_state.terminal_size and
        height = viewport_height local_state.terminal_size in
    let document_lines = display_viewport text_width state.text color viewport false 
        |> List.map (fun x -> x ^ String.make (status_width - text_width) ' ') in
    let lines = 
          title_line status_width state.document_name
        @ document_lines
        @ spacer_lines status_width (max (height - (List.length document_lines)) 0)
        @ error_line status_width local_state.error
        @ status_line status_width state local_state
        @ display_help status_width in

    print_lines lines
