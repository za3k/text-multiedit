(* Three Views of a Document
*  =========================
*
* A document is an immutable string. All documents end in a newline (and 
* therefore are at least 1 character long).
* 
* Cursors always point at a character, never between characters. Cursors can
* validly point at any character in the document, including newlines. They
* cannot go past the beginning/end character in a document.
* 
* Example (newlines written as 'N')
* 
*     ==POS view==
*     0 0123456789
*     1           012345
*      "Line 0NLine 1NNN"
*         ^          ^^^
* 
*     ==LINE/COL view==
*     Line 0N
*       ^     This is line 0, col 2 == pos 2
*     Line 1N
*           ^ This is line 1, col 6 == pos 13
*     N
*     ^       This is line 2, col 0 == pos 14
*     N
*     ^       This is line 3, col 0 == pos 15
*
*     ==S-LINE/S-COL view (width 3)==
*     Lin
*       ^       s-line 0, col 2
*     e 0       
*
*     N
*
*     Lin
*
*     e 1
*
*     N         s-line 5, col 0
*     ^
*     N         s-line 6, col 0
*     ^
*     N         s-line 7, col 0
*     ^
* 
* The three views are:
* 
*  - the POS view           [POS]2
*     - POS counts characters from start of document.
*     - POS 0 is the first character in the document.
*  - the LINE/COL view:     L0C2    
*     - LINE/COL view is based on "physical" lines within the document based
*       only on newline characters. It doesn't depend on word wrap, terminal
*       size, or viewport position.
*     - LINE 0 is the first line in a document, and LINE 0, COL 0 is the first
*       character in a document.
*     - The newline is considered the last character in each line. Remember
*       that the last line in a document also ends in a newline.
*     - LINE N, COL M is the same as POS(Nth newline position + M+1 more chars)
*  - the SLINE/SCOL view:   0      + SL 0 + SC 2
*                           L0[C0] + SL 0 + SC 2
*     - the SLINE/SCOL view is based on "screen" lines, taking into account
*       word wrap but not the viewport. 
*       In other words, how would the document look if it was wrapped to 80
*       columns, but infinitely tall?
*     - While you could count S-LINE from the top of the document, this means
*       that a pointer to (ex.) SLINE 50 would not the be same across users,
*       would become invalid if the terminal width changed, and generally is
*       not very stable in the face of editing.
*       Therefore, we store as deltas from something more stable:
*         - Given a POS, we can treat it as an s-line containing that POS value
*           [ignore or zero the s-col value]. 
*           This is how we store where the top of the screen should be ('view')
*         - Given two POS values, what is the delta of S-LINES and S-COLS
*           between them?         
*           0      + SL 0 + SC 2
*         - Given a LINE and a POS, what is the delta of S-LINES and S-COLS
*           between them?       
*           L0[C0] + SL 0 + SC 2
*       Additionally, we avoid storing even these deltas anywhere permanent.
* 
* POS should always be between 0 and LEN-1 (inclusive) -- 0 to 15 in the example
* POS functions accept values outside this range but clip them. Returned values
* will be in this range.
*     Exception: For convenience, [nth_line_start NEWLINES] returns [LEN],
*     which is not a valid input LINE or a valid output POS
* 
* LINE should be between 0 and NEWLINES-1 (inclusive) -- 0 to 3 in the example
* COL should be between 0 and the length of the line (excluding any newline
* before the line, including the one at the end of the line). In example line
* 1, COL could range from 0 to 6.
* LINE/COL functions accept values outside these ranges and clip them. Returned
* values will be in these ranges.
*)

let clamp (x1:int) (x2:int) (x:int) : int = min (max x x1) x2
let document_end (s: string) : int = (String.length s)-1
let document_clamp (s: string) : int->int = clamp 0 (document_end s)

let string_count (s:string) (c: char) : int =
    String.fold_left (fun acc x -> if x = c then acc + 1 else acc) 0 s

let nth_char (c: char) (s: string) (n: int) : int = 
    (* [nth_char s c n] is the index of the n-th copy of [c] in [s].
    Raises [Not_found] if there are less than [n] copies of [c] in [s]. *)
    if n <= 0 then 0 else
    let rec helper (offset: int) : int->int = function
        | 0 -> offset
        | n -> helper (String.index_from s (offset+1) c) (n-1)
    in helper ~-1 n
(* [nth_line_start] happily accepts one line past the end of the document, and
returns an index one past the end of the document. *)
let nth_line_start s n = 
    if n = 0 || (n = 1 && s = "\n") then 0
    else 1 + nth_char '\n' s n 

let line_of (text : string) (pos : int) : int * int =
    (* [line_of text pos] is the line and column number of the [pos]-th byte in
    [text].
    All indices are from 0.*)
    let rec helper (pos: int) (lines_before_offset: int) (offset: int) =
        (* Invariant: offset is first character after a newline OR first char
         * in file *)
        match String.index_from_opt text offset '\n' with
            | Some i when i < pos -> helper pos (lines_before_offset+1) (i+1)
            | _ -> (lines_before_offset, pos-offset)
    in helper (document_clamp text pos) 0 0

let line_for (text : string) (pos : int) : int * int =
    (* [line_for text pos] is the start and end of the line containing the
       [pos]-th byte in [text]. *)
    (* TODO: Optimize *)
    let (line_num, col) = line_of text pos in
    let start = nth_line_start text line_num in
    let end_line = ~-1 + nth_line_start text (line_num+1) in
    (start, end_line)

let pos_of (text: string) (line: int) (col: int) : int =
    (* [pos_of text line col] is the byte index of the [col]-th byte in the
    [line]-th line of [text].
    All indices are from 0.*)
    let max_line_num = (string_count text '\n')-1 in 
    match line with
        | n when n<0 -> 0
        | n when n>max_line_num -> document_end text
        | _ ->
            let line_start = nth_line_start text line in
            let next_line_start = nth_line_start text (line+1) in
            let line_length = next_line_start-line_start in (* includes trailing newline *)
            line_start + clamp 0 (line_length-1) col

(* S-line/s-col functions *)
type sline_delta = int * int

let wholeline_sline_index (width: int) (line_start: int) (pos: int) : int * int =
    (* [pos] is located within the s-line with index [wholeline_sline_index width
    line_start pos]. [line_start] is the index of the first character in the
    physical line containing [pos].
       The first s-line is sline 0. *)
    ((pos - line_start) / width, (pos - line_start) mod width)
let sline_index (width: int) (text: string) (pos: int) : int * int =
    let (line_start, _) = line_for text pos in
    wholeline_sline_index width line_start pos

let num_slines_in_line (width: int) (first: int) (last: int) : int = 
    (last - first) / width + 1
let num_slines_in_lines (width: int) (text: string) (first: int) (last: int) : int =
    (* Count the number of s-lines between [first] (at the beginning of a
    physical line)
       and [last]  (at the end of a physical line--some \n) *)
    let rec helper (first: int) (counted: int) : int =
        if first > last then counted else
        let (_, endl) = line_for text first in
        helper (endl+1) (counted + num_slines_in_line width first endl)
    in helper first 0

let sline_for (width: int) (text: string) (pos: int) : int * int =
    let (_, col) = sline_index width text pos
    and (_, line_end) = line_for text pos in

    let start = pos - col in
    let end_ = min (start + width - 1) line_end in
    (start, end_)
let sline_length (width: int) (text: string) (pos: int) : int =
    let (s,e) = sline_for width text pos in
    e-s+1

let rec sline_difference (width: int) (text: string) (pos1: int) (pos2: int) : sline_delta =
    (* To go from [pos1] to [pos2], how many rows and columns must you add? *)
    if (pos1 > pos2) then let (a, b) = 
        (sline_difference width text pos2 pos1) in (-a, -b) else
    let (start1, end1) = line_for text pos1
    and (start2, end2) = line_for text pos2 in
    let total_slines = num_slines_in_lines width text start1 end2
    and line2slines  = num_slines_in_lines width text start2 end2 in
    let (pos1sline, pos1col) = wholeline_sline_index width start1 pos1
    and (pos2sline, pos2col) = wholeline_sline_index width start2 pos2 in
    (*
    --o++                               -- = A = pos1sline[2]
    +++++
    +++++
    +o--                                -- = B = line2slines[4] - pos2sline[1] - 1

                -- o ++..++ o -- = total_slines[5+5+5+4]
    (subtract)  -- o        o -- = A + 1 + B
    (equals)         ++..++ o    = difference 
                                 = total_slines - A - B - 1
                                 = total_slines - pos1sline - line2slines + pos2sline

    --o+++o--- (one-line case)   = pos2sline - pos1sline
                                   (total_slines = line2slines)
                                 = total_slines - pos1sline - line2slines + pos2sline

    Note: if both ends are in the same sline, the result should be 0, which is not really
    covered by the above logic (but 
    *)
    (total_slines - pos1sline - (line2slines - pos2sline), pos2col - pos1col)

let sline_add_whole (width: int) (text: string) (sline_start: int) (delta_slines1: int) : int =
    (* Given [sline_start] which is the start of some s-line, move an integer
    number of s-lines in either direction. Clip to the document boundries.
    If the result is off the top of the document, [0] is returned.
    If the result is off the bottom of the document, [String.length text] is returned (one past the final valid index).
    *)
    let (start1, end1) = line_for text sline_start in
    let (sline_num, col) = sline_index width text sline_start in
    if col <> 0 then -999 else
    let rec helper pos delta_slines = (* POS is the start of a physical line *)
        if delta_slines < 0 then
            (* Move left 1 physical line from pos (or less) *)
            if pos = 0 then 0 else (* Don't go off the start of the document *)
            let (start1, end1) = line_for text (pos-1) in
            helper start1 (delta_slines + num_slines_in_line width start1 end1)
        else 
            (* Move right 1 physical line from pos (or less) *)
            let (start1, end1) = line_for text pos in
            let num_slines = num_slines_in_line width start1 end1 in
            if delta_slines < num_slines then pos + delta_slines * width
            else if end1 >= (document_end text) then (String.length text) (* Only go 1 off the end of the document *)
            else helper (end1+1) (delta_slines - num_slines)
    in helper start1 (delta_slines1 + sline_num)

let sline_add (width: int) (text: string) (pos: int) (delta: sline_delta) : int =
    (* Given the index [pos] into [text], move [delta] columns and rows from that index. The row is adjusted first, then the column.
    If the result is off the top of the document, [0] is returned.
    If the result is off the bottom of the document, [String.length text] is returned (one past the final valid index).
    If the result is off the left or right side of an s-line, it is silently clipped (not wrapped around).
    *)
    let (delta_slines, delta_scols) = delta
    and (start1, end1) = line_for text pos in
    let (_, scol1) = wholeline_sline_index width start1 pos
    and (sline_start, _) = sline_for width text pos in
    let final_sline_start = sline_add_whole width text sline_start delta_slines in
    if final_sline_start >= (String.length text) then final_sline_start else
    final_sline_start + clamp 0 ((sline_length width text final_sline_start) - 1) (delta_scols + scol1)
