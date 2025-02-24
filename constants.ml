let version = "0.0.1"

let max_rows = 80

let editor_colors = let open Color in
    [Red;Green;Yellow;Blue;Magenta;Cyan;White;BrightBlack;BrightRed;BrightGreen;BrightYellow;BrightBlue;BrightMagenta;BrightCyan;BrightWhite]

let shortcuts : (string list * (string*string) option * Types.button) list = [
    (* Shortcuts are here listed in priority order of how to show them in the help *)
    (* input-bytes list / Help Display / enum-name *)
    (["\024"; "\022"], Some ("C-x", "Exit"), Exit); (* Ctrl-X or Ctrl-C. Note that Ctrl-C just exits with SIGINT currently. *)
    (["\019"], Some ("C-s", "Save"), Save); (* Requires ~IXON terminal setting *)
    (["\021"], Some ("C-u", "Paste"), Paste);
    (["\011"], Some ("C-k", "Cut"), Cut);
    (["\027[1;3B"], Some ("A-↓", "Scroll Down"), ScrollDown); (* Alt-Down *)
    (["\027[1;3A"], Some ("A-↑", "Scroll Up"), ScrollUp); (* Alt-Up *)
    (*
    (["\007"], Some ("C-g", "Help"), Help); (* Ctrl-G because Ctrl-H is backspace *)
    (["\n"], Some ("C-j", "Justify"), Justify); (* Requires ~ICRNL terminal setting to tell apart from enter *)
    *)

    (["\027[6~"], None, PageDown);
    (["\027[5~"], None, PageUp);
    (["\027[1~"; "\027[H"], None, Home);
    (["\027[4~"; "\027[F"], None, End);
    (["\027[A"], None, Up);
    (["\027[B"], None, Down);
    (["\027[C"], None, Right);
    (["\027[D"], None, Left);
    (["\r"], None, Key '\n'); (* Enter *)
    (["\t"], None, Tab);
    (["\b"; "\127"], None, Backspace);
    (["\027[3~"], None, Del); 
]
