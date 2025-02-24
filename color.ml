type t = 
    | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White 
    | BrightBlack | BrightRed | BrightGreen | BrightYellow | BrightBlue | BrightMagenta | BrightCyan | BrightWhite

let color_code : t -> string = function
    | Black   -> "40m"    | BrightBlack   -> "100;30m" 
    | Red     -> "41;30m" | BrightRed     -> "101;30m" 
    | Green   -> "42;30m" | BrightGreen   -> "102;30m" 
    | Yellow  -> "43;30m" | BrightYellow  -> "103;30m" 
    | Blue    -> "44;97m" | BrightBlue    -> "104;97m" 
    | Magenta -> "45;30m" | BrightMagenta -> "105;30m" 
    | Cyan    -> "46;30m" | BrightCyan    -> "106;30m" 
    | White   -> "47;30m" | BrightWhite   -> "107;30m"

let colorize (color: t) : string -> string =
    Printf.sprintf "\027[%s%s\027[0m" (color_code color)
