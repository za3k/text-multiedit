TEXTMU - The complete list

[x] Get a standalone binary (which will work on tilde)
[x] Move debug-only code into debug.ml (printing)
[x] Move pos/line/sline logic into a separate file.
[x] Cutting at the end of the file appends "\n" to the clipboard, and shouldn't
[x] Display
	[x] Fork two display modes - debug (done) and regular (clone the current one)
	[x] Reset the screen 'on\_exit'
	[x] Display only visible lines
	[x] KEEP line numbers (and make them all the same width)
	[x] Completely rewrite the screen each update
    [x] Avoid flicker -- During display, write each line separately (blanking as needed)
	[x] Remove s-line numbers
	[x] Add 2 help lines
	[x] Improve background/foreground color pairs
    [x] Add title bar
	[x] Add the line of users
	[x] Add an error message display
[x] In-Channels 
	[x] Write them (file-backed, reads a specific type, notes whether it's ready to read or not)
	[x] Add a way to listen for a list of In-Channels to be ready
[x] Out-Channels
	[x] Write them (file-backed, writes a specific type)
[x] Write a real server, but that sends back simple dummy data and assumes there's one connection at a time
[x] Write the real client logic
	[x] Listen to first of (network, keyboard, SIGWINCH) using In-Channels
	[x] Add keyboard locking/unlocking, Don't listen to keyboard while locked
[ ] Write the real server logic
    [x] Support multiple connections
	[x] Stand-alone mode: Pass in a condition variable to the server, which is triggered when the user is ready
    [x] Make sure the server PRINTS AN ERROR MESSAGE if it crashes with an exception--this appears to not be the default! Ex: Sys.remove "non-existent file"
    [x] Send messages to all users, not just one
	[x] Add message queues for each user
    [x] Assign user colors
	[x] Add document setup messages when a user connects
    [ ] Have a dictionary of documents -- look up or create one on demand
	[ ] Add file load+save
[x] Fix bug where the blank document doesn't show a cursor or the line number
[ ] Terminal resizing
    [x] Listen for terminal resize
    [ ] Detect the terminal size -- requires C, punt this (see: https://github.com/cryptosense/terminal_size/blob/master/src/terminal_size_stubs.c)
    [ ] Change the terminal size
[ ] Add a "colored string" type to simplify display + padding logic
[ ] Refactor pos/line/sline logic
	- Move pos/line/sline logic into a separate module.
	  Abstract over text ranges and positions. 

	  [ Implicitly, all functions take 'text' 'width' as the first two arguments ]
	  [ Implicitly, a position is only valid for a fixed Text+Width ]
	  position = int 						Or does it also include an imaginary column? HMM?
	  width = int
	  range = (position, position)
	  line_delta = (int, int)
	  sline_delta = (int, int)
	  line = range
	  sline = range

	  Position.from_pos int -> position   From a pos, get a position. [Internally, just store it -- pos is the canonical internal format]
	  Position.to_pos position -> int     From a position, get a pos.
	  Range.make string -> range	      From two positions, get a range
	  Range.ends range -> (position * position)			From a range, get the start and end position
	  Position.shift_chars position int -> position		From a position, add/subtract <n> characters -- use this to adjust cursors and views for actions
	  Position.get_line position -> line				From a position, get a LINE or SLINE (which is/has a text range)
	  Position.get_sline position -> sline
	  Line.get_range line -> range
	  Sline.get_range sline -> range

	  Sline.Delta.make int int -> sline_delta
	  Sline.Delta.lines sline_delta -> int
	  Sline.Delta.cols sline_delta -> int

	  Line.Delta.make int int -> line_delta
	  Line.Delta.lines line_delta -> int
	  Line.Delta.cols line_delta -> int

	  Sline.shift position sline_delta -> position		From a position, adjust by LINES+COLS or SLINES+SCOLS and get a new position.
	  Line.shift position line_delta -> position

	  Sline.diff position position -> sline_delta       From two positions, calculate the LINES+COLS difference, or the SLINES+SCOLS difference.
	  Line.diff position position -> line_delta

	  Line.number line -> int							Get the line number of a LINE
	  Sline.number_in_line sline -> int					Get the s-line number of a SLINE within its line

	  Range.replace [text] [width] range string -> text	Provide a function to replace a text range with new text
	  Range.get [text] [width] range -> string			Provide a function to get a text range within text

	  Position.remap_on_replace [text] [width] range string position -> position	Shift cursors/view when text changes
	  Position.remap_on_resize [text] [width] width position -> position			This actually just returns position (since we use pos representation)
	  
	  Sline.get_line sline -> line						Do we need these?
	  Line.get_nth_sline line int -> sline
	  													Do we need to represent "after the end of the document" or 
[ ] Fix all known bugs
	[ ] Up/down should remember the "imaginary" column off the right end the cursor is on until the user types or presses left/right
[ ] Scan the code and fix every place an exception can possibly be throwing, removing them one at a time
[ ] Upload the finished binary to tilde as /bin/text
[ ] Test multi-user editing
[ ] Fix any bugs found
[ ] Write a list of things to simplify in the code
[ ] Refactor pass - simpligy each of those things
[ ] Post about 'text' to blogsuchin
[ ] Post about 'text' to tilde chat
[ ] Post about 'text' to my own blog
[ ] Add a documentation patch to close https://github.com/ocaml/ocaml/issues/13822 (Printexc backtraces)
[ ] You are done!

FUTURE FEATURES
[ ] Read-only mode
[ ] Line numbers, go to line
[ ] Built-in help
[ ] Justify
[ ] Auto-save
[ ] Shift-arrows to highlight text
[ ] Multiple columns
