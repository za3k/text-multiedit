TEXTMU - The complete list

[x] Get a standalone binary (which will work on tilde)
[ ] Write a list of things to simplify in the code
[ ] Refactor pass - simpligy each of those things
[ ] Fix all known bugs
[ ] Up/down should remember the "imaginary" column off the right end the cursor is on until the user types or presses left/right
[ ] Cutting at the end of the file appends "\n" to the clipboard, and shouldn't
[ ] Display
	[x] Fork two display modes - debug (done) and regular (clone the current one)
	[ ] Display only visible lines
	[ ] KEEP line numbers (and make them all the same width)
	[ ] Completely rewrite the screen each update
			[ ] Avoid flicker -- During display, write each line separately (blanking as needed)
	[ ] Remove s-line numbers
	[ ] Add the line of users
	[ ] Add 2 help lines
	[ ] Add an error message display
	[/] Reset the screen 'on\_exit'
	[ ] Improve background/foreground color pairs
[ ] In-Channels 
	[ ] Write them (file-backed, reads a specific type, notes whether it's ready to read or not)
	[ ] Add a way to listen for a list of In-Channels to be ready
[ ] Out-Channels
	[ ] Write them (file-backed, writes a specific type)
[ ] Write a real server, but that sends back simple dummy data and assumes there's one connection at a time
[ ] Write the real client logic
	[ ] Listen to first of (network, keyboard, SIGWINCH) using In-Channels
	[ ] Add keyboard locking/unlocking, Don't listen to keyboard while locked
[ ] Write the real server logic
	[ ] Add document setup messages when a user connects
	[ ] Add file load+save
	[ ] Add message queues for each user
	[ ] Stand-alone mode: Pass in a condition variable to the server, which is triggered when the user is ready
[ ] Scan the code and fix every place an exception can possibly be throwing, removing them one at a time
[ ] Upload the finished binary to tilde as /bin/text
[ ] Test multi-user editing
[ ] Fix any bugs found
[ ] Write a list of things to simplify in the code
[ ] Refactor pass - simpligy each of those things
[ ] Post about 'text' to blogsuchin
[ ] Post about 'text' to tilde chat
[ ] Post about 'text' to my own blog
[ ] You are done!

FUTURE FEATURES
[ ] Read-only mode
[ ] Line numbers, go to line
[ ] Built-in help
[ ] Justify
[ ] Auto-save
[ ] Shift-arrows to highlight text
[ ] Multiple columns
