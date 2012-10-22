OskaChallenge
=============

Notes:
- white moves first
- must move forward and diagonal
- can jump the other player only once
- if no legal move, pass back the current board
- middle of the board has two squares, so depth is 2n - 3
- rows are indexs in the board list 0 to X
- columns refer to an index in a specific row (or string), so column numbers change depending on the row

Wining states:
- all opponents pieces are remove from the board
- all your pieces make it to the oposite side

Test boards:
["www-","--w","--","---","bbbb"]
["www-","---", "-w", "-b-", "b-bb"]  