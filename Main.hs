--Caroline McQuatt 
--Peter Zhang


--TODO depth first search
oska_x1y2 :: [String] -> Char -> Int -> [String]
oska_x1y2 board side m = board



--does not check for valid starting indexes or tile, must be correct
isLegalMove :: [String] -> Int -> Int -> Int -> Int -> Char -> Bool
isLegalMove board cur_r cur_c move_r move_c side
	| (not row_exists) || (not col_exists)	= False
	| (distance == 1) && cur_r == mid_row	= mv_tile == '-' && (isLegalMid cur_c move_c False)
	| (distance == 1)						= valid_col_reg && mv_tile == '-'
	| (distance == 2) && 
		(getJumpRow side cur_r) == mid_row	= (mv_tile == '-') &&
												(not ((jmp_tile == side) || (jmp_tile == '-'))) &&
													(isLegalMid cur_c move_c True)
	| (distance == 2)						= valid_col_jmp && (mv_tile == '-') &&
												(not ((jmp_tile == side) || (jmp_tile == '-')))
	| otherwise								= False
	where
		mid_row				= div ((length board) - 1) 2 
		distance			= abs (cur_r - move_r)
		row_exists			= (move_r >= 0) && (move_r < (length board)) 
		col_exists			= (move_c >= 0) && (move_c < (length (getRow board move_r))) 
		valid_col_reg		= (move_c == cur_c) || (move_c == (cur_c - 1))
		valid_col_jmp		= (move_c == cur_c) || (move_c == (cur_c - 2)) 
		mv_tile				= getTile board move_r move_c
		jmp_tile			= if (isJumpMid cur_r mid_row side) then
								getTile board (getJumpRow side cur_r)  (getJTileColMid cur_c move_c)
								else
									getTile board (getJumpRow side cur_r) (getJTileColNorm cur_c move_c)

--jump_r and jump_c should be negative if no jump
genMove :: [String] -> Int -> Int -> Int -> Int -> Int -> Int -> [String]
genMove board cur_r cur_c move_r move_c jump_r jump_c = do
	if ((jump_c <= 0) || (jump_r <= 0)) then
		(replace 
			(replace board cur_r 0 new_cur_row) move_r 0 new_move_row)
		else
			(replace 
				(replace 
					(replace board cur_r 0 new_cur_row) 
					move_r 0 new_move_row)
				jump_r 0 new_jump_row)
	where
		old_cur_row			= getRow board cur_r
		new_cur_row			= replace old_cur_row cur_c 0 '-'
		old_move_row		= getRow board move_r 
		piece 				= old_cur_row !! cur_c
		new_move_row		= replace old_move_row move_c 0 piece
		old_jump_row		= board !! jump_r
		new_jump_row		= replace old_jump_row jump_c 0 '-'

number_pieces :: [String] -> Char -> Int
number_pieces board player
	| null board							= 0
	| otherwise								= (count_in_row (head board) player) + (number_pieces (tail board) player)

--helpers-------------------------------------------------------------------------

count_in_row :: String -> Char -> Int
count_in_row row player
	| null row 								= 0
	| (head row) == player					= 1 + count_in_row (tail row) player
	| otherwise								= count_in_row (tail row) player

--Helper: assumes row and cols exist
isLegalMid :: Int -> Int -> Bool -> Bool 
isLegalMid cur_c move_c is_jmp 
	| is_jmp && cur_c == 0 					= move_c == 1
	| is_jmp && cur_c == 1					= (move_c == 0) || (move_c == 2)
	| is_jmp && cur_c == 2					= move_c == 1
	| cur_c == 0							= (move_c == 0) || (move_c == 1)
	| cur_c == 1							= (move_c == 2) || (move_c == 1)
	| otherwise 							= False

--Helper
isJumpMid :: Int -> Int -> Char -> Bool
isJumpMid cur_r mid_row side = 
	(((cur_r - 1 == mid_row) && side == 'w') || ((cur_r - 1 == mid_row) && side == 'b'))


--Helper: assumes that the jump is legal
getJumpRow :: Char -> Int -> Int
getJumpRow side cur_r =
	if side == 'b' then
		(cur_r - 1) 
		else
			(cur_r + 1)					

--Helper: assumes that the jump is legal
getJTileColNorm :: Int -> Int -> Int
getJTileColNorm cur_c move_c
	| cur_c == move_c						= cur_c
	| (cur_c - 2) == move_c					= (cur_c - 1)
	| otherwise								= (-1)

--Helper: assumes that the jump is legal
getJTileColMid :: Int -> Int -> Int
getJTileColMid cur_c move_c
	| cur_c == 0							= 1
	| cur_c == 1 && move_c == 0				= 0
	| cur_c == 1 && move_c == 2				= 1
	| cur_c == 2							= 1
	| otherwise								= (-1)

replace :: [a] -> Int -> Int -> a -> [a]
replace arr index cnt rep
	| null arr 				= arr
	| index == cnt			= rep : (tail arr)
	| otherwise				= (head arr) : (replace (tail arr) index (cnt + 1) rep)

getTile :: [[a]] -> Int -> Int -> a
getTile board row col = 
	(getRow board row) !! col

getRow :: [a] -> Int -> a
getRow board row = 
	board !! row
