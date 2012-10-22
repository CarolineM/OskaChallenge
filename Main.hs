--Caroline McQuatt 
--36959104
--Peter Zhang


-- TODO depth first search
oska_x1y2 :: [String] -> Char -> Int -> [String]
oska_x1y2 board side m = board


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
		old_cur_row			= board !! cur_r
		new_cur_row			= replace old_cur_row cur_c 0 '-'
		old_move_row		= board !! move_r 
		piece 				= old_cur_row !! cur_c
		new_move_row		= replace old_move_row move_c 0 piece
		old_jump_row		= board !! jump_r
		new_jump_row		= replace old_jump_row jump_c 0 '-'

replace :: [a] -> Int -> Int -> a -> [a]
replace arr index cnt rep
	| null arr 				= arr
	| index == cnt			= rep : (tail arr)
	| otherwise				= (head arr) : (replace (tail arr) index (cnt + 1) rep)
