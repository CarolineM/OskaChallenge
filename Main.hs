--Caroline McQuatt 
--Peter Zhang
data Tree a =
         Branch { 
            board :: [String], 
            children :: [Tree [String]]
            } 
    deriving (Eq, Show)


oska_p1t7 :: [String] -> Char -> Int -> [String]
oska_p1t7 board player moves = 
        analyze_board_p1t7 (Branch board (statesearch_p1t7 [board] moves player)) player
        --(Branch board (statesearch [board] moves player))


statesearch_p1t7 :: [[String]] -> Int -> Char -> [Tree [String]]
statesearch_p1t7 unexplored moves player
	| null unexplored || moves == 0		= []
	| not (null newstates)				= (map (\x -> (Branch x (statesearch_p1t7 (genNewStates_p1t7 x player) (moves - 1) next_player))) newstates)
	| otherwise							= statesearch_p1t7 (tail unexplored) (moves - 1) next_player
		where 
			newstates		= (genNewStates_p1t7 (head unexplored) player)
			next_player		= if (player == 'w') then
			  					'b'
			  					else
			  						'w'

genNewStates_p1t7 :: [String] -> Char -> [[String]]
genNewStates_p1t7 board player = genNewStatesHelper_p1t7 board player (findTiles_p1t7 board player 0 0)


genNewStatesHelper_p1t7 :: [String] -> Char ->  [(Int,Int)]-> [[String]]
genNewStatesHelper_p1t7 board side locs 
        | locs == [] = []
        | otherwise  = (filter (\x -> (not (null x))) (genNewStatesforPiece_p1t7 board (fst (head locs)) (snd (head locs)) 0)) ++ (genNewStatesHelper_p1t7 board side (tail locs))


-- assumes the first list inside board is 0 row
-- findTiles maze1 'w' 0 0
-- returns [(0,0),(1,0),(2,0),(2,1)]
findTiles_p1t7 :: [String] -> Char -> Int -> Int -> [(Int, Int)]
findTiles_p1t7 board side countx county 
        | board == [] = []
        | otherwise = (findTilesOnRow_p1t7 (head board) side 0 county) ++ (findTiles_p1t7 (tail board) side 0 (county+1))
        
findTilesOnRow_p1t7 :: String-> Char -> Int -> Int -> [(Int,Int)]
findTilesOnRow_p1t7 row side countx y
        | row == [] = []
        | (head row) == side = (countx, y) : (findTilesOnRow_p1t7 (tail row) side (countx+1) y)
        | otherwise = (findTilesOnRow_p1t7 (tail row) side (countx+1) y)

-- generate new states for a given piece at given location c and r
-- genNewStatesforPiece maze1 0 2 0
-- returns [["ww--","-ww","--","---","bbbb"]]
genNewStatesforPiece_p1t7 ::[String] -> Int -> Int ->  Int -> [[String]]
genNewStatesforPiece_p1t7 board c r count_r
        | (length board) == count_r         = []
        |  otherwise                        = if (abs r - count_r) > 2 then
                                                (genNewStatesforPiece_p1t7 board c r (count_r+1))
                                                else
                                                    (movesForPieceAtRow_p1t7 board count_r 0 r c) ++ (genNewStatesforPiece_p1t7 board c r (count_r+1))



-- get possible moves for a single piece at given row
-- the col parameter is a counter, so always start at 0
movesForPieceAtRow_p1t7 :: [String] -> Int -> Int -> Int -> Int -> [[String]]
movesForPieceAtRow_p1t7 board row col piece_r piece_c 
        | ((length (getRow_p1t7 board row))) == col                 = []
        | otherwise                                                 = newstate : (movesForPieceAtRow_p1t7 board row (col+1) piece_r piece_c)
        where
                side     = (getTile_p1t7 board piece_r piece_c)
                newstate = (do if (2 == (row-piece_r) || ((-2) == (row-piece_r))) 
                                then (makeJump_p1t7 board piece_r piece_c row col side) 
                                else (do if (isLegalMove_p1t7 board piece_r piece_c row col side) 
                                           then (genMove_p1t7 board piece_r piece_c row col (-111) (-111)) 
                                           else []))



analyze_board_p1t7 :: Tree [String] -> Char -> [String]
analyze_board_p1t7 root side =
	if null (children root) then
		(board root) -- means no available moves, so return root
		else
			getboard_p1t7 branch_scores max_score				
		where 
			branch_scores	= map (\x -> (total_branch_p1t7 x side)) (children root) 
			max_score 		= maximum (snd (unzip branch_scores))

getboard_p1t7 :: [([String],Int)] -> Int -> [String]
getboard_p1t7 branch_scores max_score
	| null branch_scores			= []
	| (snd (head branch_scores)) == max_score 
									= (fst (head branch_scores))
    | otherwise						= getboard_p1t7 (tail branch_scores) max_score

total_branch_p1t7 :: Tree [String] -> Char -> ([String], Int)
total_branch_p1t7 b_root side = 
    if null (children b_root) then
        ((board b_root), totalboard_p1t7 (board b_root) side)
        else
	       ((board b_root), (head (totalboards_p1t7 (children b_root) next_player side)))
	where
		next_player 			= if side == 'w' then
										'b'
										else
											'w'

totalboards_p1t7 :: [Tree [String]] -> Char -> Char -> [Int]
totalboards_p1t7 lot lastmoved maxplayer
	| null lot						= []
    | null (children (head lot))    = totalboard_p1t7 (board (head lot)) maxplayer : (totalboards_p1t7 (tail lot) lastmoved maxplayer)
    | lastmoved == maxplayer        = minimum (totalboards_p1t7 (children (head lot)) next_player maxplayer) : (totalboards_p1t7 (tail lot) lastmoved maxplayer)
	| otherwise						= maximum (totalboards_p1t7 (children (head lot)) next_player maxplayer) : (totalboards_p1t7 (tail lot) lastmoved maxplayer)								
		where 
			next_player 			= if lastmoved == 'w' then
										'b'
										else
											'w'

--static board analysis
totalboard_p1t7 :: [String] -> Char -> Int
totalboard_p1t7 board player
	| isWinningBoard_p1t7 board player							= 1000000
	| isWinningBoard_p1t7 board opponent						= (-100000)
	| one_left_case_p1t7 board opponent 						= (-1000)
    | otherwise                                                 = sum [0, (test_blocked_from_end_p1t7 (collumn_num_with_test_p1t7 edgerow opponent 0) (collumn_num_with_test_p1t7 endrow '-' 0)),
                                                                    (- (test_blocked_from_end_p1t7 (collumn_num_with_test_p1t7 edgerow player 0) (collumn_num_with_test_p1t7 endrow '-' 0))),
                                                                        (greater_num_players_p1t7 board player opponent), (num_player_in_end_p1t7 board player), 
                                                                        (- (num_player_in_end_p1t7 board opponent)), oneleft]    
		where 
            opponent	=	if player == 'w' then
  								'b'
  								else
  									'w' 
            endrow		=	if player == 'b' then
							 	(board !! 0)
								else
									(board !! ((length board) -1))
            edgerow		=	if player == 'b' then
							 	(board !! 1)
								else
									(board !! ((length board) -2))
            oneleft     =   if (one_left_case_p1t7 board player) then
                                60
                                else
                                    0

num_player_in_end_p1t7 :: [String] -> Char -> Int
num_player_in_end_p1t7 board player =
    ((count_in_row_p1t7 row player) * 30)
    where row           = if player == 'w' then
                            (board !! ((length board) - 1))
                            else
                                (head board)

greater_num_players_p1t7 :: [String] -> Char -> Char -> Int
greater_num_players_p1t7 board player opponent =
    difference * 20
            where
                difference = number_pieces_p1t7 board player - number_pieces_p1t7 board opponent

test_blocked_from_end_p1t7 :: (Eq a, Num a) => [a] -> [a] -> Int
test_blocked_from_end_p1t7 pindex openindex
    | (not (null pindex)) && (blocked_from_end_p1t7 pindex openindex) = 20
    | otherwise                                                       = 0

blocked_from_end_p1t7 :: (Eq a, Num a) => [a] -> [a] -> Bool
blocked_from_end_p1t7 pindex openindex
	| null pindex						= True
	| elem (head pindex) openindex || elem ((head pindex) + 1) openindex
										= False
	| otherwise 						= True && blocked_from_end_p1t7 (tail pindex) openindex

collumn_num_with_test_p1t7 :: (Eq a1, Num a) => [a1] -> a1 -> a -> [a]
collumn_num_with_test_p1t7 row test_value i
	| null row	   						= []
	| (head row) == test_value			= i : collumn_num_with_test_p1t7 (tail row) test_value (i + 1)
	| otherwise							= collumn_num_with_test_p1t7 (tail row) test_value (i + 1)				

one_left_case_p1t7 :: [String] -> Char -> Bool
one_left_case_p1t7 board player =
  number_pieces_p1t7 cut_board player == 1 &&
  player_in_row_p1t7 (offensive_rows_p1t7 board player) board player
  		where
  			cut_board	=	if player == 'w' then
  								(init board)
  								else
  									(tail board)

player_in_row_p1t7 :: [Int] -> [String] -> Char -> Bool
player_in_row_p1t7 lor board player
	| null lor			= False
	| otherwise			= elem player (board !! (head lor)) || player_in_row_p1t7 (tail lor) board player

offensive_rows_p1t7 :: [String] -> Char -> [Int]
offensive_rows_p1t7 board player =
	if player == 'w' then
		[(mid+1)..(length board - 2)]
		else
			[(mid-1)..1]
		where
			mid 	=	div ((length board) - 1) 2


--does not check for valid starting indexes or tile, must be correct
isLegalMove_p1t7 :: [String] -> Int -> Int -> Int -> Int -> Char -> Bool
isLegalMove_p1t7 board cur_r cur_c move_r move_c side
        | (not row_exists) || (not col_exists)            = False
        | not (distance == 1)                             = False
        | side == 'w' && d_prime >= 0                     = False
        | side == 'b' && d_prime <= 0                     = False
        | cur_r == mid_row 
        	&& (isLegalMid_p1t7 cur_c move_c False)       = mv_tile == '-' 
        |  valid_col_reg && not (cur_r == mid_row)        = mv_tile == '-'
        | otherwise                                       = False
        where
                mid_row                      = div ((length board) - 1) 2
                d_prime                      = cur_r - move_r
                distance                     = abs d_prime
                row_exists                   = (move_r >= 0) && (move_r < (length board)) 
                col_exists                   = (move_c >= 0) && (move_c < (length (getRow_p1t7 board move_r))) 
                valid_col_reg                = (move_c == cur_c) || (move_c == (cur_c - 1))
                mv_tile                      = getTile_p1t7 board move_r move_c


--does not check for valid starting indexes or tile, must be correct
makeJump_p1t7 :: [String] -> Int -> Int -> Int -> Int -> Char -> [String]
makeJump_p1t7 board cur_r cur_c move_r move_c side
        | not (distance == 2)                                                                   = []
        | (not row_exists) || (not col_exists)                                                  = []
        | side == 'w' && d_prime >= 0                                                           = []
        | side == 'b' && d_prime <= 0                                                           = []
        | ((getJumpRow_p1t7 side cur_r) == mid_row) && (isLegalMid_p1t7 cur_c move_c True) &&
                ((mv_tile == '-') && 
                    (not ((getJumpTile_p1t7 board cur_r cur_c mid_row move_c side == side) 
                        || (getJumpTile_p1t7 board cur_r cur_c mid_row move_c side  == '-'))))  = genMove_p1t7 board cur_r cur_c move_r move_c (getJumpRow_p1t7 side cur_r) (getJTileColMid_p1t7 cur_c move_c) 
        | valid_col_jmp && not ((getJumpRow_p1t7 side cur_r) == mid_row) && 
                 ((mv_tile == '-') && 
                    (not ((getJumpTile_p1t7 board cur_r cur_c mid_row move_c side == side) || 
                        (getJumpTile_p1t7 board cur_r cur_c mid_row move_c side == '-'))))     = genMove_p1t7 board cur_r cur_c move_r move_c (getJumpRow_p1t7 side cur_r) (getJTileColNorm_p1t7 cur_c move_c) 
        | otherwise                                                                            = []
        where
                mid_row                      = div ((length board) - 1) 2 
                d_prime                      = cur_r - move_r
                distance                     = abs d_prime 
                row_exists                   = (move_r >= 0) && (move_r < (length board)) 
                col_exists                   = (move_c >= 0) && (move_c < (length (getRow_p1t7 board move_r)))
                valid_col_jmp                = (move_c == cur_c) || (move_c == (cur_c - 2)) 
                mv_tile                      = getTile_p1t7 board move_r move_c

getJumpTile_p1t7 :: [String] -> Int -> Int -> Int -> Int -> Char -> Char
getJumpTile_p1t7 board cur_r cur_c mid_row move_c side =
    if (isJumpMid_p1t7 cur_r mid_row side) then
        getTile_p1t7 board (getJumpRow_p1t7 side cur_r)  (getJTileColMid_p1t7 cur_c move_c)
              else
                getTile_p1t7 board (getJumpRow_p1t7 side cur_r) (getJTileColNorm_p1t7 cur_c move_c)

--jump_r and jump_c should be negative if no jump
genMove_p1t7 :: [String] -> Int -> Int -> Int -> Int -> Int -> Int -> [String]
genMove_p1t7 board cur_r cur_c move_r move_c jump_r jump_c = do
        if ((jump_c < 0) || (jump_r < 0)) then
                (replace_p1t7 
                        (replace_p1t7 board cur_r 0 new_cur_row) move_r 0 new_move_row)
                else
                        (replace_p1t7 
                                (replace_p1t7 
                                        (replace_p1t7 board cur_r 0 new_cur_row) 
                                        move_r 0 new_move_row)
                                jump_r 0 new_jump_row)
        where
                old_cur_row                 = getRow_p1t7 board cur_r
                new_cur_row                 = replace_p1t7 old_cur_row cur_c 0 '-'
                old_move_row                = getRow_p1t7 board move_r 
                piece                       = old_cur_row !! cur_c
                new_move_row                = replace_p1t7 old_move_row move_c 0 piece
                old_jump_row                = board !! jump_r
                new_jump_row                = replace_p1t7 old_jump_row jump_c 0 '-'

number_pieces_p1t7 :: [String] -> Char -> Int
number_pieces_p1t7 board player
        | null board                     = 0
        | otherwise                      = (count_in_row_p1t7 (head board) player) + (number_pieces_p1t7 (tail board) player)

--expects either 'b' or 'w' for player
isWinningBoard_p1t7 :: [String] -> Char -> Bool
isWinningBoard_p1t7 board player
        | player == 'w'                  = number_pieces_p1t7 board 'b' == 0 || checklast_p1t7 board 0 ((length board) - 1) 'w'
        | otherwise                      = number_pieces_p1t7 board 'w' == 0 || checklast_p1t7 board 0 0 'b'

--helpers-------------------------------------------------------------------------
checklast_p1t7 :: [String] -> Int -> Int -> Char -> Bool
checklast_p1t7 board i row player
        | null board                    = True
        | i == row                      = (elem player (head board)) && checklast_p1t7 (tail board) (i + 1) row player
        | otherwise                     =  not((elem player (head board))) && checklast_p1t7 (tail board) (i + 1) row player 


count_in_row_p1t7 :: String -> Char -> Int
count_in_row_p1t7 row player
        | null row                      = 0
        | (head row) == player          = 1 + count_in_row_p1t7 (tail row) player
        | otherwise                     = count_in_row_p1t7 (tail row) player

--Helper: assumes row and cols exist
isLegalMid_p1t7 :: Int -> Int -> Bool -> Bool 
isLegalMid_p1t7 cur_c move_c is_jmp 
        | is_jmp && cur_c == 0         = move_c == 1
        | is_jmp && cur_c == 1         = (move_c == 0) || (move_c == 2)
        | is_jmp && cur_c == 2         = move_c == 1
        | cur_c == 0                   = (move_c == 0) || (move_c == 1)
        | cur_c == 1                   = (move_c == 2) || (move_c == 1)
        | otherwise                    = False

--Helper
isJumpMid_p1t7 :: Int -> Int -> Char -> Bool
isJumpMid_p1t7 cur_r mid_row side = 
        (((cur_r + 1 == mid_row) && side == 'w') || ((cur_r - 1 == mid_row) && side == 'b'))


--Helper: assumes that the jump is legal
getJumpRow_p1t7 :: Char -> Int -> Int
getJumpRow_p1t7 side cur_r =
        if side == 'b' then
                (cur_r - 1) 
                else
                        (cur_r + 1)                                        

--Helper: assumes that the jump is legal
getJTileColNorm_p1t7 :: Int -> Int -> Int
getJTileColNorm_p1t7 cur_c move_c
        | cur_c == move_c     		= cur_c
        | (cur_c - 2) == move_c     = (cur_c - 1)
        | otherwise                 = (-1)

--Helper: assumes that the jump is legal
getJTileColMid_p1t7 :: Int -> Int -> Int
getJTileColMid_p1t7 cur_c move_c
        | cur_c == 0                    = 0
        | cur_c == 1 && move_c == 0     = 0
        | cur_c == 1 && move_c == 2     = 1
        | cur_c == 2                    = 1
        | otherwise                     = (-1)

replace_p1t7 :: [a] -> Int -> Int -> a -> [a]
replace_p1t7 arr index cnt rep
        | null arr                      = arr
        | index == cnt                  = rep : (tail arr)
        | otherwise                     = (head arr) : (replace_p1t7 (tail arr) index (cnt + 1) rep)

getTile_p1t7 :: [[a]] -> Int -> Int -> a
getTile_p1t7 board row col = 
        (getRow_p1t7 board row) !! col

getRow_p1t7 :: [a] -> Int -> a
getRow_p1t7 board row = 
        board !! row

