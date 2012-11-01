--Caroline McQuatt 
--Peter Zhang
maze1 = ["www-","--w","--","---","bbbb"];
data Tree a =
         Branch { 
            board :: [String], 
            children :: [Tree [String]]
            } 
    deriving (Eq, Show)


oska_x1y2 :: [String] -> Char -> Int -> [String]
oska_x1y2 board player moves = 
        analyze_board (Branch board (statesearch [board] moves player)) player


--TODO check branches for cycles
statesearch :: [[String]] -> Int -> Char -> [Tree [String]]
statesearch unexplored moves player
        | null unexplored || moves == 0                = []
        | not (null newstates)                                = (map (\x -> (Branch x (statesearch (genNewStates x player) (moves - 1) player))) newstates)
        | otherwise                                                        = statesearch (tail unexplored) (moves - 1) player
                where newstates                = (genNewStates (head unexplored) player)


--TODO
genNewStates :: [String] -> Char -> [[String]]
genNewStates board player = genNewStatesHelper board player (findTiles board player 0 0)


genNewStatesHelper :: [String] -> Char ->  [(Int,Int)]-> [[String]]
genNewStatesHelper board side locs 
        | locs == [] = []
        | otherwise  = (genNewStatesforPiece board (fst (head locs)) (snd (head locs)) 0) ++ (genNewStatesHelper board side (tail locs))


-- assumes the first list inside board is 0 row
-- findTiles maze1 'w' 0 0
-- returns [(0,0),(1,0),(2,0),(2,1)]
findTiles :: [String] -> Char -> Int -> Int -> [(Int, Int)]
findTiles board side countx county 
        | board == [] = []
        | otherwise = (findTilesOnRow (head board) side 0 county) ++ (findTiles (tail board) side 0 (county+1))
        
findTilesOnRow :: String-> Char -> Int -> Int -> [(Int,Int)]
findTilesOnRow row side countx y
        | row == [] = []
        | (head row) == side = (countx, y) : (findTilesOnRow (tail row) side (countx+1) y)
        | otherwise = (findTilesOnRow (tail row) side (countx+1) y)

-- generate new states for a given piece at given location c and r
-- genNewStatesforPiece maze1 0 2 0
-- returns [["ww--","-ww","--","---","bbbb"]]
genNewStatesforPiece ::[String] -> Int -> Int ->  Int -> [[String]]
genNewStatesforPiece board c r count_r
        | ((length board) - 1) == count_r = (movesForPieceAtRow board count_r 0 c r )
        |  otherwise                = (movesForPieceAtRow board count_r 0 c r ) ++ (genNewStatesforPiece board c r (count_r+1))



-- get possible moves for a single piece at given roll.. does not account for jumps, havent figured out how to jump >_>
-- movesForPieceAtRow maze1 1 0 0 1
-- returns [["w-w-","w-w","--","---","bbbb"],["w-w-","-ww","--","---","bbbb"]]
movesForPieceAtRow :: [String] -> Int -> Int -> Int -> Int -> [[String]]
movesForPieceAtRow board row col piece_r piece_c 
        | ((length (getRow board row)) - 1) == col = newstate
        |otherwise = newstate ++ (movesForPieceAtRow board row (col+1) piece_r piece_c)
        where
                side     = (getTile board piece_r piece_c)
                newstate = (do if (isLegalMove board piece_r piece_c row col side) 
                                                        then [(genMove board piece_r piece_c row col (-111) (-111))] 
                                                        else [])
                
--TODO
analyze_board :: Tree [String] -> Char -> [String]
analyze_board (Branch board children) side = board


--does not check for valid starting indexes or tile, must be correct
isLegalMove :: [String] -> Int -> Int -> Int -> Int -> Char -> Bool
isLegalMove board cur_r cur_c move_r move_c side
        | (not row_exists) || (not col_exists)        = False
        | (distance == 1) && cur_r == mid_row        = mv_tile == '-' && (isLegalMid cur_c move_c False)
        | (distance == 1)                                                = valid_col_reg && mv_tile == '-'
        | (distance == 2) && 
                (getJumpRow side cur_r) == mid_row        = (mv_tile == '-') && (not ((jmp_tile == side) || (jmp_tile == '-'))) && (isLegalMid cur_c move_c True)
        | (distance == 2)                                                = valid_col_jmp && (mv_tile == '-') &&
                                                                                                (not ((jmp_tile == side) || (jmp_tile == '-')))
        | otherwise                                                                = False
        where
                mid_row                                = div ((length board) - 1) 2 
                distance                        = abs (cur_r - move_r)
                row_exists                        = (move_r >= 0) && (move_r < (length board)) 
                col_exists                        = (move_c >= 0) && (move_c < (length (getRow board move_r))) 
                valid_col_reg                = (move_c == cur_c) || (move_c == (cur_c - 1))
                valid_col_jmp                = (move_c == cur_c) || (move_c == (cur_c - 2)) 
                mv_tile                                = getTile board move_r move_c
                jmp_tile                        = if (isJumpMid cur_r mid_row side) then
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
                old_cur_row                        = getRow board cur_r
                new_cur_row                        = replace old_cur_row cur_c 0 '-'
                old_move_row                = getRow board move_r 
                piece                                 = old_cur_row !! cur_c
                new_move_row                = replace old_move_row move_c 0 piece
                old_jump_row                = board !! jump_r
                new_jump_row                = replace old_jump_row jump_c 0 '-'

number_pieces :: [String] -> Char -> Int
number_pieces board player
        | null board                                                        = 0
        | otherwise                                                                = (count_in_row (head board) player) + (number_pieces (tail board) player)

--expects either 'b' or 'w' for player
isWinningBoard :: [String] -> Char -> Bool
isWinningBoard board player
        | player == 'w'                                                        = number_pieces board 'b' == 0 || checklast board 0 ((length board) - 1) 'w'
        | otherwise                                                                = number_pieces board 'w' == 0 || checklast board 0 0 'w'

--helpers-------------------------------------------------------------------------
checklast :: [String] -> Int -> Int -> Char -> Bool
checklast board i row player
        | null board                                                         = True
        | i == row                                                                 = (elem player (head board)) && checklast (tail board) (i + 1) row player
        | otherwise                                                                =  not((elem player (head board))) && checklast (tail board) (i + 1) row player 


count_in_row :: String -> Char -> Int
count_in_row row player
        | null row                                                                 = 0
        | (head row) == player                                        = 1 + count_in_row (tail row) player
        | otherwise                                                                = count_in_row (tail row) player

--Helper: assumes row and cols exist
isLegalMid :: Int -> Int -> Bool -> Bool 
isLegalMid cur_c move_c is_jmp 
        | is_jmp && cur_c == 0                                         = move_c == 1
        | is_jmp && cur_c == 1                                        = (move_c == 0) || (move_c == 2)
        | is_jmp && cur_c == 2                                        = move_c == 1
        | cur_c == 0                                                        = (move_c == 0) || (move_c == 1)
        | cur_c == 1                                                        = (move_c == 2) || (move_c == 1)
        | otherwise                                                         = False

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
        | cur_c == move_c                                                = cur_c
        | (cur_c - 2) == move_c                                        = (cur_c - 1)
        | otherwise                                                                = (-1)

--Helper: assumes that the jump is legal
getJTileColMid :: Int -> Int -> Int
getJTileColMid cur_c move_c
        | cur_c == 0                                                        = 1
        | cur_c == 1 && move_c == 0                                = 0
        | cur_c == 1 && move_c == 2                                = 1
        | cur_c == 2                                                        = 1
        | otherwise                                                                = (-1)

replace :: [a] -> Int -> Int -> a -> [a]
replace arr index cnt rep
        | null arr                                 = arr
        | index == cnt                        = rep : (tail arr)
        | otherwise                                = (head arr) : (replace (tail arr) index (cnt + 1) rep)

getTile :: [[a]] -> Int -> Int -> a
getTile board row col = 
        (getRow board row) !! col

getRow :: [a] -> Int -> a
getRow board row = 
        board !! row

