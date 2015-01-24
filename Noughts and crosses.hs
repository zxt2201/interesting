G52AFP Coursework 1 - Noughts and crosses
   
Your full name(s)
Your full email address(es)

----------------------------------------------------------------------

> import Data.List
> import Data.Maybe

For flexibility, the size of the board is defined as a constant:

> size                  :: Int
> size                  =  3

The board itself is represented as a list of list of player values,
with the width and height of the board always being of the above size:

> type Board            =  [[Player]]

In turn, a player value is either a nought, a blank, or a cross, with
a blank representing a space on the board that is not yet occupied:

> data Player           =  Nought | Blank | Cross
>                          deriving (Ord, Eq, Show)


> data Tree a = Node a [Tree a]
>   deriving Show

The following code displays a board on the screen:

> showBoard             :: Board -> IO ()
> showBoard             =  putStrLn . unlines . concat
>                             . separate hbar . map showRow
>                          where
>			      hbar = [replicate (size * 6) '-']
>
> showRow               :: [Player] -> [String]
> showRow               =  beside . separate vbar . map showPlayer
>                          where
>                             beside = foldr1 (zipWith (++))
>                             vbar   = replicate (size + 2) "|"
>
> showPlayer            :: Player -> [String]
> showPlayer Nought     =  ["     ", " +-+ ", " | | ", " +-+ ", "     "]
> showPlayer Blank      =  ["     ", "     ", "     ", "     ", "     "]
> showPlayer Cross      =  ["     ", " \\ / ", "  X  ", " / \\ ", "     "]
>
> separate              :: a -> [a] -> [a]
> separate x []         =  []
> separate x [y]        =  [y]
> separate x (y:ys)     =  y : x : separate x ys

> emptyBoard = replicate size (replicate size Blank )

> try:: Board
> try = [[Nought,Cross,Cross],[Blank,Blank,Blank],[Nought,Cross,Nought]]

> rows :: Board -> [[Player]]
> rows = id

> cols :: Board -> [[Player]]
> cols = transpose

> diagonal :: Board -> [[Player]]
> diagonal xss = [xss !! n !! n | n <- [0..(size -1)]] : [xss !! n !!(size - n - 1) | n <- [0..(size -1)]] : []

> empty :: Player -> Bool
> empty = (==) Blank

> turn :: Board -> Player
> turn xss = if num Nought xss > num Cross xss then Cross else Nought
> 				where num p = length . filter (== p) . concat

> wins :: Player -> Board -> Bool 
> wins p xss = or [all (== p) xs | xs <- rows xss ++ cols xss ++ diagonal xss]

> aas xs = Just (xs ++ [])

> winner :: Board -> Maybe Player
> winner xss | wins Nought xss = Just Nought
>			 | wins Cross xss  = Just Cross
>			 | otherwise       = Nothing

> move :: Int -> Player -> Board -> Maybe Board
> move n p xss | n < 0 || n > (size * size - 1) = Nothing
>			   | list !! n /= Blank             = Nothing
>			   | otherwise = Just (split (take n list ++ [p] ++ drop (n + 1) list))
>                where 
>					list = concat xss
> 					split = chop size

> chop :: Int -> [a] -> [[a]]
> chop n [] = []
> chop n xs = take n xs : chop n (drop n xs)	

> toTree :: Board -> Tree Board
> toTree xss  | wins p xss = Node xss []
>			  | otherwise = Node xss ((map toTree . catMaybes) [move n p xss | n <- [0..(size * size - 1)]])
> 			where p = turn xss

> getNode :: Tree a  -> a
> getNode (Node x _) = x 
						  
> minMaxTree :: Tree Board -> Tree (Board, Player)
> minMaxTree (Node b []) | wins Nought b = Node (b, Nought) []
>						 | wins Cross b  = Node (b, Cross) []
>						 | otherwise     = Node (b, Blank) []						 
> minMaxTree (Node b ts) = Node (b, p) ts' 
>				where 
>				ts' = map minMaxTree ts
>				p = minmax (map (snd . getNode) ts')
>				minmax = if turn b == Cross then maximum else minimum

> getBoard :: Tree (Board, Player) -> Board
> getBoard (Node (b, p) []) = b
> getBoard (Node (b, p) (t:ts))| snd n == p = fst n
>							   | otherwise = getBoard (Node (b, p) ts)
>		where n = getNode t
>
> nextComputerBoard :: Board -> Board
> nextComputerBoard = getBoard.minMaxTree.toTree					
> 


426



> main = do
>   putStrLn "which player do you want to choose, O or X?"
>   (char:_) <- getLine
>   if validChar char then
>     mainloop (startingBoard char) --(toPlayer char)
>   else
>   putStrLn "Not Valid"
>   where 
>         startingBoard ch | ch == 'X' = fromJust (move 0 Nought emptyBoard)
>                          | ch == 'O' = emptyBoard
>         validChar 'O' = True
>         validChar 'X' = True
>         validChar _   = False
>     --    toPlayer ch | ch == 'X' = Cross
>    --                 | ch == 'O' == Nought


> getOpponent p | p == Cross = Nought
>               | p == Nought = Cross

> toString :: Player -> Char
> toString p | p == Cross = 'X'
>            | p == Nought = 'O'

> mainloop :: Board -> IO()
> mainloop b = do 
>                showBoard b
>                if wins opponent b then putStrLn ((toString opponent) :  " wins")
>                else  do
>                       putStrLn "Which next position?"
>                       n <- readLn
>                       let b' = move n player b 
>                       if b' == Nothing then putStrLn "The position is not allowed"
>                       else
>                           if wins player (fromJust b') then putStrLn ((toString player) :  " wins") 
>                           else mainloop (nextComputerBoard (fromJust b'))
>        where 
>        player = turn b
>        opponent = getOpponent player
>
>
> 
>
> -- 0 2 4 6
>
>
>
>
>
>
>
>
>
>
>
>
>
>
>
>
>
>
>
>
>
>
>
>
>
>
>
>
>
>
>
>

> n = Nought
> b = Blank
> c = Cross
> ex = [ [c,c,n]
>      , [n,b,b]
>      , [n,c,n]]



----------------------------------------------------------------------

