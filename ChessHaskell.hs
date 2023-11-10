type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])
---------------------------------------------------------------------------------------
setBoard :: Board

setBoard = (White, [R ('h',1),N ('g',1),B ('f',1),K ('e',1),Q ('d',1),B ('c',1),N ('b',1),R ('a',1),P ('h',2),P ('g',2),P ('f',2),P ('e',2),P ('d',2),P ('c',2),P ('b',2),P ('a',2)] ,[R ('h',8),N ('g',8),B ('f',8),K ('e',8),Q ('d',8),B ('c',8),N ('b',8),R ('a',8),P ('h',7),P ('g',7),P ('f',7),P ('e',7),P ('d',7),P ('c',7),P ('b',7),P ('a',7)])
--------------------------------------------------------------------------
visualizeBoard:: Board->String

visualizeBoard (x,y,z) = "    a    b    c    d    e    f    g    h   " ++ forCol 8 'a' (x,y,z) ++ "\n\nTurn : " ++ show x

forCol i j z = if i>0 then  "\n" ++ show i ++" |"++forRow i j z else ""

forRow i j z = if j<='h' then getElement i j z ++ forRow i (succ j) z else forCol (i-1) 'a' z

getElement i j (x,y,z) 
                 |elem (R (j,i)) y = " RW |" 
                 |elem (N (j,i)) y = " NW |"   
                 |elem (B (j,i)) y = " BW |"
                 |elem (K (j,i)) y = " KW |"   
                 |elem (Q (j,i)) y = " QW |"
                 |elem (P (j,i)) y = " PW |"
                 |elem (R (j,i)) z = " RB |"
                 |elem (N (j,i)) z = " NB |"
                 |elem (B (j,i)) z = " BB |"
                 |elem (K (j,i)) z = " KB |"
                 |elem (Q (j,i)) z = " QB |"
                 |elem (P (j,i)) z = " PB |"
                 |otherwise = "    |"
--------------------------------------------------------------------------
isLegal:: Piece -> Board -> Location -> Bool

isLegal (R (x,y)) (t,w,b) (i,j) = if exist (R (x,y)) w b then if notTaken (R (x,y)) w b (i,j) then if j==y then checkCol x i w b j else if x == i then checkRow y j w b i else False else False else error "Does not exist"

isLegal (N (x,y)) (t,w,b) (i,j) = if exist (N (x,y)) w b then if notTaken (N (x,y)) w b (i,j) then checkKnight x y i j  else False else error "Does not exist"

isLegal (B (x,y)) (t,w,b) (i,j) = if exist (B (x,y)) w b then if notTaken (B (x,y)) w b (i,j) then if  ((charToNum x) - y) == ((charToNum i)-j) then checkDiag  x y w b i j  else  if ((charToNum x) + y) == ((charToNum i)+j)  then checkAntiDiag x y w b i j else False else False else error "Does not exist"

isLegal (Q (x,y)) (t,w,b) (i,j) = if exist (Q (x,y)) w b then if notTaken (Q (x,y)) w b (i,j) then if j==y then checkCol x i w b j else if x == i then checkRow y j w b i else if  ((charToNum x) - y) == ((charToNum i)-j) then checkDiag  x y w b i j  else  if ((charToNum x) + y) ==  ((charToNum i)+j)  then checkAntiDiag x y w b i j else False else False else error "Does not exist"

isLegal (K (x,y)) (t,w,b) (i,j) = if exist (K (x,y)) w b then if notTaken (K (x,y)) w b (i,j) then checkAdj x y i j else False else error "Does not exist"

isLegal (P (x,y)) (t,w,b) (i,j) = if exist (P (x,y)) w b then if x==i then if notTakenBlock j i w b  then  checkPawn (P (x,y)) w b i j else False else if taken i j (P (x,y)) w b then checkPawnAttk (P (x,y)) w b i j else False else error "Does not exist"

checkCol :: Char->Char->[Piece]->[Piece]->Int->Bool
checkCol x i w b j  = if x==i then False else if x<i then incCol (succ x) i w b j else incCol (succ i) x w b j

incCol :: Char->Char->[Piece]->[Piece]->Int->Bool
incCol x i w b j = if x<i then notTakenBlock j x w b && incCol (succ x) i w b j else True

checkRow :: Int->Int->[Piece]->[Piece]->Char->Bool
checkRow y j w b i  = if y==j then False else if y<j then incRow (succ y) j w b i else incRow (succ j) y w b i

incRow :: Int->Int->[Piece]->[Piece]->Char->Bool
incRow y j w b i = if y<j then notTakenBlock y i w b && incRow (succ y) j w b i else True

checkDiag :: Char->Int->[Piece]->[Piece]->Char->Int->Bool
checkDiag x y w b i j = if(x<i) then incDiag (succ x) (succ y) w b i j else incDiag (succ i) (succ j) w b x y

incDiag :: Char->Int->[Piece]->[Piece]->Char->Int->Bool
incDiag x y w b i j = if x<i then notTakenBlock y x w b && incDiag (succ x) (succ y) w b i j else True

checkAntiDiag :: Char->Int->[Piece]->[Piece]->Char->Int->Bool
checkAntiDiag x y w b i j = if x<i then incAntiDiag (succ x) (y-1) w b i j else incAntiDiag (succ i) (j-1) w b x y

incAntiDiag :: Char->Int->[Piece]->[Piece]->Char->Int->Bool
incAntiDiag x y w b i j = if x<i then notTakenBlock y x w b && incAntiDiag (succ x) (y-1) w b i j else True

notTakenBlock :: Int->Char->[Piece]->[Piece]->Bool
notTakenBlock x y w b 
                        |elem (R (y,x)) w || elem (R (y,x)) b = False
                        |elem (P (y,x)) w || elem (P (y,x)) b = False
                        |elem (N (y,x)) w || elem (N (y,x)) b = False
                        |elem (K (y,x)) w || elem (K (y,x)) b = False
                        |elem (Q (y,x)) w || elem (Q (y,x)) b = False
                        |elem (B (y,x)) w || elem (B (y,x)) b = False
                        |otherwise = True

checkKnight x y i j |(succ (succ x)) == i && succ y == j = True
                    |(succ (succ x)) == i && succ j == y = True
                    |(succ (succ i)) == x && succ y == j = True
                    |(succ (succ i)) == x && succ j == y = True
                    |(succ (succ y)) == j && succ x == i = True
                    |(succ (succ y)) == j && succ i == x = True
                    |(succ (succ j)) == y && succ x == i = True
                    |(succ (succ j)) == y && succ i == x = True
                    |otherwise = False

checkAdj :: Char->Int->Char->Int->Bool
checkAdj x y i j = if (subtr x i) <=1 && abs (y-j) <=1 then True else False

checkPawn :: Piece->[Piece]->[Piece]->Char->Int->Bool
checkPawn (P (x,y)) w b i j =if elem (P (x,y)) w then if y==2 then (j==3|| j==4) && notTakenBlock 3 i w b else (y+1) == j  else  if y==7 then (j==6|| j==5) && notTakenBlock 5 i w b else (y-1) == j

checkPawnAttk (P (x,y)) w b i j = if elem (P (x,y)) w then ((y+1)==j) && ( ((succ i) == x) || ((succ x) == i) )  else ((y-1)==j) && ( ((succ i) == x) || ((succ x) == i) )

taken x y piece w b = if elem piece w then takenH (x,y) b  else takenH (x,y) w

takenH z x    |elem (R z) x = True
              |elem (N z) x = True                               
              |elem (B z) x = True
              |elem (K z) x = True
              |elem (Q z) x = True
              |elem (P z) x = True 
              |otherwise = False

notTaken x w b y = if elem x w then notTakenH y w else notTakenH y b  
                                        
notTakenH z x |elem (R z) x = False
              |elem (N z) x = False                               
              |elem (B z) x = False
              |elem (K z) x = False
              |elem (Q z) x = False
              |elem (P z) x = False 
              |otherwise = True

charToNum :: Char -> Int
charToNum 'a' = 1
charToNum 'b' = 2
charToNum 'c' = 3
charToNum 'd' = 4
charToNum 'e' = 5
charToNum 'f' = 6
charToNum 'g' = 7
charToNum 'h' = 8

subtr :: Char->Char->Int
subtr x a = if x==a then 0 else if (x<a) then (1 + subtr (succ x) a) else 1 + (subtr (succ a) x)

exist piece w b = elem piece w || elem piece b
--------------------------------------------------------------------------------
suggestMove:: Piece -> Board -> [Location]
suggestMove p b = filter (isLegal p b) cells

cells :: [(Char, Int)]
cells = concatMap (\x -> map (\y -> (x, y)) [1..8]) ['a'..'h']
------------------------------------------------------------------------------
move:: Piece -> Location -> Board -> Board
move p (x,y) (t,w,b) = if t==White then if elem p b then error  "This is White player's turn, Black can't move."  else  if isLegal  p (t,w,b) (x,y) then  if taken x y p w b then  (Black,(removeAndAdd p w x y),(remove (x,y) b))  else   (Black,(removeAndAdd p w x y),b)  else error ("Illegal move for piece "++show p) else if elem p w then error "This is Black player's turn, White can't move."   else if isLegal p  (t,w,b) (x,y)  then  if taken x y p w b then   (White,(remove (x,y) w),(removeAndAdd p b x y))  else (White,w,(removeAndAdd p b x y))  else error ("Illegal move for piece "++show p)

removeAndAdd (R (i,j)) (h:t) x y  = if h == (R (i,j)) then (R (x,y)):t else h:(removeAndAdd (R (i,j)) (t) x y)
removeAndAdd (N (i,j)) (h:t) x y  = if h == (N (i,j)) then (N (x,y)):t else h:(removeAndAdd (N (i,j)) (t) x y)  
removeAndAdd (B (i,j)) (h:t) x y  = if h == (B (i,j)) then (B (x,y)):t else h:(removeAndAdd (B (i,j)) (t) x y)
removeAndAdd (K (i,j)) (h:t) x y  = if h == (K (i,j)) then (K (x,y)):t else h:(removeAndAdd (K (i,j)) (t) x y) 
removeAndAdd (Q (i,j)) (h:t) x y  = if h == (Q (i,j)) then (Q (x,y)):t else h:(removeAndAdd (Q (i,j)) (t) x y)
removeAndAdd (P (i,j)) (h:t) x y  = if h == (P (i,j)) then (P (x,y)):t else h:(removeAndAdd (P (i,j)) (t) x y)

remove z x |elem (R z) x = removeh (R z) x
           |elem (N z) x = removeh (N z) x                            
           |elem (B z) x = removeh (B z) x
           |elem (K z) x = removeh (K z) x
           |elem (Q z) x = removeh (Q z) x
           |otherwise = removeh (P z) x
           
removeh piece (h:t) = if h==piece then t else h:(removeh piece t)
-------------------------------------------------------------------------