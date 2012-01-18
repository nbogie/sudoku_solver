import Control.Monad (join, foldM) -- join :: Monad m => m (m a) -> m a 
import Data.Char (digitToInt)
import Data.List (delete, nub, minimumBy, maximumBy, find)
import Data.Ord (comparing)
import qualified Data.Map as M
import Text.Printf (printf)

---------------------------------------------------------------------------------
-- Representing squares, units, peers
---------------------------------------------------------------------------------
rowNames = "ABCDEFGHI"
colNames = "123456789"

type Error = String

type Board = M.Map Square Values
type Values = [Int]
type Square = String
type Unit = [Square]
squares = [ [r,c] | r<-rowNames, c<-colNames]

rows,cols,boxes :: [Unit]
rows = [ [ [r,c] | c<-colNames ] | r<-rowNames]
cols = [ [ [r,c] | r<-rowNames ] | c<-colNames]
boxes = [cross c r | c<- ["ABC", "DEF", "GHI"], r<- ["123", "456", "789"]]

cross as bs = [ [a,b] | a<-as, b<-bs]

unitsFor :: Square -> [Unit]
unitsFor s = units M.! s
  where
  units :: M.Map Square [Unit]
  units = M.fromList [(s, [u | u<-unitList, s`elem` u]) | s<- squares]
  unitList = rows ++ cols ++ boxes 

peersFor :: Square -> [Square]
peersFor s = peers M.! s
  where
  peers :: M.Map Square [Square]
  peers = M.fromList [(s, ps s) | s <- squares]
          where ps s = delete s $ nub $ concat (unitsFor s)

---------------------------------------------------------------------------------
-- Parsing board
---------------------------------------------------------------------------------
parseChar '.' = [1..9]
parseChar d = [digitToInt d]

parseBoardNoCP :: String -> M.Map Square Char
parseBoardNoCP str = M.fromList $ zip squares str

--Make a board from a simple parsed board by trying to run full assignment of 
--each of its filled-in values (detects conflict). Doesn't try to solve completely.
parseBoard :: String -> Either Error Board
parseBoard str = foldM asgn emptyBoard singles
  where 
    asgn :: Board -> (Square, Int) -> Either Error Board
    asgn b (sq, v) = assign sq v b
    emptyBoard = M.fromList . zip squares $ repeat [1..9]
    singles = [(s,head vs) | (s,vs) <- parseBoardToList str, length vs == 1]
    parseBoardToList s = zip squares $ map parseChar s

---------------------------------------------------------------------------------
-- Displaying board (and partially parsed board)
---------------------------------------------------------------------------------
-- Can't use Show typeclass, as Board is just a type alias
display :: Board -> String
display b = displayGridded (printf ("%"++show width++"s") . concatMap show) b
  where width = worstLength + 1
        worstLength = maximum $ map (length . (b M.!)) squares

displayRaw :: M.Map Square Char -> String
displayRaw = displayGridded (:[]) 

displayGridded :: (a -> String) -> M.Map Square a -> String
displayGridded fn m = unlines $ map (concatMap disp) $ wrap squares
  where disp sq = fn $ m M.! sq
        wrap xs | length xs <= 9 = [xs]
                | otherwise      = take 9 xs : (wrap . drop 9) xs

---------------------------------------------------------------------------------
-- Demo data
---------------------------------------------------------------------------------
demo = 
  "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
demoBoard = parseBoard demo

---------------------------------------------------------------------------------
-- SOLVER
---------------------------------------------------------------------------------
solve :: String -> Either Error Board
solve str = parseBoard str >>= search

instance Monad (Either e) where
  Left l >>= _f = Left l
  Right r >>= f = f r 
  return = Right

search :: Board -> Either Error Board
search b | allLenOne = return b --Finished!
         | otherwise = 
  case find isRight . map (\d -> assign squareWithFewestChoices d b >>= search) $ choices of
    Just (Right soln) -> Right soln
    Nothing -> Left "various searches failed (so various errors" 
  where 
    allLenOne = all ((==1) . length . (b M.!)) squares
    (squareWithFewestChoices, choices) = 
      minimumBy (comparing (length . snd)) [(s, vs) | s <- squares, let vs = b M.! s, length vs > 1]
    isRight :: Either a b -> Bool
    isRight (Right _) = True
    isRight _ = False

assign :: Square -> Int -> Board -> Either Error Board
assign s d b = 
  foldM (eliminate s) b otherVals
    where otherVals = delete d $ b M.! s
  
--eliminate a digit from the given square, propagating constraints.
eliminate ::  Square -> Board -> Int -> Either Error Board
eliminate s b d = 
  -- if we've already removed the value at that square, do nothing. 
  if d `notElem` b M.! s
    then return b
    else return b >>= step1 s d >>= step2 s d

-- Remove digit from sq, and apply rule 1:
-- (1) If a square s is reduced to one value d2, 
-- then eliminate d2 from the peers.
step1 ::  Square -> Int -> Board -> Either Error Board
step1 s d b = 
  case delete d (b M.! s) of
    []   -> Left $ "Contradiction " ++ show (s, d, "no possibles")
    [d2] -> foldM elim (M.insert s [d2] b) (peersFor s)
              where elim b' p = eliminate p b' d2
    vs   -> return $ M.insert s vs b

-- Apply rule 2: 
-- (2) If a unit u is reduced to only one place for a value d, 
--     then put it there.
step2 :: Square -> Int -> Board -> Either Error Board
step2 s d board = foldM unitTrim board (unitsFor s)
  where 
    unitTrim b unit = 
      case unitSquaresWithVal unit d of
          []      -> Left $ "No squares in unit have value " ++ show (unit, d)
          [oneSq] -> assign oneSq d b -- d can only be in one place in unit; 
                                      -- assign it there.
          _       -> return b
        where unitSquaresWithVal u d = [s | s <- u, d `elem` (b M.! s)]

---------------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------------
main = interact $ unlines . map solveAndShow . lines
  where
  solveAndShow :: String -> String
  solveAndShow str = displayRaw (parseBoardNoCP str) ++ 
                     "\n\n" ++ either ("Can't solve because "++) display (solve str)

---------------------------------------------------------------------------------
-- Further work: 
---------------------------------------------------------------------------------
-- 1) instead of reporting contradictions with Maybe Board, 
--    it might be more helpful to use Either Error Board.
--    This way the nature of the contradiction could be reported back.
-- 4) I don't know if fold is really what we want.  We want to short circuit 
--    as soon as we encounter a contradiction.
--
-- DONE:
-- 2) Finish the solver with search!
-- 3) That the fns given to fold all have the same special case for Nothing 
--    suggests an abstraction.  Perhaps there's a monadic fold.
