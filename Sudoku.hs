import Text.Printf(printf)
import Data.List (delete, nub, (\\), minimumBy, find)
import Data.Char (digitToInt)
import Data.Ord (comparing)
import Data.Maybe (isNothing, catMaybes, isJust)
import Data.Maybe (fromJust) --TODO: remove this evil (keep its use restricted to conveniences for demoing)
import Control.Monad (join) -- join :: Monad m => m (m a) -> m a 
import qualified Data.Map as M

---------------------------------------------------------------------------------
-- Representing squares, units, peers
---------------------------------------------------------------------------------
rowNames = "ABCDEFGHI"
colNames = "123456789"
-- rowSquares :: [[Square]]
-- rowSquares = [Square r c | r <- rowNames, c <- colNames]

type Board = M.Map Square Values
type Values = [Int]
type Square = String
type Unit = [Square]
squares = [r:c:[] | r<-rowNames, c<-colNames]

unitlist :: [Unit]
unitlist = rows ++ cols ++ boxes 

rows,cols,boxes :: [Unit]
-- alternative = map (\r -> map (\c -> r:c:[]) colNames) rowNames
rows = [ [r:c:[] | c<-colNames ] | r<-rowNames]
cols = [ [r:c:[] | r<-rowNames ] | c<-colNames]
boxes = [cross c r | c<- ["ABC", "DEF", "GHI"], r<- ["123", "456", "789"]]
cross as bs = [a:b:[] | a<-as, b<-bs]

units :: M.Map Square [Unit]
units = M.fromList [(s, [u | u<-unitlist, s`elem` u]) | s<- squares]

unitsFor :: Square -> [Unit]
unitsFor s = units M.! s

peers :: M.Map Square [Square]
peers = M.fromList $ [(s, ps s) | s <- squares]
        where ps s = delete s $ nub $ concat (unitsFor s)
peersFor :: Square -> [Square]
peersFor s = peers M.! s

---------------------------------------------------------------------------------
-- PArsing board
---------------------------------------------------------------------------------

parseChar '.' = [1..9]
parseChar d = [digitToInt d]

parseBoardToList ::  String -> [(String, Values)]
parseBoardToList s = zip squares $ map parseChar s

--make a board from a simple parsed board by trying to run full assignment of each of its filled-in values (detects conflict). Doesn't try to solve completely.
parseBoard :: String -> Maybe Board
parseBoard str = foldl f (Just emptyBoard) singles
  where 
    emptyBoard = M.fromList . zip squares $ repeat [1..9]
    singles = filter ((==1) . length . snd) $ parseBoardToList str
    f :: Maybe Board -> (Square, Values) -> Maybe Board
    f Nothing _ = Nothing
    f (Just b) (sq, [v]) = assign sq v b
    f (Just b) (sq, vs) = error $ "Programming bug - filter sould have removed multi-value squares but got "++ show (sq, vs)

display :: Board -> String
display m = unlines $ map ( concat . map disp) $ wrap squares
  where disp sq = printf "%11s" (concatMap show $ m M.! sq)
        wrap xs | length xs <= 9 = [xs]
                | otherwise      = take 9 xs : (wrap . drop 9 ) xs
---------------------------------------------------------------------------------
-- Helpers for experimenting in GHCI
---------------------------------------------------------------------------------
-- tb "Try to assign to Board"
tb :: Square -> Int -> Board -> IO Board
tb s d b = do
  
  case assign s d b of
    Just okBoard -> putStrLn ("assigned " ++ show (s, d)) >> pb okBoard
    Nothing      -> error "Couldn't assign"

-- pb "Print Board (IO)".  Returns the board so you can chain e.g. with "it".
pb :: Board -> IO Board 
pb b = do
  putStrLn $ display b
  return b

---------------------------------------------------------------------------------
-- Demo data
---------------------------------------------------------------------------------
demo =  "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
baddemo =  "44....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"

demoBoard = parseBoard demo

---------------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------------
main = do 
  pp "initial" $ parseBoard demo
  pp "a1 assigned with 4" $ assign "A1" 4 (fromJust demoBoard)
  pp "a2 assigned with 4" $ assign "A2" 1 (fromJust demoBoard)
  pp "Try to assign A2 with 4" $ assign "A2" 4 (fromJust demoBoard)
  pp "Try to solve" $ solve demo
    where pp title (Just b) = bannerWith title >> putStrLn (display b)
          pp title Nothing = bannerWith title >> putStrLn " failed to assign (Nothing)"
          bannerWith msg = let bn = ((take 80 . cycle) "=") in putStrLn bn >> putStrLn msg >> putStrLn bn 
          
---------------------------------------------------------------------------------
-- SOLVER
---------------------------------------------------------------------------------
solve :: String -> Maybe Board
solve str = search $ parseBoard str

search :: Maybe Board -> Maybe Board
search Nothing = Nothing --Failed earlier
search (Just board) | allLengthOne = Just board --Finished!
                    | otherwise    = join . find isJust . map (\d -> search $ assign squareWithFewestChoices d board) $ choices
--     ## Chose the unfilled square s with the fewest possibilities
  where 
    allLengthOne = all ((==1) . length . (board M.!)) squares
    (squareWithFewestChoices, choices) = minimumBy (comparing (length . snd)) $ [(s, vs) | s <- squares, let vs = board M.! s, length vs > 1]

assign :: Square -> Int -> Board -> Maybe Board
assign s d b = 
  foldl (eliminate s) (Just b) otherVals
    where otherVals = delete d $ b M.! s
  
--eliminate a digit from the given square's peers
eliminate ::  Square -> Maybe Board -> Int -> Maybe Board
eliminate s Nothing d =      Nothing --TODO: this impl is only here so we can apply this fn to its own output, in a fold.
eliminate s (Just board) d = 
  -- if we've already removed the value at that square, do nothing. 
  -- else remove it, experimentally and case the result
  --   zero values remaining -> return Nothing (fail)
  --   one value remaining   -> try to "assign" that value (mutual recursion)
  --   n values remaining    -> return experimentally changed board
  if not (d `elem` board M.! s)
    then Just board
    else case delete d (board M.! s) of
           []        -> Nothing
           vs@[lastVal] -> let b' = M.insert s vs board in step2 s d $ foldl elim (Just b') (peersFor s)
                          where elim b p = eliminate p b lastVal
                                --TODO: just switching the order of eliminate args
           vs        -> step2 s d $ Just $ M.insert s vs board

-- This could be scoped within eliminate, but it starts to be tricky avoiding either shadowing refs or using the wrong one
-- from the higher context: e.g. board, b, b', b2...
-- A top-level declaration is less prone to such errors for the beginner, 
-- but more verbose (we have to pass in the square and the digit where otherwise we'd have access from the context).
step2 :: Square -> Int -> Maybe Board -> Maybe Board
step2 _ _ Nothing = Nothing
step2 s d b2M = foldl unitTrim b2M (unitsFor s)
  where unitTrim Nothing _     = Nothing
        unitTrim (Just b) unit = 
          case findSquaresInUnitWithValue unit d of
                 [] -> Nothing -- Contradiction: no place for this value in this unit
                 -- d can only be in one place in unit; assign it there
                 [oneSq] -> assign oneSq d b
                 _ -> Just b
                 where findSquaresInUnitWithValue u d = [s | s <- u, d `elem` (b M.! s)]

-- Further work: 
-- 1) instead of reporting contradictions with Maybe Board, it might be more helpful to use Either Error Board.
-- This way the nature of the contradiction could be reported back.
-- 3) That the fns given to fold all have the same special case for Nothing suggests an abstraction.  Perhaps there's a monadic fold.
-- 4) I don't know if fold is really what we want.  We want to short circuit as soon as we encounter a contradiction.
--
--
-- DONE:
-- 2) Finish the solver with search!
