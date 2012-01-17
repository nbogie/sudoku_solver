import Text.Printf(printf)
import Data.List (delete, nub)
import Data.Char (digitToInt)
import Data.Maybe (isNothing, catMaybes, fromJust)
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
parseBoard = M.fromList . parseBoardToList

parseBoardToList ::  String -> [(String, Values)]
parseBoardToList s = zip squares $ map parseChar s

display :: Board -> String
display m = unlines $ map ( concat . map disp) $ wrap squares
  where disp sq = printf "%11s" (concatMap show $ m M.! sq)
        wrap xs | length xs <= 9 = [xs]
                | otherwise      = take 9 xs : (wrap . drop 9 ) xs

demo =  "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"

b = parseBoard demo

main = do 
  putStrLn $ show $ parseBoard demo
  putStrLn $ display $ fromJust $ assign "A1" 4 b

---------------------------------------------------------------------------------
-- SOLVER
---------------------------------------------------------------------------------
assign :: Square -> Int -> Board -> Maybe Board
assign s d m = if d `elem` m M.! s
                 then eliminate s d m
                 else Nothing 
  
--eliminate a digit from the given square's peers
eliminate ::  Square -> Int -> Board -> Maybe Board
eliminate s d board = foldl elimOne (Just board) (peersFor s)
  where 
    elimOne :: Maybe Board -> Square -> Maybe Board
    elimOne Nothing p = Nothing
    elimOne (Just m) p = case delete d (m M.! p) of
                          [] -> Nothing
                          [o] -> assign p o (M.insert p [o] m)
                          vs -> Just (M.insert p vs m)

