import System.IO  
import Data.Char
import qualified Data.Text as DT

main = do
	handle <- openFile "D:/Studia/SPOP/input.txt" ReadMode
	input <- hGetContents handle
	checkInputLength input
	let 
		rowValuesString = head (lines input)
		columnValuesString = (lines input) !! 1
		housesPositionsString = last (lines input)
		rowValues = getIntList rowValuesString 
		columnValues = getIntList columnValuesString
		housesPositions = getIntToupleList housesPositionsString
		rowCount = length(rowValues) 
		columnCount = length(columnValues)
		initialBoard = buildInitialBoard rowCount columnCount housesPositions 
		executionContext = ExecutionContext rowCount columnCount rowValues columnValues housesPositions initialBoard in
			print (solvePuzzle executionContext)

-- funkcja sprawdzaj¹ca czy odczytany plik sk³ada siê z 3 linii
checkInputLength :: String 		-- ³añcuch znaków odczytany z pliku (ca³y)
					-> IO () 	-- wartoœæ zwracana (ignorowane)
checkInputLength input = do
						if length (lines input) > 3 
						then error "Too many lines in input file"
						else putStr ""
						
-- pobiera listê int z jej tekstowej reprezentacji
getIntList :: String 		-- badany ci¹g znaków
					-> [Int]	-- wartoœæ zwracana - sparsowana lista liczb ca³kowitych
getIntList intListString = do
						let 
							ignore1 = checkListStart intListString
							ignore2 = checkListEnd intListString
							filteredNumbers = (filter (\x -> isDigit x || x == ' ') intListString)
							numbers = map (\x -> read x::Int) (map DT.unpack (DT.splitOn (DT.pack " ") (DT.pack filteredNumbers))) in
								numbers

-- pobiera listê krotek (Int, Int) z jej reprezentacji tekstowej								
getIntToupleList :: String 	-- reprezentacja tekstowa listy
					-> [(Int, Int)] -- wynikowa lista
getIntToupleList listString = do
						let
							ints = getIntList listString
							toupleList = toTupleList ints in
								toupleList
								
-- przekszta³a listê elementów t w listê krotek (t, t). Kolejne krotki s¹ tworzone na podsawie s¹siaduj¹cych elementów
toTupleList :: [t]
				-> [(t, t)]
toTupleList [] = []
toTupleList (x:[]) = error "Provided list doesn't contains tuples"
toTupleList (x:y:rest) = (x,y) : toTupleList(rest)
							

-- sprawda czy zadany ci¹g znaków zaczyna siê od znaku [					
checkListStart :: String		-- badany ci¹g znaków
					-> IO ()	-- 
checkListStart list = do
					if head list /= '['
						then error "Line has to start with [ sign"
						else putStr ""
						
-- sprawdza czy zadany ci¹g znaków koñczy siê znakiem ]
checkListEnd :: String 			-- badany ci¹g znaków
					-> IO ()
checkListEnd list = do
					if last list /= ']'
						then error "Line has to end with ] sign"
						else putStr ""
	
-- klasa przechowuj¹ca kontekst wywo³ania programu
data ExecutionContext = ExecutionContext { 	rowCount :: Int						-- liczba wierszy
											, columnCount :: Int				-- liczba kolumn
											, rowValues :: [Int]				-- liczba przy³¹czy w kolejnych wierszach
											, columnValues :: [Int]				-- liczba kolumn w kolejnych wierszach
											, housesPositions :: [(Int,Int)]	-- pozycje domów na planszy
											, initialBoard :: [Int]				-- tablica zainicjalizowana wy³¹cznie pozycjami domów
										} deriving (Show) 

-- tworzy planszê z naniesionymi pozycjami domów
buildInitialBoard :: Int -- liczba wierszy
					-> Int -- liczba kolumn
					-> [(Int, Int)] -- pozycje domów
					-> [Int] -- wynikowa plansza
buildInitialBoard rowCount columnCount housesPositions =
					let 
						emptyBoard = createEmptyBoard rowCount columnCount in
							putHouses emptyBoard columnCount housesPositions
					
-- tworzy planszê wype³nion¹ 0
createEmptyBoard :: Int -- liczba wierszy
					-> Int -- liczba kolumn
					-> [Int] -- wynikowa plansza
createEmptyBoard rowCount columnCount = 
					take (rowCount * columnCount) (repeat 0)
			
-- umieszcza budynki na planszy			
putHouses :: [Int] -- plansza
			-> Int -- liczba kolumn
			-> [(Int, Int)] -- pozycje budynkow
			-> [Int] -- wynikowa plansza
putHouses board _ [] = 
					board
putHouses board columnCount (position:otherPositions) =
					putHouses (replaceAtN board (fst position * columnCount + snd position) 1) columnCount otherPositions
	
-- zamienia wartoœæ w liœcie na odpowiedniej pozycji	
replaceAtN :: [Int] -- lista
			-> Int -- pozycja, na której trzeba podmieniæ wartoœæ
			-> Int -- wartoœæ, która ma zostaæ ustawiona
			-> [Int] -- wynikowa lista
replaceAtN (x:list) 0 value = value:list
replaceAtN (x:list) index value = x : (replaceAtN list (index-1) value)
					
-- rozwi¹zuje zagadkê architekta
solvePuzzle :: ExecutionContext -- kontekst zagadki
		-> [[Int]] 				-- rozwi¹zania
solvePuzzle context =
		[[0, 0]]