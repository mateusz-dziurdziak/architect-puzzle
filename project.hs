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

-- funkcja sprawdzaj�ca czy odczytany plik sk�ada si� z 3 linii
checkInputLength :: String 		-- �a�cuch znak�w odczytany z pliku (ca�y)
					-> IO () 	-- warto�� zwracana (ignorowane)
checkInputLength input = do
						if length (lines input) > 3 
						then error "Too many lines in input file"
						else putStr ""
						
-- pobiera list� int z jej tekstowej reprezentacji
getIntList :: String 		-- badany ci�g znak�w
					-> [Int]	-- warto�� zwracana - sparsowana lista liczb ca�kowitych
getIntList intListString = do
						let 
							ignore1 = checkListStart intListString
							ignore2 = checkListEnd intListString
							filteredNumbers = (filter (\x -> isDigit x || x == ' ') intListString)
							numbers = map (\x -> read x::Int) (map DT.unpack (DT.splitOn (DT.pack " ") (DT.pack filteredNumbers))) in
								numbers

-- pobiera list� krotek (Int, Int) z jej reprezentacji tekstowej								
getIntToupleList :: String 	-- reprezentacja tekstowa listy
					-> [(Int, Int)] -- wynikowa lista
getIntToupleList listString = do
						let
							ints = getIntList listString
							toupleList = toTupleList ints in
								toupleList
								
-- przekszta�a list� element�w t w list� krotek (t, t). Kolejne krotki s� tworzone na podsawie s�siaduj�cych element�w
toTupleList :: [t]
				-> [(t, t)]
toTupleList [] = []
toTupleList (x:[]) = error "Provided list doesn't contains tuples"
toTupleList (x:y:rest) = (x,y) : toTupleList(rest)
							

-- sprawda czy zadany ci�g znak�w zaczyna si� od znaku [					
checkListStart :: String		-- badany ci�g znak�w
					-> IO ()	-- 
checkListStart list = do
					if head list /= '['
						then error "Line has to start with [ sign"
						else putStr ""
						
-- sprawdza czy zadany ci�g znak�w ko�czy si� znakiem ]
checkListEnd :: String 			-- badany ci�g znak�w
					-> IO ()
checkListEnd list = do
					if last list /= ']'
						then error "Line has to end with ] sign"
						else putStr ""
	
-- klasa przechowuj�ca kontekst wywo�ania programu
data ExecutionContext = ExecutionContext { 	rowCount :: Int						-- liczba wierszy
											, columnCount :: Int				-- liczba kolumn
											, rowValues :: [Int]				-- liczba przy��czy w kolejnych wierszach
											, columnValues :: [Int]				-- liczba kolumn w kolejnych wierszach
											, housesPositions :: [(Int,Int)]	-- pozycje dom�w na planszy
											, initialBoard :: [Int]				-- tablica zainicjalizowana wy��cznie pozycjami dom�w
										} deriving (Show) 

-- tworzy plansz� z naniesionymi pozycjami dom�w
buildInitialBoard :: Int -- liczba wierszy
					-> Int -- liczba kolumn
					-> [(Int, Int)] -- pozycje dom�w
					-> [Int] -- wynikowa plansza
buildInitialBoard rowCount columnCount housesPositions =
					let 
						emptyBoard = createEmptyBoard rowCount columnCount in
							putHouses emptyBoard columnCount housesPositions
					
-- tworzy plansz� wype�nion� 0
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
	
-- zamienia warto�� w li�cie na odpowiedniej pozycji	
replaceAtN :: [Int] -- lista
			-> Int -- pozycja, na kt�rej trzeba podmieni� warto��
			-> Int -- warto��, kt�ra ma zosta� ustawiona
			-> [Int] -- wynikowa lista
replaceAtN (x:list) 0 value = value:list
replaceAtN (x:list) index value = x : (replaceAtN list (index-1) value)
					
-- rozwi�zuje zagadk� architekta
solvePuzzle :: ExecutionContext -- kontekst zagadki
		-> [[Int]] 				-- rozwi�zania
solvePuzzle context =
		[[0, 0]]