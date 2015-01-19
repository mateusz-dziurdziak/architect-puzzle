import System.IO  
import Data.Char
import qualified Data.Text as DT

main = do
	putStrLn "Please define input file to load:" 
	filename <- getLine
	handle <- openFile filename ReadMode
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
			printFinalSolutionAndSave (filterSolutions(solvePuzzle executionContext) rowValues columnValues housesPositions) (length rowValues)
			
printFinalSolutionAndSave :: [Int] -> Int -> IO ()
printFinalSolutionAndSave xs size = do
			putStrLn "---------------------------------------"
			putStrLn "Solution that matches task conditions:"
			print (splitToRows xs size)
			putStrLn "---------------------------------------"
			putStrLn "Do you want to save solution to file? [y/n]"
			decision <- getLine
			if(decision == "y" || decision == "Y")
			then do
				putStrLn "Insert a file name to save solution:"
				filenameOut <- getLine
				-- handle <- openFile filenameOut WriteMode
				writeFile filenameOut (map intToDigit xs)
				putStrLn "Solution saved to file."
			else putStrLn "Solution not saved."
		
-- funkcja sprawdzaj¹ca czy odczytany plik sk³ada siê z 3 linii
checkInputLength :: String 		-- ³añcuch znaków odczytany z pliku (ca³y)
					-> IO () 	-- wartoœæ zwracana (ignorowane)
checkInputLength input = do
						if length (lines input) > 3 
						then error "Too many lines in input file"
						else putStr ""
						
-- pobiera listê int z jej tekstowej reprezentacji
getIntList :: String 			-- badany ci¹g znaków
					-> [Int]	-- wartoœæ zwracana - sparsowana lista liczb ca³kowitych
getIntList intListString = do
						let 
							ignore1 = checkListStart intListString
							ignore2 = checkListEnd intListString
							filteredNumbers = (filter (\x -> isDigit x || x == ' ') intListString)
							numbers = map (\x -> read x::Int) (map DT.unpack (DT.splitOn (DT.pack " ") (DT.pack filteredNumbers))) in
								numbers

-- pobiera listê krotek (Int, Int) z jej reprezentacji tekstowej								
getIntToupleList :: String 			-- reprezentacja tekstowa listy
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
					-> IO ()	-- (ewentualny) blad wypisany na konsole
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

-- konwencja:
--		0 - puste
--		1 - dom
--		2 - przylacze
--
-- klasa przechowuj¹ca kontekst wywo³ania programu
data ExecutionContext = ExecutionContext { 	rowCount :: Int						-- liczba wierszy
											, columnCount :: Int				-- liczba kolumn
											, rowValues :: [Int]				-- liczba przy³¹czy w kolejnych wierszach
											, columnValues :: [Int]				-- liczba kolumn w kolejnych wierszach
											, housesPositions :: [(Int,Int)]	-- pozycje domów na planszy
											, initialBoard :: [Int]				-- tablica zainicjalizowana wy³¹cznie pozycjami domów
										} deriving (Show) 

-- tworzy planszê z naniesionymi pozycjami domów
buildInitialBoard :: Int 			-- liczba wierszy
					-> Int 			-- liczba kolumn
					-> [(Int, Int)] -- pozycje domów
					-> [Int]		-- wynikowa plansza
buildInitialBoard rowCount columnCount housesPositions =
					let 
						emptyBoard = createEmptyBoard rowCount columnCount in
							putHouses emptyBoard columnCount housesPositions
					
-- tworzy planszê wype³nion¹ 0
createEmptyBoard :: Int 		-- liczba wierszy
					-> Int 		-- liczba kolumn
					-> [Int] 	-- wynikowa plansza
createEmptyBoard rowCount columnCount = 
					take (rowCount * columnCount) (repeat 0)
			
-- umieszcza budynki na planszy			
putHouses :: [Int] 			-- plansza
			-> Int 			-- liczba kolumn
			-> [(Int, Int)] -- pozycje budynkow
			-> [Int] 		-- wynikowa plansza
putHouses board _ [] = 
					board
putHouses board columnCount (position:otherPositions) =
					putHouses (replaceAtN board (fst position * columnCount + snd position) 1) columnCount otherPositions
	
-- zamienia wartoœæ w liœcie na odpowiedniej pozycji	
replaceAtN :: [Int] 	-- lista
			-> Int 		-- pozycja, na której trzeba podmieniæ wartoœæ
			-> Int 		-- wartoœæ, która ma zostaæ ustawiona
			-> [Int] 	-- wynikowa lista
replaceAtN (x:list) 0 value = value:list
replaceAtN (x:list) index value = x : (replaceAtN list (index-1) value)

-- rozwi¹zuje zagadkê architekta
solvePuzzle :: ExecutionContext -- kontekst zagadki
		-> [[Int]] 				-- rozwi¹zania
solvePuzzle context =
			let 
				boards = addGas (sum (rowValues context)) (initialBoard context) in
					boards

-- dzieli zadan¹ planszê na listê wierszy (ka¿dy wiersz jest list¹ wartoœci)
splitToRows :: [Int] 		-- plansza
				-> Int 		-- rozmiar wiersza (liczba kolumn)
				-> [[Int]] 	-- lista wierszy
splitToRows board columnCount = if length board == columnCount 
									then [board]
									else [take columnCount board] ++ (splitToRows (drop columnCount board) columnCount)
									
-- dzieli zadana plansze na liste kolumn (kazda kolumna to lista wartosci)
splitToCols :: [Int] 		-- plansza
				-> Int 		-- rozmiar kolumny (liczba wierszy)
				-> [[Int]]	-- lista kolumn
splitToCols board rowCount = 
			 splitCols (splitToRows board rowCount)
			
splitCols :: [[Int]] -> [[Int]]
splitCols ([]:_) = []
splitCols list =
			[map head list] ++ splitCols (map tail list)
			

addGas :: Int
		-> [Int]
		-> [[Int]]
addGas 0 board = [board]
addGas _ [] = []
addGas gasCount board = if countEmpty board < gasCount
						then []
						else if checkFirst board 0
							then combine 2 (addGas (gasCount - 1) (drop 1 board)) ++ combine 0 (addGas gasCount (drop 1 board))
							else combine (head board) (addGas gasCount (drop 1 board))

-- sprawdza czy pierwszy element listy jest rowny zadanej wartoœci							
checkFirst :: [Int] -- lista
			-> Int 	-- zadana wartoœæ
			-> Bool -- wynik operacji
checkFirst [] _ = False
checkFirst (x:xs) z 
			| x == z = True
			| otherwise = False

-- ³¹czy zadan¹ wartoœæ  z ka¿d¹ list¹ przekazan¹ w drugim parametrze
combine :: Int 			-- wartoœæ
			-> [[Int]] 	-- lista list 
			-> [[Int]] 	-- wynikowa lista
combine _ [] = []
combine value (x:xs) = [(value:x)] ++ (combine value xs) 

-- liczy wyst¹pienia 0 w liœcie								
countEmpty :: [Int] 	-- lista
			-> Int 		-- liczba wyst¹pieñ 0
countEmpty [] = 0
countEmpty (0:xs) = 1 + countEmpty xs
countEmpty (x:xs) = countEmpty xs 

-- liczy wystapienia zadanej liczby w liscie
countNumInList :: [Int]		-- lista Intow
			-> Int			-- liczba ktorej wystapienia zliczamy
			-> Int			-- liczba wystapien zliczanej liczby

countNumInList [] _ = 0
countNumInList (x:xs) a = 
			if (x == a)
			then 1 + countNumInList xs a
			else 0 + countNumInList xs a

-------------------------------------------------------------------------------------
--    FILTROWANIE WYNIKOW
-------------------------------------------------------------------------------------
-- filtruje wygenerowane rozwiazania w poszukiwaniu takiego, ktore spelnia zalozenia.
filterSolutions :: [[Int]] 	-- lista wygenerowanych rozwiazan
		-> [Int]			-- lista liczb z lewej strony planszy
		-> [Int]			-- lista liczb nad plansza
		-> [(Int, Int)]		-- pozycje domow
		-> [Int]			-- RETURN jedyne sluszne rozwiazanie
filterSolutions solutions rowValues columnValues housesPositions =
			let
			newSols = filterNumberOfHousesAndGasTanks solutions (length housesPositions) 	-- czy liczba przylaczy i domow jest taka sama
			newSols2 = filterRowsNumbersMatch newSols rowValues								-- czy rozmieszczenie przylaczy zgodne z liczbami z lewej strony planszy
			newSols3 = filterColsNumbersMatch newSols2 columnValues							-- czy rozmieszczenie przylaczy zgodne z liczbami nad plansza
			newSols4 = filterGasNextToHouses newSols3 housesPositions rowValues 			-- czy przylacza przy domach (ponad/pod lub obok)
			newSols5 = filterGasNotNextToEachOther newSols4 (length rowValues) in			-- czy przylacza nie stykaja sie (ponad/pod, obok, po przekatnej)
				head newSols5																-- powinno zostac jedno rozwiazanie - prawidlowe, spelniajace warunki (przy zalozeniu ze kazda lamiglowka ma dokladnie jedno rozwiazanie)

-- filtruje rozwiazania sprawdzajac czy ilosc domow jest taka jak ilosc zbiornikow z gazem
filterNumberOfHousesAndGasTanks :: [[Int]] 		-- rozwiazania
		-> Int									-- ilosc domow
		-> [[Int]]								-- RETURN przefiltrowane rozwiazania
filterNumberOfHousesAndGasTanks [sol] housesNo =
			if (sum(sol) == 3 * housesNo)
			then [sol]
			else []
filterNumberOfHousesAndGasTanks (s:rest) housesNo = 
			(filterNumberOfHousesAndGasTanks [s] housesNo) ++ (filterNumberOfHousesAndGasTanks rest housesNo)
			
-- filtruje rozwiazania sprawdzajac czy ilosc przylaczy w wierszach jest zgodna z liczbami z lewej strony planszy
filterRowsNumbersMatch :: [[Int]]	-- lista wygenerowanych rozwiazan
		-> [Int]					-- lista liczb z lewej strony planszy
		-> [[Int]]					-- RETURN rozwiazania po przefiltrowaniu
filterRowsNumbersMatch [s] nums =
			if(checkRowsOrCols (splitToRows s (length nums)) nums)
			then [s]
			else []
filterRowsNumbersMatch (s:rest) nums =
			(filterRowsNumbersMatch [s] nums) ++ (filterRowsNumbersMatch rest nums)
			
-- sprawdza warunek ze w wierszu lub kolumnie jest x przylaczy
checkRowsOrCols :: [[Int]]	-- lista wierszy
		-> [Int]			-- lista liczb z lewej strony planszy
		-> Bool				-- czy jest ok
checkRowsOrCols [] _ = True
checkRowsOrCols [row] [num] = 
			(countNumInList row 2) == num
checkRowsOrCols (row:rows) (num:nums) = 
			checkRowsOrCols [row] [num] && checkRowsOrCols rows nums
			
-- filtruje rozwiazania sprawdzajac czy ilosc przylaczy w kolumnach jest zgodna z liczbami nad plansza
filterColsNumbersMatch :: [[Int]]	-- lista wygenerowanych rozwiazan
		-> [Int]					-- lista liczb nad plansza
		-> [[Int]]					-- RETURN rozwiazania po przefiltrowaniu
filterColsNumbersMatch [s] nums =
		if(checkRowsOrCols (splitToCols s (length nums)) nums)
		then [s]
		else []
filterColsNumbersMatch (s:rest) nums =
			(filterColsNumbersMatch [s] nums) ++ (filterColsNumbersMatch rest nums)
			
-- filtruje rozwiazania sprawdzajac czy przy kazdym domu jest przylacze
filterGasNextToHouses :: [[Int]]	-- lista wygenerowanych rozwiazan
		-> [(Int, Int)]				-- lista polozen domow
		-> [Int]					-- liczby z lewej strony planszy
		-> [[Int]]					-- RETURN rozwiazania po przefiltrowaniu
filterGasNextToHouses [s] houses rows =
		if(gasNextToHouses (splitToRows s (length rows)) houses)
		then [s]
		else []
filterGasNextToHouses (s:rest) houses rows =
			(filterGasNextToHouses [s] houses rows) ++ (filterGasNextToHouses rest houses rows) 

-- sprawdza czy przylacza znajduja sie obok domow
gasNextToHouses :: [[Int]]	-- wiersze
		-> [(Int, Int)]		-- lista domkow (polozen)
		-> Bool				-- czy jest ok
gasNextToHouses rows [] = True
gasNextToHouses rows ((a,b):xs) = 
			if( gasNextToHouse (a,b) rows (length rows) && gasNextToHouses rows xs)
			then True
			else False

-- sprawdza czy jakies przylacze znajduje sie obok domu
gasNextToHouse :: (Int, Int)	-- polozenie domu
		-> [[Int]] 				-- plansza w postaci wierszy
		-> Int 					-- rozmiar planszy
		-> Bool					-- czy jest jakies przylacze (min jedno) przy domu
gasNextToHouse (a,b) rows size =
			if left (a,b) rows size || right (a,b) rows size || under (a,b) rows size || above (a,b) rows size
			then True
			else False

-- czy po lewej od domu jest przylacze
left (x,y) rows size = 
	if(y-1 >= 0)
	then if((rows !! x) !! (y-1) == 2)
		then True
		else False
	else False

-- czy po prawej od domu jest przylacze
right (x,y) rows size = 
	if(y+1 < size)
	then if((rows !! x) !! (y+1) == 2)
		then True
		else False
	else False
	
-- czy pod domem jest przylacze
under (x,y) rows size = 
	if(x-1 >= 0)
	then if((rows !! (x-1)) !! y == 2)
		then True
		else False
	else False

-- czy nad domem jest przylacze
above (x,y) rows size = 
	if(x+1 < size)
	then if((rows !! (x+1)) !! y == 2)
		then True
		else False
	else False
	
-- filtruje rozwiazania, przy ktorych zbiorniki z gazem nie stykaja sie ze soba
filterGasNotNextToEachOther :: [[Int]] 	-- plansze z rozwiazaniami
		-> Int 							-- rozmiar planszy
		-> [[Int]]						-- plansze po przefiltrowaniu (spelniajace warunek nie stykania sie przylaczy)
filterGasNotNextToEachOther [s] size=
		if(gasNotNextToEachOther s s size 0)
		then [s]
		else []
filterGasNotNextToEachOther (s:rest) size =
		(filterGasNotNextToEachOther [s] size) ++ (filterGasNotNextToEachOther rest size) 
			
-- sprawdza czy w rozwiazaniu wokol przylaczy nie znajduja sie inne
gasNotNextToEachOther :: [Int] 	-- plansza po ktorej iterujemy
		-> [Int] 				-- plansza na ktorej sprawdzamy sasiadow
		-> Int 					-- rozmiar planszy
		-> Int 					-- aktualny indeks
		-> Bool					-- czy rozwiazanie spelnia warunek nie stykania sie przylaczy
gasNotNextToEachOther [] _ _ _ = True
gasNotNextToEachOther (x:xs) s size ind =
		if(x == 2)
		then if (leftGas ind s size || rightGas ind s size || underGas ind s size || aboveGas ind s size || leftUpperGas ind s size || rightUpperGas ind s size || leftLowerGas ind s size || rightLowerGas ind s size)
			then False
			else gasNotNextToEachOther xs s size (ind+1)
		else gasNotNextToEachOther xs s size (ind+1)
			
-- czy po lewej od pola o indeksie ind na planszy board nie znajduje sie przylacze
leftGas :: Int -> [Int] -> Int -> Bool
leftGas ind board size =
		if(ind `mod` size == 0)
		then False
		else if (board !! (ind -1) == 2)
			then True
			else False

-- czy po prawej od pola o indeksie ind na planszy board nie znajduje sie przylacze	
rightGas :: Int -> [Int] -> Int -> Bool		
rightGas ind board size =
		if(ind `mod` size == (size - 1))
		then False
		else if (board !! (ind + 1) == 2)
			then True
			else False

-- czy pod polem o indeksie ind na planszy board nie znajduje sie przylacze
underGas :: Int -> [Int] -> Int -> Bool
underGas ind board size =
		if(floor (fromIntegral (ind `div` size) :: Double) == (size - 1))
		then False
		else if ( board !! (ind + size) == 2)
			then True
			else False
			
-- czy nad polem o indeksie ind na planszy board nie znajduje sie przylacze
aboveGas :: Int -> [Int] -> Int -> Bool
aboveGas ind board size =
		if(floor (fromIntegral (ind `div` size) :: Double) == 0)
		then False
		else if ( board !! (ind - size) == 2)
			then True
			else False
			
-- czy na lewo ponad polem o indeksie ind na planszy board nie znajduje sie przylacze
leftUpperGas :: Int -> [Int] -> Int -> Bool
leftUpperGas ind board size =
		if((floor (fromIntegral (ind `div` size) :: Double) == 0) || (ind `mod` size == 0))
		then False
		else if ((board !! (ind - size - 1)) == 2)
			then True
			else False

-- czy na prawo ponad polem o indeksie ind na planszy board nie znajduje sie przylacze		
rightUpperGas :: Int -> [Int] -> Int -> Bool	
rightUpperGas ind board size =
		if((floor (fromIntegral (ind `div` size) :: Double) == 0) || (ind `mod` size == size - 1))
		then False
		else if ((board !! (ind - size + 1)) == 2)
			then True
			else False
			
-- czy na prawo pod polem o indeksie ind na planszy board nie znajduje sie przylacze
leftLowerGas :: Int -> [Int] -> Int -> Bool
leftLowerGas ind board size =
		if((floor (fromIntegral (ind `div` size) :: Double) == size - 1) || (ind `mod` size == 0))
		then False
		else if ((board !! (ind + size - 1)) == 2)
			then True
			else False
			
-- czy na lewo pod polem o indeksie ind na planszy board nie znajduje sie przylacze
rightLowerGas :: Int -> [Int] -> Int -> Bool
rightLowerGas ind board size =
		if((floor (fromIntegral (ind `div` size) :: Double) == size - 1) || (ind `mod` size == size - 1))
		then False
		else if ((board !! (ind + size + 1)) == 2)
			then True
			else False
