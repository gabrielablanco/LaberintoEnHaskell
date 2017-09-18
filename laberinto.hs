import System.Process

main = do
  let laberinto = [0, 0, 1, 1, 1, 1,
		   1, 0, 1, 0, 0, 1,
		   1, 0, 1, 0, 1, 1,
		   1, 0, 1, 0, 1, 1,
		   1, 0, 0, 0, 1, 1,
		   2, 2, 2, 2, 2, 2
		   ]
  let comenzar = 1
  let pos = 1
  juego laberinto comenzar pos

juego :: [Int] -> Int -> Int -> IO()
juego laberinto comenzar pos = do
 --system("sleep 1")
 --system("clear")
 show_lab laberinto comenzar pos
 let nueva_pos = pos + 1
 let temp = drop pos laberinto
 let temp1 = head temp
 if mod pos 6 == 0 then do
    putStrLn "Has finalizado el laberinto"
 else
    if temp1 == 1 then do
       let nueva_pos = pos + 6
       juego laberinto comenzar nueva_pos
    else
	juego laberinto comenzar nueva_pos

show_lab :: [Int] -> Int -> Int -> IO()
show_lab laberinto cont pos = do
  let h = head laberinto
  decidir h pos cont
  putStr " "
  let cont1 = cont + 1
  let laberinto1 = tail laberinto
  cr_if_mult_6 cont
  if not (cont == 36) then
     show_lab laberinto1 cont1 pos
  else
     putStr ""

decidir :: Int -> Int -> Int -> IO()
decidir h pos cont = do
   if cont == pos then
	putStr "O"
   else if h == 0 then
	putStr "."
   else 
	putStr "X"

cr_if_mult_6 :: Int -> IO()
cr_if_mult_6 c = do
  let test = mod c 6
  if test == 0 then
	putStrLn ""
  else
	putStr ""