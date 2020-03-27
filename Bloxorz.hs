{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import qualified Data.Array as A

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc 
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    *** TODO ***    d o n e

    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}

data Cell = HardCell | SoftCell | Block | Switch | EmptySpace | WinCell
    deriving (Eq, Ord)

instance Show Cell where
    show HardCell = [hardTile]
    show SoftCell = [softTile]
    show Block = [block]
    show Switch = [switch]
    show EmptySpace = [emptySpace]
    show WinCell = [winningTile]


{-
    *** TODO ***    d o n e

    Tip de date pentru reprezentarea nivelului curent
-}

data Level = Level
  { gameMap :: A.Array Position Cell --harta jocului formata din Cell
  , blockPosition :: (Position, Position) --pozitia blocului in harta (poate ocupa o celula - vertical, sau doua - orizontal)
  , gameStatus :: String --Game won, lost or in progress
  , switchStatus :: (String, [Position]) --switch status : Activated/ Deactivated + lista de pozitii
  } deriving (Eq, Ord)

{-
    *** Opțional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară, 
    instantiati explicit clasele Eq și Ord pentru Level. 
    În cazul acesta, eliminați deriving (Eq, Ord) din Level. 
-}

-- instance Eq Level where
--     (==) = undefined

-- instance Ord Level where
--     compare = undefined

{-
    *** TODO ***    d o n e

    Instantiati Level pe Show. 

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou. 
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n". 
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n". 
-}

instance Show Level where
    show (Level array position status _) = "\n" ++ unlines list ++ if status == "win" then "Congrats! You won!\n"
                                                                       else if status == "lost" then "Game Over\n" else ""
        where 
            list = [concat [show (if (x,y) /= ((fst (fst position)), (snd (fst position))) && (x,y) /= ((fst (snd position)), (snd (snd position)))
                                  then (array A.! (x,y)) 
                                  else Block
                                  ) | y <- [lowy..highy]] | x <- [lowx..highx]]
            ((lowx, lowy), (highx, highy)) = A.bounds array


{-
    *** TODO ***    d o n e

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

emptyLevel :: Position -> Position -> Level
emptyLevel (highX, highY) (x, y) =
    (Level (A.array ((0,0), (highX, highY)) [((i, j), EmptySpace) | i <- [0..highX], j <- [0..highY]]) ((x, y), (x,y)) "in progress" ("", []))

{-
    *** TODO ***    d o n e

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        'H' pentru tile hard 
        'S' pentru tile soft 
        'W' pentru winning tile 
-}

addTile :: Char -> Position -> Level -> Level
addTile letter (x, y) (Level array position status swsts) = case letter of 'H' -> (Level (array A.// [((x, y), HardCell)]) position status swsts)
                                                                           'S' -> (Level (array A.// [((x, y), SoftCell)]) position status swsts)
                                                                           'W' -> (Level (array A.// [((x, y), WinCell)]) position status swsts)
                                                                           _ -> (Level array position status swsts)


{-
    *** TODO ***    d o n e

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau 
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch (x, y) list (Level array position status _) = (Level (array A.// [((x, y), Switch)]) position status ("off", list))

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***    d o n e

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
-}

activate :: Cell -> Level -> Level
activate cell (Level array position status swsts) = 
  case cell of HardCell -> if status /= "lost" then (Level array position "in progress" swsts)
                           else (Level array position "lost" swsts)
               SoftCell -> if fst position == snd position || status == "lost" then (Level array position "lost" swsts) 
                           else (Level array position "in progress" swsts)
               EmptySpace -> (Level array position "lost" swsts)
               WinCell -> if fst position == snd position then (Level array position "win" swsts)
                          else (Level array position "in progress" swsts)
               Switch -> if fst swsts == "off" 
                         then (Level (array A.// [(snd swsts!!i, HardCell) | i <- [0..length (snd swsts) - 1]]) position status ("on", snd swsts))
                         else (Level (array A.// [(snd swsts!!i, EmptySpace) | i <- [0..length (snd swsts) - 1]]) position status ("off", snd swsts))
               Block -> (Level array position status swsts)
                                                           

{-
    *** TODO ***    d o n e

    Mișcarea blocului în una din cele 4 direcții 
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}

move :: Directions -> Level -> Level
move direction (Level array position status swsts) =
    let ((i1,j1), (i2,j2)) = position
        p1 = fst position
        p2 = snd position
    in case direction of North -> if (p1 == p2) then activate (array A.! (i1 - 1, j1)) (activate (array A.! (i2 - 2, j2)) (Level array ((i1 - 1,j1), (i2 - 2,j2)) status swsts))
                                  else if (j1 == j2) then activate (array A.! (i2 - 2, j2)) (Level array ((i1 - 1,j1), (i2 - 2,j2)) status swsts)
                                  else activate (array A.! (i1 - 1,j1)) (activate (array A.! (i2 - 1,j2)) (Level array ((i1 - 1,j1), (i2 - 1,j2)) status swsts))
                         South -> if (p1 == p2) then activate (array A.! (i1 + 1, j1)) (activate (array A.! (i2 + 2, j2)) (Level array ((i1 + 1,j1), (i2 + 2,j2)) status swsts)) 
                                  else if (j1 == j2) then activate (array A.! (i2 + 1, j2)) (Level array ((i1 + 2,j1), (i2 + 1,j2)) status swsts) 
                                  else activate (array A.! (i1 + 1,j1)) (activate (array A.! (i2 + 1,j2)) (Level array ((i1 + 1,j1), (i2 + 1,j2)) status swsts))
                         West -> if (p1 == p2) then activate (array A.! (i1, j1 - 1)) (activate (array A.! (i2, j2 - 2)) (Level array ((i1,j1 - 1), (i2,j2 - 2)) status swsts))
                                 else if (j1 == j2) then activate (array A.! (i1, j1 - 1)) (activate (array A.! (i2, j2 - 1)) (Level array ((i1,j1 - 1), (i2,j2 - 1)) status swsts)) 
                                 else activate (array A.! (i1, j1 - 2)) (Level array ((i1,j1 - 2), (i2,j2 - 1)) status swsts) 
                         East -> if (p1 == p2) then activate (array A.! (i1, j1 + 1)) (activate (array A.! (i2, j2 + 2)) (Level array ((i1,j1 + 1), (i2,j2 + 2)) status swsts)) 
                                 else if (j1 == j2) then activate (array A.! (i1, j1 + 1)) (activate (array A.! (i2, j2 + 1)) (Level array ((i1,j1 + 1), (i2,j2 + 1)) status swsts)) 
                                 else activate (array A.! (i1, j1 + 2)) (Level array ((i1,j1 + 2), (i2,j2 + 1)) status swsts) 
                            

{-
    *** TODO ***    d o n e

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame (Level _ _ game _) = if game /= "win" || game /= "lost" then True else False

{-
    *** TODO ***    d o n e

    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}


getStatus :: Level -> String
getStatus (Level _ _ status _) = status

instance ProblemState Level Directions where
    successors level@(Level array position status swsts) = 
      [(direction, move direction level) | direction <- [North, South, West, East], getStatus (move direction level) /= "lost"]

    isGoal (Level _ _ state _) = if state == "win" then True else False

    -- Doar petru BONUS
    -- heuristic = undefined
