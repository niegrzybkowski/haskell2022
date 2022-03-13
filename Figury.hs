module Figury where

data Pozycja = Pozycja Double Double deriving Show
data Rozmiar = Rozmiar Double Double deriving Show
type Promien = Double 

data Figura = 
    Prostokat Pozycja Rozmiar |
    Kolo Pozycja Promien  |
    Odcinek Pozycja Pozycja
    deriving Show

wymiar :: Figura -> Integer 
wymiar (Prostokat _ _) = 2
wymiar (Kolo _ _) = 2
wymiar (Odcinek _ _) = 1

pole :: Figura -> Double 
pole (Prostokat _ (Rozmiar x y)) = x * y
pole (Kolo _ r) = pi * r * r
pole (Odcinek _ _) = 0.0

skaluj :: Figura -> Double -> Figura
skaluj (Prostokat poz (Rozmiar x y)) a = Prostokat poz (Rozmiar (x * a) (y * a))
skaluj (Kolo poz r) a = Kolo poz $ r * a
skaluj (Odcinek (Pozycja x1 y1) (Pozycja x2 y2)) a = 
    Odcinek (Pozycja x1 y1) 
        (Pozycja 
        (x1+(x2-x1) * a)
        (y1+(y2-y1) * a))