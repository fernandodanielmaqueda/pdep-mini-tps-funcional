module TP2 where

------------------------------------------
-- Modelo inicial
------------------------------------------

data Celular = Celular {
  linea :: (String, String),
  saldo :: Int,
  proveedor :: String
} deriving (Show, Eq)

------------------------------------------

promoRecarga :: Int -> Celular -> Celular
promoRecarga montoPagado celular = recargarSaldo (calculoMontoRecarga montoPagado celular) celular

recargarSaldo :: Int -> Celular -> Celular
recargarSaldo montoFinalDeRecarga celular = (Celular (linea celular) ((saldo celular) + montoFinalDeRecarga) (proveedor celular))

calculoMontoRecarga :: Int -> Celular -> Int
calculoMontoRecarga montoPagado celular
 | ((proveedor celular) == "Movistar") && ((codigoAreaLinea (linea celular)) == "011") = promoMovistar montoPagado
 | ((proveedor celular) == "Personal") = promoPersonal montoPagado
 | otherwise = montoPagado

codigoAreaLinea :: (String, String) -> String
codigoAreaLinea (codigoArea, _) = codigoArea

promoMovistar :: Int -> Int
promoMovistar = (3*)

promoPersonal :: Int -> Int
promoPersonal montoPagado = montoPagado + (min montoPagado 100)