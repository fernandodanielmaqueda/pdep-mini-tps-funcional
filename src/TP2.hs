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
promoRecarga montoPagado celular = (Celular (linea celular) ((montoFinalDeRecarga celular) (saldo celular) montoPagado) (proveedor celular))

recargaSinPromo :: Int -> Celular -> Celular
recargaSinPromo montoPagado celular = (Celular (linea celular) (sinPromo (saldo celular) montoPagado) (proveedor celular))

montoFinalDeRecarga :: Celular -> (Int -> Int -> Int)
montoFinalDeRecarga celular
 | ((proveedor celular) == "Movistar") && ((codigoAreaLinea (linea celular)) == "011") = promoMovistar
 | ((proveedor celular) == "Personal") = promoPersonal
 | otherwise = sinPromo

codigoAreaLinea :: (String, String) -> String
codigoAreaLinea (codigoArea, _) = codigoArea

promoMovistar :: Int -> Int -> Int
promoMovistar saldoActual montoPagado = saldoActual + 3 * montoPagado

promoPersonal :: Int -> Int -> Int
promoPersonal saldoActual montoPagado = saldoActual + montoPagado + (min montoPagado 100)

sinPromo :: Int -> Int -> Int
sinPromo saldoActual montoPagado = saldoActual + montoPagado