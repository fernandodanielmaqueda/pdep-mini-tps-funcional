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

recargarCelular :: (Int -> Int -> Int) -> Int -> Celular -> Celular
recargarCelular montoFinalDeRecarga montoPagado celular = (Celular (linea celular) (montoFinalDeRecarga (saldo celular) montoPagado) (proveedor celular))

promoRecarga :: Int -> Celular -> Celular
promoRecarga montoPagado celular = recargarCelular (calculoMontoRecarga celular) montoPagado celular

recargaSinPromo :: Int -> Celular -> Celular
recargaSinPromo montoPagado celular = recargarCelular sinPromo montoPagado celular

calculoMontoRecarga :: Celular -> (Int -> Int -> Int)
calculoMontoRecarga celular
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