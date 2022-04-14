module TP2 where

------------------------------------------
-- Modelo inicial
------------------------------------------

data Celular = Celular {
  linea :: (String, String),
  saldo :: Int,
  proveedor :: String
} deriving (Show, Eq)

prueba :: Celular
prueba = Celular {
  linea = ("011", "1234-5678"),
  saldo = 100,
  proveedor = "Movistar"
}
------------------------------------------

promoRecarga :: Int -> Celular -> Celular
promoRecarga montoPagado celular = (Celular (linea celular) ((promociones celular) (saldo celular) montoPagado) (proveedor celular))

promociones :: Celular -> (Int -> Int -> Int)
promociones celular
 | ((proveedor celular) == "Movistar") && ((fst (linea celular)) == "011") = promoMovistar
 | ((proveedor celular) == "Personal") = promoPersonal
 | otherwise = sinPromo

promoMovistar :: Int -> Int -> Int
promoMovistar saldoActual montoPagado = saldoActual + 3 * montoPagado

promoPersonal :: Int -> Int -> Int
promoPersonal saldoActual montoPagado = saldoActual + montoPagado + (min montoPagado 100)

sinPromo :: Int -> Int -> Int
sinPromo saldoActual montoPagado = saldoActual + montoPagado