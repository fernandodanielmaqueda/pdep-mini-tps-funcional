module TP1 where

esMes :: Int -> Bool
esMes mes = (mes >= 1) && (mes <= 12)

hayCambioDeEstacion :: Int -> Bool
hayCambioDeEstacion mes = ((mod mes 3) == 0) && (esMes mes)

estacion :: Int -> String
estacion mes
 | esMes mes = estaciones mes

estaciones :: Int -> String
estaciones mes
 | ((mes == 1) || (mes == 2)) = "verano"
 | ((mes == 4) || (mes == 5)) = "otonio"
 | ((mes == 7) || (mes == 8)) = "invierno"
 | ((mes == 10) || (mes == 11)) = "primavera"
 | (mes == 12) = "primavera/verano"
 | otherwise = (estaciones (mes - 1)) ++ "/" ++ ((estaciones (mes + 1)))