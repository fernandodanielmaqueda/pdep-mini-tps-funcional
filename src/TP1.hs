module TP1 where

esMes :: Int -> Bool
esMes mes = (mes >= 1) && (mes <= 12)

hayCambioDeEstacion :: Int -> Bool
hayCambioDeEstacion mes = ((mod mes 3) == 0) && (esMes mes)

estacion :: Int -> String
estacion mes
 | ((mes == 1) || (mes == 2)) = "verano"
 | ((mes == 4) || (mes == 5)) = "otonio"
 | ((mes == 7) || (mes == 8)) = "invierno"
 | ((mes == 10) || (mes == 11)) = "primavera"
 | (hayCambioDeEstacion mes) = (estacionMesAnterior mes) ++ "/" ++ (estacionMesSiguiente mes)

estacionMesAnterior :: Int -> String
estacionMesAnterior mes
 | (mes == 1) = estacion 12
 | (esMes mes) = estacion (mes - 1)

estacionMesSiguiente :: Int -> String
estacionMesSiguiente mes
 | (mes == 12) = estacion 1
 | (esMes mes) = estacion (mes + 1)