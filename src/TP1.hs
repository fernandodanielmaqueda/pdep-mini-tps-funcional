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
 | (hayCambioDeEstacion mes) = estacion((mesAnterior mes)) ++ "/" ++ estacion((mesSiguiente mes))

mesAnterior :: Int -> Int
mesAnterior mes
 | (mes == 1) = 12
 | (esMes mes) = (mes - 1)

mesSiguiente :: Int -> Int
mesSiguiente mes
 | (mes == 12) = 1
 | (esMes mes) = (mes + 1)