module TP3 where
import Cafe

prepararFrapuccino :: Gramos -> Cafe
prepararFrapuccino = (servirEnVaso 400).(licuar 60 120).(agregarHielo 6).(prepararCafe 80).molerGranos

prepararCafeDeFiltro :: Vaso -> Gramos -> Cafe
prepararCafeDeFiltro tamanioVaso = (servirEnVaso tamanioVaso).(prepararCafe 1000).molerGranos