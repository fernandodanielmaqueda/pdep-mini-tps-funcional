module TP4 where
import ContenidoAudiovisual

tieneHashtag :: String -> Video -> Bool
tieneHashtag = flip (flip elem.hashtags)

minutosTotalesConHashtag :: String -> Playlist -> Int
minutosTotalesConHashtag hashtag = sum.map (\video -> minutos video).filter (tieneHashtag hashtag).videos

estanRelacionados :: Video -> Video -> Bool
estanRelacionados video1 = any (== True).map ((flip tieneHashtag) video1).hashtags

recomendable :: Video -> Playlist -> Bool
recomendable videoNuevo = (>= cantidad).length.filter (estanRelacionados videoNuevo).videos
    where
        cantidad = 2

agregarVideosRecomendados :: [Video] -> Playlist -> Playlist
agregarVideosRecomendados = flip (foldr agregarUnVideoRecomendado)

agregarUnVideoRecomendado :: Video -> Playlist -> Playlist
agregarUnVideoRecomendado = agregarAPlaylistSoloSi recomendable