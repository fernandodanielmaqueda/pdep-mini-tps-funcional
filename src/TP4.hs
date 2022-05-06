module TP4 where
import ContenidoAudiovisual

tieneHashtag :: String -> Video -> Bool
tieneHashtag hashtag video = elem hashtag (hashtags video)

minutosTotalesConHashtag :: String -> Playlist -> Int
minutosTotalesConHashtag hashtag playlist = (foldl (+) 0.map (\video -> minutos video).filter (tieneHashtag hashtag)) (videos playlist)

estanRelacionados :: Video -> Video -> Bool
estanRelacionados video1 video2 = (any (== True).map (\hashtag -> any (== hashtag) (hashtags video1))) (hashtags video2)

recomendable :: Video -> Playlist -> Bool
recomendable videoNuevo playlist = ((>= cantidad).length.filter (\videoPlaylist -> estanRelacionados videoNuevo videoPlaylist)) (videos playlist)
    where
        cantidad = 2

agregarVideosRecomendados :: [Video] -> Playlist -> Playlist
agregarVideosRecomendados videosNuevos playlist = foldr (\videoNuevo playlist -> agregarAPlaylistSoloSi recomendable videoNuevo playlist) playlist videosNuevos