import Site
import Album
main :: IO ()
main = do
    sitesE <- loadSites "data/genalbum.json"
    case sitesE of
        Left err -> putStrLn err
        Right sites -> genAlbum "out.html" sites 