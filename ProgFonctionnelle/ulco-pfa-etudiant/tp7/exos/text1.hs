import qualified Data.ByteString.Char8 as C

main::IO()
main = do
    -- lire text1.hs en ByteString
    file <- C.readFile "text1.hs"
    -- convertir en String
    let contents = C.unpack file
    -- afficher
    putStrLn contents