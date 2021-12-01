import qualified Data.Text as T
import qualified Data.Text.IO as T

main::IO()
main = do
    -- lire text1.hs en ByteString
    file <- T.readFile "text2.hs"
    -- convertir en String
    let contents = T.unpack file
    -- afficher
    putStrLn contents