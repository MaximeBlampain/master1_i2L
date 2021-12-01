import qualified Data.ByteString.Char8 as C
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as T

main::IO()
main = do
    -- lire text1.hs en ByteString
    file <- C.readFile "text3.hs"
    -- convertir en String
    let contents = E.decodeUtf8 file
    -- afficher
    T.putStrLn contents