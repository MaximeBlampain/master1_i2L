import qualified Data.ByteString.Char8 as C
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as T

main::IO()
main = do
    -- lire text1.hs en ByteString
    file <- T.readFile "text4.hs"
    -- convertir en String
    let contents = E.encodeUtf8 file
    -- afficher
    C.putStrLn contents