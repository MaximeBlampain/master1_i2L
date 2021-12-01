import qualified Data.Text.Lazy.IO as L
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T

main::IO()
main = do
    -- lire text1.hs en ByteString
    file <- T.readFile "text4.hs"
    -- convertir en String
    let contents = L.fromStrict file
    -- afficher
    L.putStrLn contents