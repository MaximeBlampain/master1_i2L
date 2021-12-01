import Data.Char (toUpper)
import Data.List.Split (splitOn)

data User = User
    { _name :: String
    , _email :: String
    } deriving Show

-- parseUser :: String -> Maybe User

-- upperize :: User -> User

main :: IO ()
main = putStrLn "TODO"

