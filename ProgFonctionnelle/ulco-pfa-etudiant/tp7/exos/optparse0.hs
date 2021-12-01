import Options.Applicative

data Args = Args
    { hello      :: String
    , val1       :: Int
    } deriving (Show)

argsP :: Parser Args
argsP = Args
      <$> strOption
          ( long "hello"
         <> help "Target for the greeting"
         <> metavar "TARGET")
      <*> option auto
          ( long "val1"
         <> help "Value 1"
         <> metavar "INT"
         <> value 1)

myinfo :: ParserInfo Args
myinfo = info (argsP <**> helper)
              (fullDesc <> header "This is my cool app!")

main :: IO ()
main = execParser myinfo >>= print

