
-- TODO Jour
data Jour = Lundi | Mardi | Mercredi | Jeudi | Vendredi | Samedi | Dimanche

-- estWeekend :: Jour -> Bool
estWeekend :: Jour -> Bool
estWeekend Samedi || Dimanche = True
estWeekend _ = False

-- compterOuvrables :: [Jour] -> Int
compterOuvrables :: [Jour] -> Int
compterOuvrables [] = 0
compterOuvrables (h:t) | estWeekend(h) = 1 + compterOuvrables(t)
        | otherwise compterOuvrables(t)

main :: IO ()
main = putStrLn "TODO"


