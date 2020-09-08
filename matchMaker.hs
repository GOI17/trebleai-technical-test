import Data.Char


data Answer =
    Digit Int
  | SingleWord String

{----------------------------------------------}
{--------------- EXACT MATCH ------------------}
{----------------------------------------------}
exactMatch :: String -> [String] -> Maybe String

{----------- Here goes your code --------------}
getWordsFromList :: Int -> String -> String -> [String] -> String
getWordsFromList i currentValue searchText list =
  if i < length list
  then
    if searchText `elem` words (map toLower currentValue)
    then currentValue
    else getWordsFromList (i + 1) (list !! i) searchText list
  else currentValue


exactMatch text list =
  if length text == 0 || length list == 0
    then Nothing
    else
      if containedInSentence 0 "" text list 0 == 1
      then Just (getWordsFromList 0 "" text list)
      else Nothing


{----------------------------------------------}
{------ CONTAINED IN JUST ONE SENTENCE --------}
{----------------------------------------------}
containedInSentence :: Int -> String -> String -> [String] -> Int -> Int

{----------- Here goes your code --------------}

containedInSentence i currentValue searchText list moreThanOne =
  if i < length list
  then
    if searchText `elem` words (map toLower currentValue)
    then containedInSentence (i + 1) (list !! i) searchText list (moreThanOne + 1)
    else containedInSentence (i + 1) (list !! i) searchText list moreThanOne
  else moreThanOne


{----------------------------------------------}
{----------------- MATCH ----------------------}
{----------------------------------------------}
match :: Answer -> [String] -> Maybe String

{----------- Here goes your code --------------}
getByIndex :: Int -> [String] -> Maybe String
getByIndex i list =
  if i < length list
  then Just (list !! i)
  else Nothing

match query list = case query of
  SingleWord query -> exactMatch query list
  Digit query -> getByIndex query list

{----------------------------------------------}
{------------------ Usage ---------------------}
{----------------------------------------------}
--ans = match (SingleWord "hola") ["Muy bien", "Hola Si", "Hola mundo", "Quiero saber de mas"]
ans = match (Digit 2) ["Muy bien", "Hola Si", "Hola mundo", "Quiero saber de mas"]

main :: IO ()
main = print ans
