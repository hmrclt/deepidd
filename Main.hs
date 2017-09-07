import Text.Pandoc
import Data.ByteString.Lazy as BL (readFile)
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.List.Split
import CamelCase
import Data.Char (isAlphaNum)


fieldTypeMap :: String -> String
fieldTypeMap "CHAR" = "String"
fieldTypeMap "AD_PSTCD1" = "String"
fieldTypeMap "AD_SMTPADR" = "String"
fieldTypeMap "LAND1" = "String"
fieldTypeMap "DATS" = "Date"
fieldTypeMap _ = "Any"

beginsWith :: Eq a => [a] -> [a] -> Bool
beginsWith [] _ = True
beginsWith _ [] = False
beginsWith a b = and $ zipWith (==) a b

colMap :: String -> String
colMap x | x `beginsWith` "OPTION" = "OPTION"
colMap x | x `beginsWith` "FIELD" = "FIELD"
colMap x | x `beginsWith` "DATA TYPE" = "DATA TYPE"
colMap x = x

opt :: Bool -> String -> String
opt True a = "Option[" ++ a ++ "]"
opt False a = a

plain :: [Block] -> String
plain b = writePlain def (Pandoc nullMeta b)

field :: M.Map String String -> String
field fieldMap = "  " ++ name ++ ": " ++ ftype
  where name = toCamelCase $ fieldMap M.! "FIELD"
        ftype = opt isOpt $ fieldTypeMap $ fieldMap M.! "DATA TYPE"
        isOpt = fieldMap M.! "OPTION" == "Optional"

caseClass :: String -> [M.Map String String] -> String
caseClass x _ | filter isAlphaNum x == "" = "\t"
caseClass title fields = "case class " ++ toCamelCase' title  ++ " (\n" ++
  intercalate ",\n" (fmap field filterFields) ++ "\n)"
  where filterFields = filter (\a ->  M.findWithDefault "" "FIELD" a /= "") fields

applyHeaders :: [String] -> [[String]] -> [M.Map String String]
applyHeaders headers = fmap zipWithHeaders
  where zipWithHeaders i  = M.fromList $ zip (fmap colMap headers) i

extract :: [String] -> [[String]] -> [String]
extract headers body = fmap (\(n:b) -> caseClass (unwords n) (applyHeaders headers b)) chunks 
  where chunks       = supersplit body
        supersplit   = (split.dropInitBlank.keepDelimsL.whenElt) (\x -> length x == 1)

deepiddTab :: Block -> Maybe String
deepiddTab (Table _ _ _ _ (headers:cells)) =
  if "DATA TYPE" `elem` niceHeaders
  then Just $ intercalate "\n\n" $ extract niceHeaders (["Unknown"] : niceCells)
  else Nothing
  where niceHeaders = fmap plain headers
        niceCells   = (fmap.fmap) plain cells
deepiddTab _        = Nothing

deepidd :: Pandoc -> String
deepidd (Pandoc _ blocks) = intercalate "\n\n" $ catMaybes $ fmap deepiddTab blocks

main :: IO ()
main = do
  f <- readDocx def <$> BL.readFile "spec.docx"
  case f of Right (a,_) -> putStrLn $ deepidd a
            Left e -> print e
