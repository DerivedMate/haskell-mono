module Main where
import Data.Char
import Data.List
import System.IO
import Control.Monad
import System.Environment   
import Text.Printf

type Fields = [String]
data Type = 
    QNone
    | QDecimal Int Int 
    | QInt 
    | QText 
    | QVChar Int
    | QChar Int
    | QTime
    | QDate 
    deriving (Eq)
type Types = [Type]
instance Show Type where 
    show QNone = ""
    show (QDecimal p q) = printf "decimal(%i, %i)" p q
    show QInt = "int"
    show QText = "text"
    show (QVChar n) = "varchar(" ++ (show n) ++ ")"
    show (QChar n) = "char(" ++ (show n) ++ ")"
    show QTime = "time"
    show QDate = "date"

split :: Char -> String -> [String] -> [String]
split t "" fs = reverse fs
split t l fs = split t lr (f:fs)
    where
        (f, lr_) = break (==t) l
        lr = drop 1 lr_

strip :: String -> String
strip l = filter (/='\r') l
    -- where 
        -- not $ elem c toStrip
        -- toStrip = ['\r']
    {-takeWhile (oneOf (/=)) $ dropWhile (oneOf (==)) l
    where 
        oneOf f c = all (f c) striped
        striped = [' ', '\r', '\\']}-}
removeSign :: String -> String
removeSign ('-':l) = l
removeSign ('+':l) = l
removeSign l = l

fieldsOfLine :: Char -> String -> Fields
fieldsOfLine t l = split t l []

-- BEGIN: Type checkers
isLen :: Int -> String -> Bool
isLen n l = (length l) == n

isDate :: String -> Bool
isDate l = all merger (zip parts matches)
    where
        merger (p, m) = m p && all isDigit p
        parts = split '-' l []
        matches = [isLen 4, isLen 2, isLen 2]

isTime :: String -> Bool 
isTime l = all merger (zip parts matches)
    where
        merger (p, m) = m p && all isDigit p
        parts = map removeSign $ split ':' l []
        matches = [isLen 3, isLen 2, isLen 2]

isInt :: String -> Bool
isInt n = all isDigit (removeSign n)

hasSpaces :: String -> Bool
hasSpaces l = any (==' ') l

isText :: String -> Bool
isText l = length l > 255 && any isLetter l

isChar :: String -> Bool
isChar l = length l <= 255 && (any (not . isDigit) l || hasSpaces l)

isDecimal :: String -> Bool
isDecimal l = numberIsh && twoPart
    where 
        numberIsh = all (\c -> isDigit c || c == '.') l
        twoPart = length (split '.' l [] ) == 2

isId :: String -> Bool
isId "id" = True
isId ('i':'d':_) = True
isId l = 
    let lRev = reverse l
    in case lRev of
        ('d':'i':_) -> True
        otherwise -> False
            
typeOfField :: String -> Type
typeOfField f
    | "" == f = QNone
    | isInt f = QInt
    | isDecimal f = 
        let [p, q] = map length (split '.' f []) 
        in QDecimal (p+q) q
    | isDate f = QDate
    | isTime f = QTime
    | isText f = QText
    | isChar f = QChar (length f)
    | otherwise = QNone

-- END: type checkers
typesOfLine :: Char -> String -> Types
typesOfLine sep l = map typeOfField fields
    where 
        fields = fieldsOfLine sep l
    
merge :: (Type, Type) -> Type
merge (QNone, new) = new
merge (_, QText) = QText
merge (QChar a, QChar b) = 
    if a /= b 
        then QVChar (max a b) 
    else QChar b
merge (QVChar a, QChar b) = QVChar (max a b)
merge (QVChar a, QVChar b) = QVChar (max a b) 
merge (QDecimal p q, QInt) = QDecimal p q
merge (QInt, QDecimal p q) = QDecimal p q
merge (a, _) = a
    

mergeTypes :: Types -> Types -> Types
mergeTypes old new = map merge $ zip old new

getTypes :: Char -> [String] -> Types -> Types
getTypes _ [] ts = ts
getTypes sep (l:ls) ts = getTypes sep ls (mergeTypes ts newTypes)
    where 
        newTypes = typesOfLine sep l :: Types

processFile :: Char -> [String] -> IO (Fields, Types)
processFile sep ls = return (fields, ts)
    where 
        (headerLine : entries) = ls
        fields = fieldsOfLine sep headerLine
        ts = getTypes sep entries (replicate (length fields) QNone)
        -- ts = map (fieldsOfLine sep) entries

makeSql :: Char -> String -> String -> String -> (Fields, Types) -> String
makeSql sep filePath dbName tableName (fs, ts) = 
    initLine 
    ++ (Data.List.intercalate ",\n\t" lns) 
    ++ endLine
    where 
        (_, lns) = foldl run (False, []) rows
        rows = zip fs ts :: [(String, Type)]
        run (hadId, acc) (field, tp) = (hasId || hadId, acc++[str])
            where 
                isAnId = isId field
                hasId = (not hadId) && isAnId
                key = 
                    if hasId 
                        then " primary key" 
                    else if isAnId 
                        then " not null" 
                    else ""
                str = field ++ " " ++ (show tp) ++ key
        initLine = printf 
            "create table if not exists %s (\n\t" 
            tableName
        endLine = printf "\n);\nload data local infile '%s' into table %s fields terminated by '%c' ignore 1 lines;" 
            filePath tableName sep

-- Todo: rewrite to a do-block (?)
insertCsv :: String -> Char -> FilePath -> IO (String)
insertCsv dbName sep path = 
    lsM 
    >>= runner 
    >>= (\a -> return $ makeSql sep path dbName tableName a)
    where
        runner ls = processFile sep ls :: IO (Fields, Types)
        f = openFile path ReadMode -- "data/oceny.txt"
        tableName = reverse $ takeWhile (/='/') $ drop 1 $ dropWhile (/='.') (reverse path)
        lsM = f 
            >>= hGetContents 
            >>= (return . map toLower) 
            >>= (\c -> return $ map strip (lines c)) :: IO [String]


readPaths :: FilePath -> IO [String]
readPaths src = fh >>= hGetContents >>= (\ls-> return $ map strip $ lines ls)
    where
        fh = openFile src ReadMode

outputCode :: String -> [IO String] -> IO ()
outputCode dbName ms = (forM ms id) >>= go
    where 
        go ps = 
            let code = intercalate "\n" ps
            in putStrLn $ printf 
            "create database if not exists %s;\nuse %s;\nset global local_infile = True;\n%s"
            dbName dbName code
        

main :: IO ()
main = do
    arr@[path, sep_, dbName] <- getArgs
    sep <- return $ (\(s:_) -> s) sep_
    paths <- readPaths path
    outputCode dbName $ map (insertCsv dbName sep) paths