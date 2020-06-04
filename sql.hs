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
strip l = takeWhile (oneOf (/=)) $ dropWhile (oneOf (==)) l
    where 
        oneOf f c = all (f c) striped
        striped = [' ', '\r', '\\']

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

isInt :: String -> Bool
isInt ('-':n) = isInt n
isInt n = all isDigit n

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
    | isDate f = QDate
    | isInt f = QInt
    | isDecimal f = 
        let 
            [p, q] = map length (split '.' f []) 
        in QDecimal (p+q) q
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
merge (a, b) = a
    

mergeTypes :: Types -> Types -> Types
mergeTypes old new = map merge $ zip old new

getTypes :: Char -> [String] -> Types -> Types
getTypes _ [] ts = ts
getTypes sep (l:ls) ts = getTypes sep ls (mergeTypes ts newTypes)
    where 
        newTypes = typesOfLine sep (strip l) :: Types

processFile :: Char -> [String] -> IO (Fields, Types)
processFile sep ls = return (fields, ts)
    where 
        (headerLine : entries) = ls
        fields = fieldsOfLine sep headerLine
        ts = getTypes sep entries (replicate (length fields) QNone)
        -- ts = map (fieldsOfLine sep) entries

makeSql :: Char -> String -> String -> String -> (Fields, Types) -> String
makeSql sep filePath dbName tableName (fs, ts) = initLine ++ (Data.List.intercalate ",\n\t" $ map run rows) ++ endLine
    where 
        rows = zip fs ts :: [(String, Type)]
        run (field, tp) = field ++ " " ++ (show tp) ++ key
            where 
                key = if isId field then " primary key" else ""
        initLine = printf 
            "create database if not exists %s;\nuse %s;\ncreate table %s (\n\t" 
            dbName dbName tableName :: String
        endLine = printf "\n);\nset global local_infile = True;\nload data local infile '%s' into table %s fields terminated by '%c' ignore 1 lines;\n" 
            filePath tableName sep


-- Todo: rewrite to a do-block
insertCsv :: FilePath -> Char -> String -> IO ()
insertCsv path sep dbName = lsM >>= runner >>= (\a -> putStrLn $ makeSql sep path dbName tableName a)
    where
        runner ls = processFile sep ls :: IO (Fields, Types)
        f = openFile path ReadMode -- "data/oceny.txt"
        tableName = reverse $ takeWhile (/='/') $ drop 1 $ dropWhile (/='.') (reverse path)
        lsM = f 
            >>= hGetContents 
            >>= (return . map toLower) 
            >>= (\c -> return $ map strip (lines c)) :: IO [String]