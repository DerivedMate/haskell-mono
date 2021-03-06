module Main where
import Data.Char
import Data.List
import System.IO
import Control.Monad
import System.Environment   
import Text.Printf
import LineReader

type Fields = [String]
data Type = 
    QNone
    | QDecimal Int Int 
    | QTInt
    | QSInt
    | QMInt
    | QInt 
    | QBInt
    | QText 
    | QVChar Int
    | QChar Int
    | QTime
    | QDate 
    | QDateTime
    deriving (Eq, Ord)
type Types = [Type]
instance Show Type where 
    show QNone = ""
    show (QDecimal p q) = printf "decimal(%i, %i)" p q
    show QInt = "int"
    show QTInt = "tinyint"
    show QSInt = "smallint"
    show QMInt = "mediumint"
    show QBInt = "bigint"
    show QText = "text"
    show (QVChar n) = "varchar(" ++ (show n) ++ ")"
    show (QChar n) = "char(" ++ (show n) ++ ")"
    show QTime = "time"
    show QDate = "date"
    show QDateTime = "datetime"

(<|>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<|>) a b c = a c || b c

(<&>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<&>) f g l = f l && g l

split :: Char -> String -> [String] -> [String]
split t "" fs = reverse fs
split t l fs = split t lr (f:fs)
    where
        (f, lr_) = break (==t) l
        lr = drop 1 lr_

strip :: String -> String
strip l =  filter 
    (foldl1 
        (<&>) 
        $ map (/=) ['\r', '\n']
    ) l
    -- where 
        -- not $ elem c toStrip
        -- toStrip = ['\r']
    {-takeWhile (oneOf (/=)) $ dropWhile (oneOf (==)) l
    where 
        oneOf f c = all (f c) striped
        striped = [' ', '\r', '\\']}-}
linesOfContent :: String -> [String]
linesOfContent = map strip . lines

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
        parts =  map removeSign $ split ':' l []
        matches = [isLen 3 <|> isLen 2, isLen 2, isLen 2]

isInt :: String -> Bool
isInt n = all isDigit (removeSign n)

hasSpaces :: String -> Bool
hasSpaces l = any (==' ') l

isText :: String -> Bool
isText l = length l > 255 && any isLetter l

isChar :: String -> Bool
isChar l = length l <= 255 
    && (any (not . (isDigit <|> (=='.'))) <|> hasSpaces) l

isDecimal :: String -> Bool
isDecimal l = numberIsh && twoPart
    where 
        parts = split '.' l []
        numberIsh = all (all isDigit) parts
        twoPart = length parts == 2

isDateTime :: String -> Bool
isDateTime l = isLen2 && (all merger $ zip parts matches)
    where
        merger (p, m) = m p
        parts = split ' ' l []
        isLen2 = length parts == 2
        matches = [isDate, isTime]

isId :: String -> Bool
isId "id" = True
isId ('i':'d':_) = True
isId l = 
    let lRev = reverse l
    in case lRev of
        ('d':'i':_) -> True
        otherwise -> False
            
typeOfField :: String -> Type
typeOfField f_
    | "" == f = QNone
    | isInt f = let g = abs $ read f 
        in if g <= 1 then QTInt else QInt
        {-in if   g <= 127         then QTInt
        else if g <= 32767       then QSInt
        else if g <= 8388607     then QMInt
        else if g <= 2147483647  then QInt
        else                     QBInt-}
    | isDecimal f = 
        let [p, q] = map length (split '.' f []) 
        in QDecimal (p+q) q
    | isDate f = QDate
    | isTime f = QTime
    | isDateTime f = QDateTime
    | isText f = QText
    | isChar f = QChar (length f)
    | otherwise = QNone
    where f = strip f_

-- END: type checkers
typesOfLine :: Char -> String -> Types
typesOfLine sep l = map typeOfField fields
    where 
        fields = fieldsOfLine sep l
    
merge :: Type -> Type -> Type
merge QNone new = new
merge _ QText = QText
merge (QChar a) (QChar b) = 
    if a /= b 
        then QVChar (max a b) 
    else QChar b
merge (QVChar a) (QChar b) = QVChar (max a b)
merge (QVChar a) (QVChar b) = QVChar (max a b) 
merge (QDecimal p q) QInt = QDecimal p q
merge QInt (QDecimal p q) = QDecimal p q
merge QTInt QInt = QInt
merge a _ = a
    

mergeTypes :: Types -> Types -> Types
mergeTypes old new = zipWith merge old new

makeSql :: Char -> String -> String -> String -> Fields -> Types -> String
makeSql sep filePath dbName tableName fs ts = 
    initLine 
    ++ (intercalate ",\n\t" lns) 
    ++ endLine
    where 
        rows = zip fs ts :: [(String, Type)]
        (_, lns) = mapAccumL run False rows 
        run hadId (field, tp) = (isPrimary || hadId, str)
            where 
                hasId = isId field
                isPrimary = (not hadId) && hasId
                key = 
                    if isPrimary then   " primary key" 
                    else if hasId then  " not null" 
                    else                ""
                str = field ++ " " ++ (show tp) ++ key
        initLine = printf 
            "create table if not exists `%s` (\n\t" 
            tableName
        endLine = printf "\n);\nload data local infile '%s' into table `%s` fields terminated by '%c' ignore 1 lines;\n" 
            filePath tableName sep

extractTypes :: Handle -> Char -> Fields -> IO Types
extractTypes fh sep fields = foldFileN go fh acc 500000
    where 
        acc = replicate (length fields) QNone
        go ts line = mergeTypes ts $ typesOfLine sep line

insertCsv :: String -> Char -> FilePath -> IO (String)
insertCsv dbName sep path = do
    f <- openFile path ReadMode
    fields <- hGetLine f 
        >>= return 
        . fieldsOfLine sep 
        . strip 
        . map toLower 
    types <- extractTypes f sep fields
    return $ makeSql sep path dbName tableName fields types
    where 
        tableName = reverse 
            $ takeWhile (/='/') 
            $ tail 
            $ dropWhile (/='.') (reverse path)

readPaths :: FilePath -> IO [String]
readPaths src =
    openFile src ReadMode
    >>= hGetContents 
    >>= (return . linesOfContent)

codifyPath :: String -> Char -> String -> IO ()
codifyPath dbName sep p = 
    insertCsv dbName sep p 
    >>= putStrLn . ("\n"++)

main :: IO ()
main = do
    [path, sep_, dbName] <- getArgs
    sep <- return $ head sep_
    paths <- readPaths path
    printf "create database if not exists %s;\nuse %s;\nset global local_infile = True;\n\n" dbName dbName
    forM_ paths (codifyPath dbName sep)
    return ()