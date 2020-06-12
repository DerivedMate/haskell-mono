{-
Issues:
1. mistyped fields
-}
insertCsv :: String -> Char -> FilePath -> IO (String)
insertCsv dbName sep path =
    makeSql sep path dbName tableName 
    <$> fields 
    <*> types
    where 
        tableName = reverse 
            $ takeWhile (/='/') 
            $ tail 
            $ dropWhile (/='.') (reverse path)
        f = openFile path ReadMode
        fields = f 
            >>= hGetLine  
            >>= return 
            . map strip
            . fieldsOfLine sep  
            . map toLower :: IO Fields
        types = 
            join
            $ extractTypes sep
            <$> f
            <*> fields :: IO Types