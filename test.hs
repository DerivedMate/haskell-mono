import Data.Char

f :: IO ()
f = y >>= putStrLn
  where 
    y = getLine >>= (return . map toLower)
