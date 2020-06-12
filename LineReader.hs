module LineReader (foldFile) where 
import Control.Monad
import System.IO


type Line = String
go :: (a -> Line -> a)
      -> Handle 
      -> a
      -> Line 
      -> IO a
go f fh acc line = do
    finish <- hIsEOF fh
    if finish then return acc
    else hGetLine fh >>= (seq acc' . go f fh acc')
    where acc' = seq line $ f acc line

foldFile :: (a -> Line -> a) -> Handle -> a -> IO a
foldFile f fh acc0 = hGetLine fh >>= go f fh acc0 

main :: IO ()
main = do
    fh <- openFile "user.tsv" ReadMode
    foldFile (\a b -> b) fh "" >>= putStrLn