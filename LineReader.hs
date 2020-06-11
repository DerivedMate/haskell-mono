module LineReader (foldFile) where 
import Control.Monad
import System.IO


type Line = String
type GoFunc a = (a -> Line -> a)
go :: GoFunc a
      -> Handle 
      -> a
      -> Line 
      -> IO a
go f fh acc line = do
    finish <- hIsEOF fh
    if finish then return acc
    else hGetLine fh >>= (seq acc' . go f fh acc')
    where acc' = f acc line

foldFile :: GoFunc a -> Handle -> a -> IO a
foldFile f fh acc0 = hGetLine fh >>= go f fh acc0 

main :: IO ()
main = do
    fh <- openFile "user.tsv" ReadMode
    foldFile (\a b -> b) fh "" >>= putStrLn