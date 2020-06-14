module LineReader (foldFile, foldFileN) where 
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


foldFileN :: (a -> Line -> a) -> Handle -> a -> Int -> IO a
foldFileN f fh acc0 n = hGetLine fh 
    >>= goN f fh acc0 n 
    where 
        goN f fh acc n line = do
            finish <- hIsEOF fh
            if finish || n <= 0 
                then return acc
            else hGetLine fh 
                >>= (seq acc' . goN f fh acc' (n-1))
                where acc' = f acc line