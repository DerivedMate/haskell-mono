import Control.Monad
import System.IO


type Accumulator = (Handle, String)
type Reduced = String
go :: (Foldable t, Monad m) 
    => (Accumulator -> Reduced -> m Accumulator) -> Accumulator -> t Reduced -> m Accumulator
go = undefined
main :: IO ()
main = do
    fh <- openFile "user.tsv" ReadMode
    let 
        f = 
    in go f (fh, )