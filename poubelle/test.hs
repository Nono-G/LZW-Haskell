import Data.Word
import qualified Data.ByteString.Lazy as BIN
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad
import System.IO
import Data.Word.Odd

x :: Word8
x = 12

succ :: Word8 -> Word8
succ x = x+1

y :: Word10
y=9

main :: IO()
main = do
	handleOut <- openFile "f.txt" WriteMode
	runPut
	hClose handleOut
