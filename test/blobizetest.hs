import System.Environment
import System.IO
--
import HEP.Parser.LHE.Sanitizer.Action 


main :: IO ()
main = do 
  args <- getArgs 
  let inputlhe = args !! 0 
      -- outputlhe = "/dev/stdout"
      outputlhe = "ttt.lhe"
  blobize inputlhe outputlhe 

