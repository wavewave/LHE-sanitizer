import System.Environment
import System.IO
--
import HEP.Parser.LHE.Sanitizer.Shuffle 


main :: IO ()
main = do 
  args <- getArgs 
  let inputlhe = args !! 0 
      outputlhe = "/dev/stdout"
  sanitizeLHEFile_shuffle inputlhe outputlhe 

