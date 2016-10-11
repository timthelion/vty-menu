{- This executables sole purpose is getting arround  http://hackage.haskell.org/trac/ghc/ticket/7789 -}
import Graphics.Vty.Menu
import System.Environment
import System.IO

main = do
 { args <- getArgs
 ; ansM <- displayMenu args
 ; let
    ans =
     case ansM of
      Nothing -> ""
      Just ans' -> ans'
 ; hPutStrLn stderr ans }
