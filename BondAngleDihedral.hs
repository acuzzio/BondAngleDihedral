import System.ShQQ
import System.Environment

errorHelpMessage = "\nNAH !!!!\n\nThis scripts works with 2, 3 or 4 argouments.\nYou have to launch it in a folder that contains xyz geometry or dynamics files and then give the index of 2,3 or 4 atoms.\nExample:\n\n $ BondAngleDihedral 1 2  <- will give the bond length between atoms 1 and 2\n $ BondAngleDihedral 4 2 6   <- will give the angle between atoms 4,2 and 6\n $ BondAngleDihedral 4 2 6 10 <- will five the dihedral between atoms 4 2 6 and 10\n\n"

main = do 
       arg <- getArgs
       case length arg of
          2 -> print "UNO"
          3 -> print "due"
          4 -> print "tre"
          otherwise -> putStrLn errorHelpMessage
