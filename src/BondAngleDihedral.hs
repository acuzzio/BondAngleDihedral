import System.ShQQ
import System.Environment
import Data.List.Split
import Control.Applicative

errorHelpMessage = "This scripts works with 2, 3 or 4 argouments.\nYou have to launch it in a folder that contains xyz geometry or dynamics files and then give the index of 2,3 or 4 atoms.\nExample:\n\n $ BondAngleDihedral 1 2  <- will give the bond length between atoms 1 and 2\n $ BondAngleDihedral 4 2 6   <- will give the angle between atoms 4,2 and 6\n $ BondAngleDihedral 4 2 6 10 <- will give the dihedral between atoms 4 2 6 and 10\n\n"

data Geometry = Geometry {getAtomnumber :: Int
                         ,getGeometry   :: [Atom]
                         } deriving Show

data Atom     = Atom     {getAtomtype   :: String
                         ,getCoords     :: Vec Double
                         } deriving Show

newtype Vec a = Vec {runVec :: [a]} deriving (Show, Read, Eq)

main = do 
  arg <- getArgs
  files <- readShell "ls"
  let fileList = lines files
      zyx      = map (take 4) $ map reverse fileList -- dull but works -> to filter xyz files
      xyzIndex = map fst $ filter (\x-> snd x == "zyx.") $ zip [0..] zyx
      fileList3= map (fileList!!) xyzIndex
      fileList2= filter (\x -> take 13 (reverse x) /= "zyx.yticolev.") fileList3
      aI    = map read2 arg
  case length xyzIndex of
     0    -> putStrLn $ "\n\nUse this script in a folder with xyz files !!!\n\n\n\n" ++ errorHelpMessage
     otherwise -> case length arg of
                   2    -> do mapM_ (calculateMe bond aI "Bond") fileList2
                   3    -> do mapM_ (calculateMe angle aI "Angle") fileList2 
                   4    -> do mapM_ (calculateMe dihedral aI "Dihedral") fileList2
                   otherwise  -> putStrLn $ "\nNAH !!\n\n" ++ errorHelpMessage
--                   otherwise  -> do mapM_ (calculateMe bond [3,4] "Bond") fileList2

calculateMe fun atomL funLabel file = do 
   a <- readXyz file
   let aa     = transformInCoord a
       label  = transformInAtomT a
       labelA = map (getRightAtoms atomL) label
       labelS = zipWith (++) (head labelA) (map show atomL)
       resu   = map (fun . getRightAtoms atomL) aa
   putStrLn $ file ++ " -> " ++ funLabel ++ " " ++ (unwords labelS) ++ "\n"
   putStrLn $ unlines $ map show resu

transformInCoord :: [Geometry] -> [[Vec Double]]
transformInCoord a = map (map getCoords) $ map getGeometry a

transformInAtomT :: [Geometry] -> [[String]]
transformInAtomT a = map (map getAtomtype) $ map getGeometry a

getRightAtoms :: [Int] -> [a] -> [a]
getRightAtoms indexlist atomlist = let 
               rightindex = map pred indexlist
               in map (\x -> atomlist!!x) rightindex

readXyz :: FilePath -> IO [Geometry]
readXyz name = do
        a <- readFile name
        let aa = map words $ lines a
            atomnumber = read2 $ head.head $ aa
            linenumber = atomnumber+2
            split      = chunksOf (linenumber) aa 
            rightSplit = filter (\x -> length x == linenumber) split
            geometries = map readGeom rightSplit
        return geometries

readGeom :: [[String]] -> Geometry
readGeom atom = let atomnumber = read2 $ head.head $ atom
                    atoms      = map readAtom $ tail $ tail atom
                in Geometry atomnumber atoms

readAtom :: [String] -> Atom
readAtom atomL = let label  = atomL !! 0
                     coords = map read3 $ map (atomL!!) [1..3]
                 in Atom label (Vec coords)

read2 x = read x :: Int

read3 x = read x :: Double

-- ================> Internal coords <==============

instance Functor Vec where
  fmap f (Vec v) = Vec $ f `fmap` v

instance Applicative Vec where
  pure x = Vec $ (repeat x)
  Vec fs <*> Vec xs = Vec $ Prelude.zipWith (\f x -> f x) fs xs

instance Num a => Num (Vec a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-) 
  (*)         = liftA2 (*) 
  abs         = liftA abs 
  signum      = liftA signum
  fromInteger = pure . fromInteger

instance (Fractional a) => Fractional (Vec a) where
   (/)  = liftA2 (/) 
   recip v = recip <$> v
   fromRational = pure . fromRational

instance (Floating a) => Floating (Vec a) where
  pi = pure pi
  exp v = liftA exp v
  log v = liftA exp v
  sqrt v = liftA sqrt v
  (**)   = liftA2 (**)
  sin v =  sin <$> v
  cos v =  cos <$> v
  tan v =  tan <$> v
  asin v = asin <$> v
  acos v = acos <$> v
  atan v = atan <$> v
  sinh v = sinh <$> v
  cosh v = cosh <$> v
  tanh v = tanh <$> v
  asinh v = asinh <$> v
  acosh v = acosh <$> v
  atanh v = atanh <$> v

toVec :: [a] -> Vec a
toVec x = Vec x

-- ============> Methods <============

vecdot :: Num a => Vec a -> Vec a -> a
v1 `vecdot` v2 =  sum . runVec $ v1 * v2

vecCross :: Num a => Vec a -> Vec a -> Vec a
v1' `vecCross` v2' = let [[x,y,z],[u,v,t]]= fmap runVec [v1',v2']
                     in Vec $ [(y*t-v*z),(u*z-x*t),(x*v-u*y)]

vecscal :: Num a => a -> Vec a -> Vec a
x `vecscal` vec = (pure x) * vec

vecnorm :: Vec Double -> Double
vecnorm v =  sqrt  $  v `vecdot` v

-- =================> BOND, ANGLE AND DIHEDRAL <=========

bond :: [Vec Double] -> Double
bond [p1,p2] = vecnorm $ p1 - p2

angle :: [Vec Double] -> Double
angle [p1,p2,p3] = let 
      ab = p1 - p2
      bc = p3 - p2
      numerator = vecdot ab bc
      denominat = (vecnorm ab) * (vecnorm bc)
      fromRadToGrad = 180.0/pi
      gradiant  = acos (numerator/denominat)
      in fromRadToGrad * gradiant

dihedral :: [Vec Double] -> Double
dihedral [p1,p2,p3,p4] =
  let [xba,xca,xcb,xdb] = zipWith (-) [p2,p3,p3,p4] [p1,p1,p2,p2]
      [w1,w2] = zipWith vecCross [xba,xcb] [xca,xdb]
      [n1,n2] = map vecnorm [w1,w2]
      teta = (180.0/pi*) . acos $ ((w1 `vecdot` w2) / (n1*n2))
        in case 0.0 > (signum $ w2 `vecdot` xba) of
                True -> -teta
                False -> teta


