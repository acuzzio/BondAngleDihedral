import System.ShQQ
import System.Environment
import Data.List.Split
import Control.Applicative

errorHelpMessage = "\nNAH !!!!\n\nThis scripts works with 2, 3 or 4 argouments.\nYou have to launch it in a folder that contains xyz geometry or dynamics files and then give the index of 2,3 or 4 atoms.\nExample:\n\n $ BondAngleDihedral 1 2  <- will give the bond length between atoms 1 and 2\n $ BondAngleDihedral 4 2 6   <- will give the angle between atoms 4,2 and 6\n $ BondAngleDihedral 4 2 6 10 <- will give the dihedral between atoms 4 2 6 and 10\n\n"

data Geometry = Geometry {getAtomnumber :: Int
                         ,getGeometry   :: [Atom]
                         } deriving Show

data Atom     = Atom     {getAtomtype   :: String
                         ,getCoords     :: Vec Double
                         } deriving Show

newtype Vec a = Vec {runVec :: [a]} deriving (Show, Read, Eq)

main = do 
       arg <- getArgs
       files <- readShell "ls *.xyz"
       let fileList = lines files
           aI    = map read2 arg
       case length arg of
          2       -> do calculateMe fileList bond aI
          3       -> do calculateMe fileList angle aI 
          4       -> do calculateMe fileList dihedral aI
          otherwise  -> putStrLn errorHelpMessage

calculateMe fileList fun atomL = do 
            a <- mapM readXyz fileList
            let aa    = map transformInCoord a
                fond  = map (map (fun . getRightAtoms atomL)) aa
            print fond

transformInCoord :: [Geometry] -> [[Vec Double]]
transformInCoord a = map (map getCoords) $ map getGeometry a

transformInAtomT :: [Geometry] -> [[String]]
transformInAtomT a = map (map getAtomtype) $ map getGeometry a

getRightAtoms :: [Int] -> [Vec Double] -> [Vec Double]
getRightAtoms indexlist atomlist = let 
               rightindex = map pred indexlist
               in map (\x -> atomlist!!x) rightindex

readXyz :: FilePath -> IO [Geometry]
readXyz name = do
        a <- readFile name
        let aa = map words $ lines a
            atomnumber = read2 $ head.head $ aa
            split      = chunksOf (atomnumber+2) aa 
            geometries = map readGeom split
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

dif2 :: [Vec Double] -> Double
dif2 [p1,p2,p3] = sqrt $ (p1 - p2) `vecdot` (p3 - p2)

bond :: [Vec Double] -> Double
bond [p1,p2] = vecnorm $ p1 - p2

angle :: [Vec Double] -> Double
angle [p1,p2,p3] = (180.0/pi*) . acos $ arg
  where arg = dif2 [p1,p2,p3] / ((p1 - p2) `vecdot` (p2 - p3))

dihedral :: [Vec Double] -> Double
dihedral [p1,p2,p3,p4] =
  let [xba,xca,xcb,xdb] = zipWith (-) [p2,p3,p3,p4] [p1,p1,p2,p2]
      [w1,w2] = zipWith vecCross [xba,xcb] [xca,xdb]
      [n1,n2] = map vecnorm [w1,w2]
      teta = (180.0/pi*) . acos $ ((w1 `vecdot` w2) / (n1*n2))
        in case 0.0 > (signum $ w2 `vecdot` xba) of
                True -> -teta
                False -> teta


