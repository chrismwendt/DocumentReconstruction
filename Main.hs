import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import System.Environment
import Data.IORef
import Graphics.Rendering.Cairo
import Data.Array.MArray
import Data.Word
import Control.Monad
import qualified Data.Set as S
import Numeric.Qhull
import qualified Data.Vector as V
import qualified Utility as U
import qualified Data.Glome.Vec as V3

main = do
    initGUI
    getArgs >>= (mainWindow $ next $ (const $ (return () :: IO ()))) . head
    mainGUI

type Triangle = (V3.Vec, V3.Vec, V3.Vec)

next continue pb = do
    w <- U.showImage pb
    continue ()

mainWindow continue file = do
    pb <- pixbufNewFromFile file
    fgs <- newIORef S.empty
    bgs <- newIORef S.empty

    w <- U.showImage pb
    widgetAddEvents w [Button1MotionMask, Button3MotionMask]

    w `on` motionNotifyEvent $ pickSample fgs bgs pb >> return True

    w `on` keyPressEvent $ eventKeyName >>= liftIO . keyPressed continue pb fgs bgs >> return True

keyPressed continue pb fgs bgs "f" = readIORef fgs >>= print
keyPressed continue pb fgs bgs "b" = readIORef bgs >>= print
keyPressed continue pb fgs bgs "c" = do
    let colsToHull colsRef = do
        cols <- readIORef colsRef
        let rgbaToColorList (r, g, b, a) = map (\c -> fromIntegral c / 255) [r, g, b]
        let points = V.fromList $ map (V.fromList . rgbaToColorList) $ S.toList cols
        return $ (V.map (\v -> V3.Vec (v V.! 0) (v V.! 1) (v V.! 2)) points, qhull' points)
    (fgPoints, fgm) <- colsToHull fgs
    case fgm of
        Left fgHull -> do
            (bgPoints, bgm) <- colsToHull bgs
            case bgm of
                Left bgHull -> do
                    subtractBG pb (toTriangles fgPoints (map V.convert fgHull)) (toTriangles bgPoints (map V.convert bgHull)) >>= continue
                Right e -> putStrLn $ errorMessage e
        Right e -> putStrLn $ errorMessage e
keyPressed continue pb fgs bgs _ = print "Only f, b, and c keys are supported."

subtractBG :: Pixbuf -> [Triangle] -> [Triangle] -> IO Pixbuf
subtractBG pb fgHull bgHull = do
    U.pixbufMap pb f
    return pb
    where
    f (r, g, b, a) = let
        (r', g', b') = modifyColor (r, g, b)
        a' = modifyAlpha (fromIntegral r / 255, fromIntegral g / 255, fromIntegral b / 255) fgHull bgHull
        in (r', g', b', a')

toTriangles :: V.Vector V3.Vec -> [V.Vector Int] -> [Triangle]
toTriangles points hull = map (\t -> (points V.! (t V.! 0), points V.! (t V.! 1), points V.! (t V.! 2))) hull

front p (a, b, c) = (p `V3.vsub` a) `V3.vdot` normal > 0
    where
    e1 = b `V3.vsub` a
    e2 = c `V3.vsub` b
    normal = e1 `V3.vcross` e2

modifyColor (r, g, b) = (r, g, b)

modifyAlpha (r, g, b) fgHull bgHull = if all (front (V3.Vec r g b)) fgHull then 255 else 0

pickSample fgs bgs pb = do
    (x, y) <- eventCoordinates
    ms <- eventModifierAll
    liftIO $ do
        (pw, ph) <- U.pixbufSize pb
        let s a = readIORef a >>= sample pb pw ph (floor x) (floor y) >>= writeIORef a
        when (Button1 `elem` ms) $ s fgs
        when (Button3 `elem` ms) $ s bgs

sample pb pw ph x y set
    | x < 0 || y < 0 || x >= pw || y >= ph = return set
    | otherwise = do
        rgba <- U.getRGBA pb x y
        when (S.notMember rgba set) $ print rgba
        return $ S.insert rgba set
