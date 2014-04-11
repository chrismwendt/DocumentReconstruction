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

next continue pb = do
    w <- U.showImage pb
    continue ()

mainWindow continue file = do
    pb <- pixbufNewFromFile file
    fgs <- newIORef S.empty
    bgs <- newIORef S.empty

    w <- U.showImage pb
    widgetAddEvents w [Button1MotionMask, Button3MotionMask]

    w `on` motionNotifyEvent $ do
        (x, y) <- eventCoordinates
        (pw, ph) <- liftIO $ U.pixbufSize pb
        ms <- eventModifierAll
        liftIO $ sample (if Button1 `elem` ms then fgs else bgs) pb pw ph (floor x) (floor y)
        return False

    w `on` keyPressEvent $ do
        k <- eventKeyName
        case k of
            "f" -> liftIO $ do
                s <- readIORef fgs
                print s
            "b" -> liftIO $ do
                s <- readIORef bgs
                print s
            "c" -> liftIO $ do
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

            _ -> liftIO $ print "Only f, b, and c keys are supported."
        return False

subtractBG :: Pixbuf -> [(V3.Vec, V3.Vec, V3.Vec)] -> [(V3.Vec, V3.Vec, V3.Vec)] -> IO Pixbuf
subtractBG pb fgHull bgHull = do
    U.pixbufMap pb f
    return pb
    where
    f (r, g, b, a) = let
        (r', g', b') = modifyColor (r, g, b)
        a' = modifyAlpha (fromIntegral r / 255, fromIntegral g / 255, fromIntegral b / 255) fgHull bgHull
        in (r', g', b', a')

toTriangles :: V.Vector V3.Vec -> [V.Vector Int] -> [(V3.Vec, V3.Vec, V3.Vec)]
toTriangles points hull = map (\t -> (points V.! (t V.! 0), points V.! (t V.! 1), points V.! (t V.! 2))) hull

front p (a, b, c) = (p `V3.vsub` a) `V3.vdot` normal > 0
    where
    e1 = b `V3.vsub` a
    e2 = c `V3.vsub` b
    normal = e1 `V3.vcross` e2

modifyColor (r, g, b) = (r, g, b)

modifyAlpha (r, g, b) fgHull bgHull = if all (front (V3.Vec r g b)) fgHull then 255 else 0

sample setRef pb pw ph x y
    | x < 0 || y < 0 || x >= pw || y >= ph = return ()
    | otherwise = do
        s <- readIORef setRef
        rgba <- U.getRGBA pb x y
        if S.notMember rgba s
            then print rgba
            else return ()
        modifyIORef setRef $ S.insert rgba
