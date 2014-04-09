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

main = do
    initGUI
    getArgs >>= mainWindow next . head
    mainGUI

next pb = do
    w <- showImage pb
    return ()

mainWindow continue file = do
    pb <- pixbufNewFromFile file
    fgs <- newIORef S.empty
    bgs <- newIORef S.empty

    w <- showImage pb
    widgetAddEvents w [Button1MotionMask, Button3MotionMask]

    w `on` motionNotifyEvent $ do
        (x, y) <- eventCoordinates
        (pw, ph) <- liftIO $ pixbufSize pb
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
                    let rgbaToColorList (r, g, b, a) = map (\c -> fromIntegral c / 255) [r, g, b, a]
                    let points = V.fromList $ map (V.fromList . rgbaToColorList) $ S.toList cols
                    return $ qhull' points
                fgm <- colsToHull fgs
                case fgm of
                    Left fgHull -> do
                        bgm <- colsToHull bgs
                        case bgm of
                            Left bgHull -> do
                                subtractBG pb fgHull bgHull >>= continue
                            Right e -> putStrLn $ errorMessage e
                    Right e -> putStrLn $ errorMessage e

            _ -> liftIO $ print "Only f, b, and c keys are supported."
        return False

subtractBG pb fgHull bgHull = do
    pixbufMap pb (\(r, g, b, a) -> let (r', g', b') = modifyColor (r, g, b); a' = modifyAlpha (r, g, b) in (r', g', b', a'))
    return pb

modifyColor (r, g, b) = (r `div` 2, g, b)

modifyAlpha (r, g, b) = 255

pixbufMap pb f = do
    (pw, ph) <- pixbufSize pb
    let f' x y = do
        rgba <- getRGBA pb x y
        setRGBA pb x y (f rgba)
    sequence [f' x y | y <- [0 .. ph - 1], x <- [0 .. pw - 1]]

sample setRef pb pw ph x y
    | x < 0 || y < 0 || x >= pw || y >= ph = return ()
    | otherwise = do
        s <- readIORef setRef
        rgba <- getRGBA pb x y
        if S.notMember rgba s
            then print rgba
            else return ()
        modifyIORef setRef $ S.insert rgba

getRGBA pb x y = do
    ps <- pixbufGetPixels pb :: IO (PixbufData Int Word8)
    row <- pixbufGetRowstride pb
    r <- readArray ps (y*row + 4*x + 0)
    g <- readArray ps (y*row + 4*x + 1)
    b <- readArray ps (y*row + 4*x + 2)
    a <- readArray ps (y*row + 4*x + 3)
    return (r, g, b, a)

setRGBA pb x y (r, g, b, a) = do
    ps <- pixbufGetPixels pb :: IO (PixbufData Int Word8)
    row <- pixbufGetRowstride pb
    writeArray ps (y*row + 4*x + 0) r
    writeArray ps (y*row + 4*x + 1) g
    writeArray ps (y*row + 4*x + 2) b
    writeArray ps (y*row + 4*x + 3) a

showImageFile f = pixbufNewFromFile f >>= showImage

showImage pb = do
    w <- windowNew
    onDestroy w mainQuit
    (pw, ph) <- pixbufSize pb
    widgetSetSizeRequest w pw ph

    f <- fixedNew
    set w [ containerChild := f ]

    onExpose f $ \_ -> drawPixbufOnWindow pb w

    widgetShowAll w

    return w

pixbufSize pb = do
    pw <- pixbufGetWidth pb
    ph <- pixbufGetHeight pb
    return (pw, ph)

drawPixbufOnWindow pb w = do
    dw <- widgetGetDrawWindow w
    gc <- gcNew dw
    (pw, ph) <- pixbufSize pb
    drawPixbuf dw gc pb 0 0 0 0 pw ph RgbDitherNone 0 0
    return False
