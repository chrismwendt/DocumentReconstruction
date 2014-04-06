{-# LANGUAGE DeriveGeneric #-}

import Graphics.UI.Gtk

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import GHC.Generics
import Control.Monad.Trans(liftIO)
import Data.IORef
import Foreign.Ptr
import Graphics.Rendering.Cairo

data Input = Input
    { image :: String
    } deriving (Show, Generic)

instance FromJSON Input
instance ToJSON Input

main :: IO ()
main = do
    inputString <- getContents
    case eitherDecode $ B.pack inputString :: Either String Input of
        Left e -> putStrLn e
        Right input@(Input { image = path }) -> do
            print input
            pixbuf <- pixbufNewFromFile path
            pb <- newIORef pixbuf
            iori <- newIORef []
            go pb iori

go pixbuf input = do
    initGUI
    window <- windowNew
    pb <- readIORef pixbuf
    surf <- imageSurfaceCreateFromPNG "tank.png"
    image <- imageNewFromPixbuf pb
    da <- drawingAreaNew
    da `on` realize $ do
        liftIO $ do
            print "HEYO"
            dw <- widgetGetDrawWindow da
            renderWithDrawable dw (do
                setSourcePixbuf pb 0 0
                rotate (pi/8)
                paint
                return ()
                )
    set window [ containerBorderWidth := 10, containerChild := da ]
    window `on` buttonPressEvent $ do
        (x, y) <- eventCoordinates
        liftIO $ modifyIORef input ((x, y):)
        liftIO $ readIORef input >>= print
        liftIO $ do
            return ()
            -- pb <- readIORef pixbuf
            -- w <- pixbufGetWidth pb
            -- pb' <- pixbufScaleSimple pb (w `div` 2) 40 InterpNearest
            -- imageSetFromPixbuf image pb'
            -- writeIORef pixbuf pb'
        return False
    -- window `on` deleteEvent $ liftIO mainQuit >> return False
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
