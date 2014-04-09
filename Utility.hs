module Utility where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Data.Array.MArray
import Data.Word

pixbufMap pb f = do
    (pw, ph) <- pixbufSize pb
    let f' x y = do
        rgba <- getRGBA pb x y
        setRGBA pb x y (f rgba)
    sequence [f' x y | y <- [0 .. ph - 1], x <- [0 .. pw - 1]]

pixbufSize pb = do
    pw <- pixbufGetWidth pb
    ph <- pixbufGetHeight pb
    return (pw, ph)

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

drawPixbufOnWindow pb w = do
    dw <- widgetGetDrawWindow w
    gc <- gcNew dw
    (pw, ph) <- pixbufSize pb
    drawPixbuf dw gc pb 0 0 0 0 pw ph RgbDitherNone 0 0
    return False
