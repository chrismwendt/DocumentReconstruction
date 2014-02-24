{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import GHC.Generics
import Codec.Picture
import Data.Vector.Storable as V

data Input = Input
    { inputImage :: String
    } deriving (Show,Generic)

instance FromJSON Input
instance ToJSON Input

scaleRGB8 :: Int -> Image PixelRGBA8 -> Image PixelRGBA8
scaleRGB8 scale = pixelMap f
    where
    compf v = fromIntegral $ (fromIntegral v) `div` scale
    f (PixelRGBA8 r g b a) = PixelRGBA8 (compf r) (compf g) (compf b) a

main :: IO ()
main = do
    inputString <- getContents
    case eitherDecode $ B.pack inputString :: Either String Input of
        Left e -> putStrLn e
        Right input -> do
            a <- readImage (inputImage input)
            case a of
                Left e -> putStrLn e
                Right (ImageRGBA8 i) -> writePng "new.png" $ scaleRGB8 2 i
                Right _ -> putStrLn "Only ImageRGBA8 is supported"
