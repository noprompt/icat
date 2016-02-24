module ICat where


import           ColorTable
import qualified Control.Monad
import qualified Codec.Picture as Picture
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Vector.Storable as SVector
import qualified Numeric
import qualified System.Console.Terminal.Size as Terminal
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.IO as IO


intToHex :: (Integral a, Show a) => a -> [Char]
intToHex i =
  let
    h = (Numeric.showHex i "")
  in
    if i <= 16 then
     '0' : h
    else
      h


approximateXtermColor :: (Num a, Ord a) => a -> a
approximateXtermColor i =
  let
    steps = [0x00, 0x5f, 0x87, 0xaf, 0xd7, 0xff]
    approx' i [] = 0
    approx' i ((low , high) : steps) =
      if low <= i && i <= high then
        let
          dl = abs (i - low)
          dh = abs (high - i)
        in
          if dl < dh then
            low
          else
            high
      else
        approx' i steps
        
  in
    approx' i (zip steps $ tail steps)


hex256at :: Picture.Image Picture.PixelRGB8 -> Int -> Int -> [Char]
hex256at p x y =
  let
    (Picture.PixelRGB8 r g b) = Picture.pixelAt p x y
    hex = List.intercalate "" $ map (intToHex . approximateXtermColor) [r, g, b]
  in
    hex


findCodeByHex :: [Char] -> Maybe [Char]
findCodeByHex hex =
  fmap fst $ List.find (\(c, h) -> h == hex) colorTable


escapeCodeAt :: Picture.Image Picture.PixelRGB8 -> Int -> Int -> [Char]
escapeCodeAt p x y = escapeCode
  where
    code = Maybe.fromMaybe "0" $ findCodeByHex $ hex256at p x y
    escapeCode = "\ESC[48;5;" ++ code ++ "m"

    
handlePicture :: Either String Picture.DynamicImage -> IO ()
handlePicture (Left errorMessage) = putStrLn errorMessage
handlePicture (Right picture) =
  let
    pictureRGB8 = Picture.convertRGB8 picture
    pw = Picture.imageWidth pictureRGB8
    ph = Picture.imageHeight pictureRGB8
  in
    do
      maybeWindow <- Terminal.size
      let step = case maybeWindow of
            -- Determine which pixels should be drawn based on the
            -- width of terminal.
            Just window ->
              let
                tw = Terminal.width window
              in
                head $ List.dropWhile (\i -> i * tw < pw) [1..]
            -- If the terminal width can not be determined, use this
            -- completely arbitrary number...
            Nothing ->
              4
      -- Print out the picture.
      Control.Monad.forM_ [0, step .. ph - 1] $
        (\y ->
          do
            Control.Monad.forM_ [0, step .. pw - 1 ] $
              (\x ->
                do 
                  IO.hPutStr IO.stdout $ escapeCodeAt pictureRGB8 x y
                  IO.hPutStr IO.stdout " ")
            IO.hPutStrLn IO.stdout "\ESC[0m")


main :: IO ()
main = do
  args <- Environment.getArgs
  case args of
    (fileName : []) ->
      Picture.readImage fileName >>= handlePicture
    _ ->
      return ()
