module ICat where

import qualified Codec.Picture                as Picture
import           ColorTable
import           Control.Monad                (forM_)
import qualified Data.List                    as List
import qualified Data.Maybe                   as Maybe
-- import qualified Data.Vector.Storable         as SVector
import qualified Numeric
import qualified System.Console.Terminal.Size as Terminal
-- import qualified System.Directory             as Directory
import qualified System.Environment           as Environment

intToHex :: (Integral a, Show a) => a -> String
intToHex i
  | i <= 16   = '0': h
  | otherwise = h
  where h = Numeric.showHex i ""

-- you write code like you have to do work upfront
-- before creating a reference to a computation.
-- no need for that.
approximateXtermColor :: (Num a, Ord a) => a -> a
approximateXtermColor i =
  approx' i (zip steps $ tail steps)
  where steps = [0x00, 0x5f, 0x87, 0xaf, 0xd7, 0xff]
        approx' _ [] = 0
        approx' j ((low , high) : steps')
          | jInRange && dlLower = low
          | jInRange && not dlLower = high
          | otherwise = approx' j steps'
          where dl = abs (j - low)
                dh = abs (high - j)
                jInRange = low <= j && j <= high
                dlLower = dl < dh

hex256at :: Picture.Image Picture.PixelRGB8 -> Int -> Int -> String
hex256at p x y =
  hex
  where (Picture.PixelRGB8 r g b) = Picture.pixelAt p x y
        hex = List.intercalate "" $ map (intToHex . approximateXtermColor) [r, g, b]

-- use hlint, it'll tell you how to clean up stuff like I did here.
findCodeByHex :: String -> Maybe String
findCodeByHex hex =
  fst <$> List.find (\(_, h) -> h == hex) colorTable


escapeCodeAt :: Picture.Image Picture.PixelRGB8 -> Int -> Int -> String
escapeCodeAt p x y = escapeCode
  where
    code = Maybe.fromMaybe "0" $ findCodeByHex $ hex256at p x y
    escapeCode = "\ESC[48;5;" ++ code ++ "m"


-- this is a bit tower-y and can be split apart and cleaned up
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
      forM_ [0, step .. ph - 1]
        (\y ->
          do
            forM_ [0, step .. pw - 1 ]
              (\x ->
                do
                  -- everybody knows where these come from
                  -- also hPutStr stdout == putStr
                  putStr $ escapeCodeAt pictureRGB8 x y
                  putStr " ")
              -- hPutStrLn stdout = putStrLn
            putStrLn "\ESC[0m")


main :: IO ()
main = do
  args <- Environment.getArgs
  case args of
    [fileName] ->
      Picture.readImage fileName >>= handlePicture
    _ ->
      return ()
