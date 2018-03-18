{-
GPLV3.0 or later copyright brmlab.cz contact timothyhobbs@seznam.cz

Also copyright cheater http://cheater.posterous.com/haskell-curses

Copyright 2012.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

This is a simple library for displaying a menu in a terminal.  It's main function is displayMenu which takes a list of strings and presents them as options to the user, and returns the string that the user selects...
 
This code was written by cheater__ and published here:
http://cheater.posterous.com/haskell-curses

18:53 < cheater_> hi timthelion
18:54 < timthelion> cheater_: hi!
18:54 < timthelion> cheater_: Did you get my message?
18:54 < cheater_> i think so
18:54 < cheater_> you had some code you wanted to use right?
18:54 < timthelion> Yes.
18:54 < cheater_> what license do you want to release it under?
18:55 < timthelion> GPL 3.0.
18:55 < cheater_> ok, that's fine with me
18:55 < timthelion> thank you :)

-}

module Graphics.Vty.Menu(displayMenu,displayMenuOfValues) where
import qualified Graphics.Vty as Vty
import Graphics.Vty.Input
import Graphics.Vty.Output
import Graphics.Vty.Config

getName :: String -> String
getName item = item
-- returns the name of a item.
-- This will become more complicated some day.

itemImage :: String -> Bool -> Vty.Image
itemImage item cursor = do
-- prints out info on an item
    let
     wfc = Vty.withForeColor
     wbc = Vty.withBackColor
     (indicator, useColor) =
      if cursor
       then (" > ", True)
       else ("   ", False)
     attr =
      if useColor
       then Vty.currentAttr `wfc` Vty.black `wbc` Vty.white
       else Vty.currentAttr `wfc` Vty.white `wbc` Vty.black
    Vty.string attr $ indicator ++ (getName item)

allocate :: IO Vty.Vty
allocate = do
-- sets up Vty
    vt <- standardIOConfig >>= Vty.mkVty
    return vt

deallocate :: Vty.Vty -> IO ()
deallocate vt =
-- frees Vty resouces
    Vty.shutdown vt

handleKeyboard :: Vty.Key -> Int -> Int -> [String] -> Vty.Vty -> IO (Vty.Vty,Maybe Int)
handleKeyboard key position offset items vt = case key of
-- handles keyboard input
    KChar 'q' -> return (vt,Nothing)
    KEsc -> return (vt,Nothing)
    KEnter -> return (vt,Just position)
    KChar 'j' -> work (position + 1) offset items vt
    KDown -> work (position + 1) offset items vt
    KChar 'k' -> work (position - 1) offset items vt
    KUp -> work (position - 1) offset items vt
    KPageUp -> do
        bounds <- Vty.displayBounds $ Vty.outputIface vt
        work (position - (Vty.regionHeight bounds)) offset items vt
    KPageDown -> do
        bounds <- Vty.displayBounds $ Vty.outputIface vt
        work (position + (Vty.regionHeight bounds)) offset items vt
    KHome -> work 0 offset items vt
    KEnd -> work (length items - 1) offset items vt
    _ -> work position offset items vt
 

work :: Int -> Int -> [String] -> Vty.Vty -> IO (Vty.Vty,Maybe Int)
work requestedPosition offset items vt = do
-- displays items 
    let position = max 0 (min requestedPosition (length items - 1))
    (cols, rows) <- Vty.displayBounds $ Vty.outputIface vt
    let
     (cols2, rows2) = (fromEnum cols, fromEnum rows)
     screenPosition = position + offset
     offset2 =
      if screenPosition >= rows2
       then offset - (screenPosition - rows2 + 1)
       else if screenPosition < 0
             then offset - screenPosition
             else offset
     items2 = drop (0 - offset2) $ zip [0..] items
     itemImages =
      map
       (\(line, item) -> itemImage item (line == position))
       items2
     imagesUnified = Vty.vertCat itemImages
     pic = Vty.picForImage $ imagesUnified
    Vty.update vt pic
    eventLoop position offset2 items vt

eventLoop :: Int -> Int -> [String] -> Vty.Vty -> IO (Vty.Vty, Maybe Int)
eventLoop position offset items vt = do
    ev <- Vty.nextEvent vt
    case ev of
     Vty.EvKey key _ -> handleKeyboard key position offset items vt
     _ -> eventLoop position offset items vt

displayMenu :: [String] -> IO (Maybe String)
displayMenu items = do
 displayMenuOfValues $ zip items items

displayMenuOfValues :: [(String,a)] -> IO (Maybe a)
displayMenuOfValues items = do
 vty <- allocate
 (vty',maybePos) <- work 0 0 (map fst items) vty
 deallocate vty'
 case maybePos of
  Just pos -> return $ Just $ snd $ items !! pos
  Nothing -> return Nothing

--main = do
-- choice <- displayMenu ["Hi","Bye","The other thing."]
-- print choice
-- the main program
