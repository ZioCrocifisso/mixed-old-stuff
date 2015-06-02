module Main where

import qualified Data.Vector as V
import Graphics.Gloss.Interface.Pure.Game hiding (KeyState(..))
import qualified Graphics.Gloss.Interface.Pure.Game as G
import Prelude hiding (Either(Left, Right))
import System.Random

data Direction = Up | Down | Left | Right deriving Eq
data Cell = Empty | Wall | Point | Goal deriving Eq
type Map = V.Vector Cell
type QTable = V.Vector (Double, Double, Double, Double)

cellsW, cellsH :: Num a => a
cellsW = 10
cellsH = 10

cellW, cellH :: Num a => a
cellW = 32
cellH = 32

winW, winH, winS :: Num a => a
winW = cellsW * cellW
winH = cellsH * cellH
winS = 2

data State = State {
        qTable :: QTable,
        mapVec :: Map,
        posFrom :: (Int ,Int),
        posTo :: (Int, Int),
        pos :: (Float, Float),
        dirTo :: Direction,
        simSpeed :: Float,
        rnd :: StdGen
}

initState :: StdGen -> State
initState g = State {
        rnd = g,
        qTable = V.replicate (cellW * cellH) (0, 0, 0, 0),
        mapVec = V.fromList
           [ Empty, Wall, Wall, Wall, Wall, Wall, Empty, Point, Empty, Empty
           , Empty, Empty, Empty, Empty, Empty, Wall, Empty, Wall, Empty, Empty
           , Empty, Empty, Empty, Wall, Empty, Wall, Empty, Empty, Empty, Empty
           , Empty, Wall, Point, Wall, Empty, Wall, Empty, Wall, Wall, Empty
           , Empty, Wall, Empty, Wall, Empty, Empty, Empty, Wall, Empty, Empty
           , Wall, Wall, Wall, Wall, Empty, Wall, Wall, Wall, Empty, Wall
           , Empty, Wall, Empty, Empty, Empty, Wall, Empty, Empty, Empty, Wall
           , Empty, Empty, Empty, Wall, Wall, Wall, Empty, Wall, Empty, Empty
           , Wall, Wall, Empty, Empty, Empty, Empty, Empty, Wall, Empty, Empty
           , Empty, Empty, Empty, Point, Wall, Wall, Wall, Wall, Empty, Goal ],
        simSpeed = 0,
        posFrom = (cellW `quot` 2, cellH `quot` 2),
        posTo = (cellW `quot` 2, cellH `quot` 2),
        pos = (cellW / 2, cellH / 2),
        dirTo = Down
        }

qGet :: State -> (Double, Double, Double, Double)
qGet state = qTable state V.! mapIndex ( fst $ posFrom state
                                       , snd $ posFrom state)

qUpdate :: Double -> Double -> Double -> State -> State -> State
qUpdate learnRate discountFac reward preState postState =
        let dir = dirTo preState
            preQs = qGet preState
            preQ = getDir dir preQs
            table = qTable postState
            preCoords = (fst $ posFrom preState, snd $ posFrom preState)
            opt = snd $ optimal 0 postState (True, True, True, True)
            preQ' = preQ + learnRate * (reward + discountFac * opt - preQ)
        in postState { qTable = table V.// [(mapIndex preCoords,
                                           updDir dir preQ' preQs)] }
        where getDir Up (x, _, _, _) = x
              getDir Down (_, x, _, _) = x
              getDir Left (_, _, x, _) = x
              getDir Right (_, _, _, x) = x

              updDir Up x (_, y, z, w) = (x, y, z, w)
              updDir Down y (x, _, z, w) = (x, y, z, w)
              updDir Left z (x, y, _, w) = (x, y, z, w)
              updDir Right w (x, y, z, _) = (x, y, z, w)

eliminateWalls :: (Bool, Bool, Bool, Bool) -> State -> QTable
eliminateWalls (d1, d2, d3, d4) st =
        let idx = mapIndex $ posTo st
            (q1, q2, q3, q4) = qTable st V.! idx
            qInfNeg q d = if d then q else - 1 / 0
            newQs = ( qInfNeg q1 d1, qInfNeg q2 d2
                    , qInfNeg q3 d3, qInfNeg q4 d4 )
        in qTable st V.// [(idx, newQs)]

step :: Float -> State -> State
step _ state =
        let (fx, fy) = posFrom state
            (tx, ty) = posTo state
            (x, y) = pos state
            speed = simSpeed state
            (rndVal, newRnd) = randomR (- 0.1, 0.1) $ rnd state
        in if fx < winW - cellW || fy < winH - cellH
              then if fromIntegral tx == x && fromIntegral ty == y && speed > 0
                   then let isNotWall (x, y) =
                                     x > 0 && y > 0 && x < winW && y < winH &&
                                     mapVec state V.! mapIndex (x, y) /= Wall
                            allowedDirs = ( isNotWall (tx, ty - cellH)
                                          , isNotWall (tx, ty + cellH)
                                          , isNotWall (tx - cellW, ty)
                                          , isNotWall (tx + cellW, ty) )
                            postState =
                                    state { posFrom = posTo state,
                                            rnd = newRnd,
                                            qTable = eliminateWalls allowedDirs
                                                                    state }
                                                     
                            (newDir, _) = optimal rndVal postState allowedDirs
                            posReward =
                                    let r = 0.5 * ( (fromIntegral $ tx - fx)
                                                  + (fromIntegral $ ty - fy) )
                                    in if r < 0 then r * 4 else - r / 10
                            cellReward =
                                    case mapVec state V.! mapIndex (tx, ty) of
                                         Goal -> 10000
                                         _ -> 0
                            reward = posReward + cellReward
                        in qUpdate 0.2 0.999 reward state postState
                             {
                                     posTo = case newDir of
                                                  Up -> (tx, ty - cellH)
                                                  Down -> (tx, ty + cellH)
                                                  Left -> (tx - cellW, ty)
                                                  Right -> (tx + cellW, ty),
                                     dirTo = newDir
                             }
                   else let advance k tk fk =
                             let dist = abs $ k - fromIntegral tk
                             in if speed < dist
                                     then k + signum (fromIntegral $ tk - fk)
                                            * speed
                                     else fromIntegral tk

                        in state { pos = (advance x tx fx, advance y ty fy) }
              else state {
                                simSpeed = 0,
                                posFrom = (cellW `quot` 2, cellH `quot` 2),
                                posTo = (cellW `quot` 2, cellH `quot` 2),
                                pos = (cellW / 2, cellH / 2),
                                dirTo = Down
                   }

optimal :: Double -> State -> (Bool, Bool, Bool, Bool) -> (Direction, Double)
optimal rndVal state (couldQ1, couldQ2, couldQ3, couldQ4) = 
        let (postQ1, postQ2, postQ3, postQ4) = qGet state
            neg b x = if b then x else - 1 / 0
            dirMax (dx, x) (dy, y) = if x > y then (dx, x) else (dy, y)
        in (Up, neg couldQ1 postQ1 + rndVal) `dirMax`
           (Down, neg couldQ2 postQ2 + rndVal * signum rndVal) `dirMax`
           (Left, neg couldQ3 postQ3 - rndVal) `dirMax`
           (Right, neg couldQ4 postQ4 - rndVal * signum rndVal)

mapIndex :: (Int, Int) -> Int
mapIndex (x, y) = quot y cellH * cellsW + quot x cellW

mapIndexRev :: Int -> (Int, Int)
mapIndexRev i = let (y, x) = divMod i cellsW in (x * cellW, y * cellH)

drawQuality :: Int -> (Double, Double, Double, Double) -> Picture
drawQuality i (q1, q2, q3, q4) =
        let (x, y) = mapIndexRev i
            adp q = if q > - 1 / 0 then abs q else 0
            qmax = adp q1 `max` adp q2 `max` adp q3 `max` adp q4
        in  translate (fromIntegral x + cellW / 2)
                      (fromIntegral y + cellH / 2) . pictures $
                [ drawQCircle 0 (- cellH / 3) q1 qmax 0
                , drawQCircle 0 (cellH / 3) q2 qmax 180
                , drawQCircle (- cellW / 3) 0 q3 qmax 90
                , drawQCircle (cellW / 3) 0 q4 qmax 270 ]
        where drawQCircle x y qd qm r | qd > - 1 / 0 = let q = realToFrac qd in
                translate x y
                        . color (makeColor (0.5 - signum q) 1 0
                                        $ abs (q / realToFrac qm) ^ 5)
                        . rotate r
                        . scale (signum q) (signum q)
                        $ triangle (cellW / 10)
              drawQCircle _ _ _ _ _ = blank
              triangle s = polygon [(0, - s), (- s, s), (s, s)]

drawCell :: Int -> Cell -> Picture
drawCell i c = let (x, y) = mapIndexRev i
               in translate (fromIntegral x + cellW / 2)
                            (fromIntegral y + cellH / 2) . color (
                       case c of
                               Wall -> black
                               Empty -> makeColor 0 0 0 0
                               Point -> makeColor 0 0 0 0
                               Goal -> green
                      ) $ rectangleSolid cellW cellH

draw :: State -> Picture
draw state = scale winS (- winS) . translate (- winW / 2) (- winH / 2) $
        pictures [ pictures . V.toList . V.imap drawQuality $ qTable state
                 , pictures . V.toList . V.imap drawCell $ mapVec state
                 , translate (fst (pos state))
                             (snd (pos state))
                            . color blue $ circle (cellW / 2)
                 ]


handle :: Event -> State -> State
handle (EventKey k G.Up _ _) s = s { simSpeed = case k of
                                                     Char '0' -> 0
                                                     Char '1' -> 2
                                                     Char '2' -> 4
                                                     Char '3' -> 8
                                                     Char '4' -> 16
                                                     Char '5' -> 32
                                                     Char '6' -> 64
                                                     Char '7' -> 2000
                                                     Char '8' -> 20000000
                                                     Char '9' -> 200000000
                                                     _ -> simSpeed s }
handle _ s = s

main :: IO ()
main = do g <- newStdGen
          print g
          play (InWindow "Labirinto" (winW * winS, winH * winS) (200, 200))
               white 30 (initState g) draw handle step
