module Main where

import Physics as P
w = 800
mw = 400

bounce b b2 _ _ =       let dir = P.vecDir (P.vecDist (P.bodyVecPos b2) (P.bodyVecPos b))
                            v = P.buildVec ((P.vecLen (P.bodyVel b)) * 0.8) dir
                        in P.bodyLaunch (P.bodyMove b (P.vecNeg (P.vecMul (P.bodyVel b) 2))) v

--system =        P.systemAdd "" (P.vecTo (0, 0)) P.nilVec 2000 (20 / w)
--              . P.systemAdd "" (P.vecTo (0.5, 0.2)) (P.vecTo (0 - 0.003, 0 - 0.003)) 0.1 (5 / w)
--              . P.systemAdd "" (P.vecTo (0.35, 0.45)) (P.vecTo (0 - 0.002, 0 - 0.003)) 0.1 (5 / w)
--              . P.systemAdd "" (P.vecTo (0-0.8, 0.2)) (P.vecTo (0 + 0.003, 0 - 0.0035)) 0.1 (5 / w)
--              . P.systemAdd "" (P.vecTo (0-0.7, 0.2)) (P.vecTo (0 + 0.003, 0 - 0.0035)) 0.1 (5 / w)
--              . P.systemAdd "" (P.vecTo (0-0.6, 0.3)) (P.vecTo (0 + 0.003, 0 - 0.0035)) 0.1 (5 / w)
--              . P.systemAdd "" (P.vecTo (0.1, 0.4)) (P.vecTo (0 - 0, 0 - 0.0035))  0.1(5 / w)
system =        P.systemAdd "" (P.vecTo (0, 0.45)) (P.vecTo (0.003, 0)) 4000 (15 / w)
                . P.systemAdd "" (P.vecTo (0, 0 - 0.45)) (P.vecTo (0 - 0.003, 0)) 1500 (10 / w)
              .  P.systemAdd "" (P.vecTo (0.12,  0.55)) (P.vecTo (0.002, 0.001)) 0.1 (1.6 / w)
                <| P.newSystem

drawBody b = circle ((P.bodySize b) * mw) |> filled red |> moveX ((P.bodyX b) * mw) |> moveY ((P.bodyY b) * mw)
drawScene bodies = collage w w (map drawBody bodies)

main = lift (drawScene . P.systemBodies) (foldp (\t s -> P.systemStep s bounce) system (fps 60))
