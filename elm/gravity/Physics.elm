module Physics where

data Vector = Vector Float Float
data Body = Body Vector Vector Vector Float Float
data NamedBody = NamedBody String Int Body
data QtNode = QtExternal (Maybe Body) Float | QtInternal [QtNode] Float Vector Float
data BodySystem = BodySystem [NamedBody] Int

gAcc = 0.00000002
dist (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
radCollision pos1 size1 pos2 size2 = if | dist pos1 pos2 <= (size1 + size2) -> True
                                        | otherwise -> False

nilVec = Vector 0 0

vecX (Vector x _) = x
vecY (Vector _ y) = y

vecPos (Vector x y) = (x, y)
vecDir (Vector x y) = atan2 y x
vecLen (Vector x y) = dist (0, 0) (x, y)
vecNeg (Vector x y) = (Vector (0 - x) (0 - y))

vecDist v1 v2 = let     dx = (vecX v2) - (vecX v1)
                        dy = (vecY v2) - (vecY v1)
                in
                        Vector dx dy

vecTo (x, y) = Vector x y
buildVec len dir = Vector ((cos dir) * len) ((sin dir) * len)

vecSum (Vector ax ay) (Vector bx by) = Vector (ax + bx) (ay + by)
vecMul a b = buildVec ((vecLen a) * b) (vecDir a)
vecDot a b = (vecLen a) * (vecLen b) * (cos ((vecDir a) - (vecDir b)))



bodyVecPos (Body pos _ _ _ _) = pos
bodyPos (Body (Vector x y) _ _ _ _) = (x, y)
bodyX (Body (Vector x _) _ _ _ _) = x
bodyY (Body (Vector _ y) _ _ _ _) = y
bodyIn x1 y1 x2 y2 (Body (Vector x y) _ _ _ _) = if     |  x >= x1
                                                        && x < x2
                                                        && y >= y1
                                                        && y < y2 -> True
                                                        |  otherwise -> False


bodyVel (Body _ vel _ _ _) = vel
bodyAcc (Body _ _ acc _ _) = acc
bodyMass (Body _ _ _ mass _) = mass
bodySize (Body _ _ _ _ size) = size

bodyMove (Body pos v a m s) vec = Body (vecSum pos vec) v a m s
bodyLaunch (Body p v a m s) vec = Body p vec a m s

bodyCollision (Body v1 _ _ _ s1) (Body v2 _ _ _ s2) = radCollision (vecPos v1) s1 (vecPos v2) s2

bodyDist (Body v1 _ _ _ _) (Body v2 _ _ _ _) = dist (vecPos v1) (vecPos v2)

bodyGravity b1 b2 = let vd = vecDist (bodyVecPos b1) (bodyVecPos b2)
                        r2 = 1 / ((vecLen vd) ^ 2)
                        m = (bodyMass b1) * (bodyMass b2)
                    in
                        if      | (vecLen vd) > 0 -> buildVec (m * gAcc * r2) (vecDir vd)
                                | otherwise -> Vector 0 0



calcQt bs sz =  let
                        floatLen = (toFloat (length bs))
                        sumb f d bodies = case bodies of
                                                [] -> d
                                                b::[] -> (f b)
                                                b1::b2::bs -> (f b1) + (f b2) + (sumb f d bs)
                                                _ -> d
                        com bs = Vector ((sumb bodyX 0 bs) / floatLen)
                                        ((sumb bodyY 0 bs) / floatLen)
                        masses bs = sumb bodyMass 0 bs
                        part bs sz x y = let    x1 = if x == 0 then (0 - sz) else 0
                                                y1 = if y == 0 then (0 - sz) else 0
                                         in
                                                filter (bodyIn x1 y1 (x1 + sz) (y1 + sz)) bs
                        split bs sz = QtInternal        [ calcQt (part bs sz 0 0) sz
                                                        , calcQt (part bs sz 1 0) sz
                                                        , calcQt (part bs sz 0 1) sz
                                                        , calcQt (part bs sz 1 1) sz ]
                                                        (masses bs)
                                                        (com bs)
                                                        (sz * 2)
                in if   | floatLen > 1 -> split bs (sz / 2)
                        | floatLen == 1 -> QtExternal (Just (head bs)) sz
                        | otherwise -> QtExternal Nothing sz

totalGravity : Body -> QtNode -> (Float, Float) -> Float -> (Maybe Vector)
totalGravity b qt pos theta = case qt of
                                (QtExternal Nothing _) -> Nothing
                                (QtExternal (Just b2) s) -> Just (bodyGravity b b2)
                                (QtInternal ns m com s) -> totalGravity2 b qt pos theta
                                _ -> Nothing

totalGravity2 : Body -> QtNode -> (Float, Float) -> Float -> (Maybe Vector)
totalGravity2 b (QtInternal ns m com s) pos theta = let x = fst pos
                                                        y = snd pos
                                                        rpos = (x + s / 2, y + s / 2)
                                                        isFar = (1 / (dist rpos (bodyPos b))) < theta
                                                        poss =  [ (x, y)
                                                                , (x + s / 2, y)
                                                                , (x, y + s / 2)
                                                                , (x + s / 2, y + s / 2) ]
                                                        grav qt p = totalGravity b qt p theta
                                                        gravs ns = zipWith grav ns poss
                                                        sumGrav ma mb = case ma of
                                                                          Just a -> case mb of
                                                                           Just b -> Just (vecSum a b)
                                                                           Nothing -> Just a
                                                                          Nothing -> case mb of
                                                                           Just b -> Just b
                                                                           Nothing -> Nothing
                                                        virBody = Body com nilVec nilVec m 0
                                                        fd = Nothing
                                                   in
                                                        if      | isFar -> Just (bodyGravity b virBody)
                                                                | True -> foldr sumGrav fd (gravs ns)

bodyStep qt b = let     mfrc = totalGravity b qt (0, 0) 0.8
                        acc = case mfrc of
                                Just a -> vecMul a (1 / (bodyMass b))
                                Nothing -> Vector 0 0
                        vel = vecSum (bodyVel b) acc
                        pos = vecSum (bodyVecPos b) vel
                in
                        Body pos vel acc (bodyMass b) (bodySize b)


nbBody (NamedBody _ _ b) = b
nbName (NamedBody n _ _) = n
nbID (NamedBody _ x _) = x
nbStep nbs (NamedBody n i b) =  let neq b1 b2 = not ((nbID b1) == (nbID b2))
                                    bs = map nbBody (filter (neq (NamedBody n i b)) nbs)
                                    qt = calcQt bs 1
                                in (NamedBody n i (bodyStep qt b))


newSystem = BodySystem [] 0
systemBodies (BodySystem bs _) = map nbBody bs
systemNamedBodies (BodySystem bs _) = bs
systemAdd n p v m s (BodySystem bs lid) = BodySystem    ( (NamedBody n (lid + 1) (Body p v nilVec m s))
                                                          :: bs )
                                                        (lid + 1)
systemRemove (NamedBody _ i _) (BodySystem bs l) =      let nbs = filter (\b -> not ((nbID b) == i)) bs
                                                        in BodySystem nbs l
systemStep (BodySystem nbs l) clf =     let     bs = map nbBody nbs
                                                aclf b1 b2 =    let n1 = nbName b1
                                                                    n2 = nbName b2
                                                                    bb1 = nbBody b1
                                                                    bb2 = nbBody b2
                                                                in NamedBody    n1
                                                                                (nbID b1)
                                                                                (clf bb1 bb2 n1 n2)
                                                clc b1 b2 = let bb1 = nbBody b1
                                                                bb2 = nbBody b2
                                                            in
                                                             if | (nbID b1) == (nbID b2) -> b1
                                                                | bodyCollision bb1 bb2 -> aclf b1 b2
                                                                | otherwise -> b1
                                                clcs obs b = case obs of
                                                                x::[] -> clc b x
                                                                x::xs -> clc (clcs xs b) x
                                                newbs = map (clcs nbs) nbs
                                        in BodySystem (map (nbStep nbs) newbs) l
