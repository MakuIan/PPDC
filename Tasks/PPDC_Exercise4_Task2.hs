-- Code to build upon for the exercise
data Point = Point Int Int 
data Line = Line Point Point
data Curve = Curve [Point]
data Polygon = Polygon [Point]
data Circle = Circle Point Int -- Center and radius
data Ellipse = Ellipse Point Point -- Two foci
data Shape = SPoint Point
            | SLine Line
            | SCurve Curve
            | SPolygon Polygon
            | SCircle Circle
            | SEllipse Ellipse
instance Show Point where
  show (Point x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Show Line where
  show (Line p1 p2) = "Line " ++ show p1 ++ " " ++ show p2

instance Show Curve where
  show (Curve points) = "Curve " ++ show points

instance Show Polygon where
  show (Polygon points) = "Polygon " ++ show points

instance Show Circle where
  show (Circle center radius) = "Circle " ++ show center ++ " " ++ show radius

instance Show Ellipse where
  show (Ellipse foci1 foci2) = "Ellipse " ++ show foci1 ++ " " ++ show foci2

instance Show Shape where
  show (SPoint point) = "SPoint " ++ show point
  show (SLine line) = "SLine " ++ show line
  show (SCurve curve) = "SCurve " ++ show curve
  show (SPolygon polygon) = "SPolygon " ++ show polygon
  show (SCircle circle) = "SCircle " ++ show circle
  show (SEllipse ellipse) = "SEllipse " ++ show ellipse

-- Move a point by dx in the x direction and dy in the y direction
movePoint :: Point -> Int -> Int -> Point
movePoint (Point x y) dx dy = Point (x + dx) (y + dy)
-- Move a shape by dx in the x direction and dy in the y direction
--a
moveShape :: Shape -> Int -> Int -> Shape
moveShape (SPoint p) dx dy = SPoint (movePoint p dx dy)
moveShape (SLine (Line p1 p2)) dx dy = SLine (Line (movePoint p1 dx dy) (movePoint p2 dx dy))
moveShape (SCurve (Curve ps)) dx dy = SCurve (Curve $ movep ps dx dy)
    where 
        movep [] _ _= []
        movep (p:ps) dx dy = (movePoint p dx dy) : (movep ps dx dy)
moveShape (SCircle (Circle c r)) dx dy = SCircle (Circle (movePoint c dx dy) r)
moveShape (SEllipse (Ellipse f1 f2)) dx dy = SEllipse (Ellipse (movePoint f1 dx dy) (movePoint f2 dx dy))
moveShape (SPolygon (Polygon ps)) dx dy = SPolygon (Polygon $ movep ps dx dy)
  where 
    movep :: [Point] -> Int -> Int -> [Point]
    movep [] _ _= []
    movep (p:ps) dx dy = (movePoint p dx dy) : (movep ps dx dy)

--b
scalePoints :: [Point] -> Int -> [Point]
scalePoints [] _ = []
scalePoints (p:ps) k = (scalep p k) : (scalePoints ps k)
scalep :: Point -> Int -> Point
scalep (Point x y) k = Point (k * x) (k * y)  
scalePoint :: Point -> Int -> Point
scalePoint (Point x1 y1) k = Point (x1 * k) (y1 * k)

scaleShape :: Shape -> Int -> Shape
scaleShape (SPoint p) _ = SPoint p
scaleShape (SLine (Line p1 p2)) k = SLine (Line (scalePoint p1 k) (scalePoint p2 k))
scaleShape (SCurve (Curve ps)) k = (SCurve $ Curve $ scalePoints ps k)
scaleShape (SCircle (Circle c r)) k = SCircle (Circle c $ r * k)  
scaleShape (SPolygon (Polygon ps)) k = (SPolygon $ Polygon $ scalePoints ps k)
scaleShape (SEllipse (Ellipse p1 p2)) k = SEllipse (Ellipse  (scalePoint p1 k) (scalePoint p2 k))

--c
rotatePoint ::Point -> Double -> Point
rotatePoint (Point x y) r = Point x' y'
  where x' = round (cos r * fromIntegral x - sin r * fromIntegral y)
        y' = round (sin r * fromIntegral x + cos r * fromIntegral y)

points :: [Point] -> Double -> [Point]
points [] _ = []
points (p:ps) r = (point p r) : (points ps r) 
point :: Point -> Double -> Point
point p r = rotatePoint p r
rotateShape :: Shape -> Double -> Shape
rotateShape (SPoint p) r = SPoint (rotatePoint p r)
rotateShape (SLine (Line p1 p2)) r = SLine (Line (rotatePoint p1 r) (rotatePoint p2 r))
rotateShape (SCurve (Curve ps)) r = SCurve (Curve $ points ps r) 
rotateShape (SCircle (Circle c ra)) r = SCircle (Circle (rotatePoint c r) ra)  
rotateShape (SPolygon (Polygon ps)) r = SPolygon (Polygon (points pPs r))
rotateShape (SEllipse (Ellipse p1 p2)) r = SEllipse (Ellipse (rotatePoint p1 r) (rotatePoint p2 r))