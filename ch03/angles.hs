--Consider three two-dimensional points a, b, and c. If we look at the angle formed by the line segment from a to b and the line segment from b to c, it either turns left, turns right, or forms a straight line. Define a Direction data type that lets you represent these possibilities.
--
--Write a function that calculates the turn made by three 2D points and returns a Direction.

import Data.Ord
import Data.List

-- counterclockwise = left
-- clockwise = right
data Direction = LeftTurn | RightTurn | Straight
    deriving (Show, Eq)

data Point2D = Point2D {
x :: Double,
y :: Double
} deriving (Show, Eq)


a = Point2D 0.0 0.0
b = Point2D 4.0 0.0
c = Point2D 4.0 4.0
d = Point2D 2.0 6.0
e = Point2D 0.0 4.0
f = Point2D 2.0 2.0

points = [a,b,c,d,e,f]
points' = [d,e,a,f,c,b]
points'' = [c,f,e,b,d,a]

-- shamelessly stolen from https://en.wikipedia.org/wiki/Graham_scan#Pseudocode:
-- # Three points are a counter-clockwise turn if ccw > 0, clockwise if
-- # ccw < 0, and collinear if ccw = 0 because ccw is a determinant that
-- # gives twice the signed  area of the triangle formed by p1, p2 and p3.
-- function ccw(p1, p2, p3):
--     return (p2.x - p1.x)*(p3.y - p1.y) - (p2.y - p1.y)*(p3.x - p1.x)
turn :: Point2D -> Point2D -> Point2D -> Direction
turn (Point2D ax ay) (Point2D bx by) (Point2D cx cy)
    | ccw > 0 = LeftTurn
    | ccw < 0 = RightTurn
    | otherwise = Straight
        where
            ccw = (bx - ax)*(cy - ay) - (by - ay)*(cx - ax)
            -- ccw = counterclockwise = cross product

turns :: [Point2D] -> [Direction]
turns (a:b:c:cs) = (turn a b c) : (turns (b:c:cs))
turns _ = []

-- sort all points by miny, then minx
sortByCoords :: [Point2D] -> [Point2D]
sortByCoords xs = sortBy c xs
    where
        c (Point2D x1  y1) (Point2D x2 y2) = case compare y1 y2 of
            LT -> LT
            GT -> GT
            EQ -> compare x1 x2

-- keep fst point and sort rest by their slope compared to fst point (e.g. sort by polar angle)
sortByAngle :: [Point2D] -> [Point2D]
sortByAngle ((Point2D x1 y1):(xs)) = (Point2D x1 y1) : (sortBy c xs)
    where
        c (Point2D x2 y2) (Point2D x3 y3) = compare slope1 slope2
            where
                slope1 = (y2-y1)/(x2-x1)
                slope2 = (y3-y1)/(x3-x1)

-- filter out all points not part of the hull by using grahams algo
-- (i.e. make sure to only turn left on next point starting by lowest-leftward point)
getConvexHull :: [Point2D] -> [Point2D]
getConvexHull (p1:p2:p3:[])
    | turn p1 p2 p3 == RightTurn = (p1:p3:[]) -- discard p2 because it's inside the convex hull
    | otherwise = (p1:p2:p3:[]) -- p2 is part of the hull
getConvexHull (p1:p2:p3:ps)
    | turn p1 p2 p3 == RightTurn = getConvexHull (p1:p3:ps) -- again, discard p2 and try p1,p3,p4 to check
    | otherwise = p1:[] ++ getConvexHull (p2:p3:ps) -- p2 is part of convex hull, continue with p2,p3,p4

grahamScan :: [Point2D] -> [Point2D]
grahamScan xs = getConvexHull (sortByAngle (sortByCoords xs))
