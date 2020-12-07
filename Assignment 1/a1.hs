module A1c where

-- Calculates dot product of two pairs
-- scale - Number to scale the product by
-- pair1 - pair of numbers to perform dot product on
-- pair2 - pair of numbers to perform dot product on
-- Returns result of dot product as number
sDotProduct::(Num a)=>a->(a, a)->(a, a)->a
sDotProduct scale pair1 pair2 = scale * ((fst pair1) * (fst pair2)
 + (snd pair1) * (snd pair2))

-- Calculates the cartesian distance of two 2-D coordinates
-- pair1 - first coordinate as a pair of numbers
-- pair2 - second coordinate as a pair of numbers
-- Returns the cartesian distance as a number
distance::(Num a, Floating a)=>(a, a)->(a, a)->a
distance pair1 pair2 = sqrt(((fst pair2) - (fst pair1))^2 + ((snd pair2) - (snd pair1))^2)

-- Calculates the cartesian distance of two 3-D coordinates
-- (x, y, z) - first coordinate as a triple of numbers
-- (a, b, c) - second coordinate as a triple of numbers
-- Returns the cartesian distance as a number
tripleDistance::(Num a, Floating a)=>(a, a, a)->(a, a, a)->a
tripleDistance (x, y, z) (a, b, c) = sqrt((x - a)^2 + (y - b)^2 + (z - b)^2)

-- Finds the lowest number in a list
-- list - the list to find the lowest number in
-- Returns the value of the lowest number
findMin::(Num a, Ord a)=>[a]->a
findMin [] = error "No minimum of an empty list"
findMin list = if (tail list) == [] 
    then (head list)
    else if (head list) < findMin (tail list)
        then (head list)
        else findMin (tail list)

-- Calculates the dot product of two lists
-- list1 - first list of numbers
-- list2 - second list of numbers
-- Returns the dot product as a float
tupleDotProduct::(Num a, Eq a)=>[a]->[a]->a
tupleDotProduct [] [] = 0
tupleDotProduct list1 list2 = if (tail list1) == []
    then (head list1) * (head list2)
    else (head list1) * (head list2) + tupleDotProduct (tail list1) (tail list2)

-- Creates a list of pairs from each list in reverse order
-- list1 - list of values for each pair
-- list2 - list of keys for each pair
-- Returns a list of pairs
revZip2Lists::(Eq a)=>[a]->[b]->[(b, a)]
revZip2Lists [] [] = []
revZip2Lists list1 list2 = if (tail list1) == []
    then ((head list2), (head list1)) : []
    else revZip2Lists (tail list1) (tail list2) ++ [((head list2), (head list1))]

-- Creates a list of every third item in the list supplied.
-- list - the list to extract items from
-- Returns a list of items
everyThird::[a]->[a]
everyThird [] = []
everyThird list = if (length list) < 3
    then []
    else (head (tail (tail list))) : everyThird (tail (tail (tail list)))