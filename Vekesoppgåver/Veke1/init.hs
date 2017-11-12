init xs = tail (reverse xs)
-- or
init xs = drop 1 (reverse xs)