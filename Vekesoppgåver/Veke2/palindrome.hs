palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs