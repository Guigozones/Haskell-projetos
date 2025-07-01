toBits:: Int -> [Int]
toBits 0 = [0]
toBits 1 = [1]
toBits n = toBits (div n 2) ++ [(mod n 2)]

fromBits:: [Int]->Int
-- fromBits list = sum [element * (2 ^ bit) | (element, bit) <- zipped]
--  where 
--     n = length list

--     bits = [n - 1, n - 2 .. 0]

--     zipped = zip list bits

fromBits [] = 0
fromBits (a:xs) = 2^(length xs)*a + fromBits xs