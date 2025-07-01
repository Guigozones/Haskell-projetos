type Horario = (Int, Int, Int)

validar :: Horario -> Bool
validar (h, m, s)
    | h < 0 || h > 23 = False
    | m < 0 || m > 59 = False
    | s < 0 || s > 59 = False
    | otherwise       = True

informasegundos :: Horario -> Int
informasegundos (h, m, s)
    | validar (h, m, s) = h * 3600 + m * 60 + s
    | otherwise         = error "Horário inválido"

type Quatroint = (Int, Int, Int, Int)

convertehora :: Quatroint -> Horario
convertehora (a, h, m, s)
    | a >= 3600 = convertehora (a - 3600, h + 1, m, s)
    | a >= 60   = convertehora (a - 60, h, m + 1, s)
    | otherwise = (h, m, a)

double:: Int -> Int
double x = x + x

todosIguais:: Eq a => a -> a -> a -> Bool
todosIguais p s t = (p == s) && (s == t)

somaDobro::Int -> Int ->Int
somaDobro x y = double x + double y

todosIguaisPI = todosIguais 3.14
todosIguaisPi s t = todosIguais 3.14 s t