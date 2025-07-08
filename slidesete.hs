main :: IO ()
main = do
    caracter <- getChar
    putChar caracter

somaNumeros :: (Int, Int) -> Int
somaNumeros (s, j) = s + j

type Pessoa = (String, String, Int)
nome :: Pessoa -> (String, String)
nome(n, p, a) = (n, p)

digitaCliente :: Pessoa -> (String, String)
digitaCliente pessoa = nome pessoa

type Hora = (Int, Int, Int)
verificaHora :: Hora -> Bool
verificaHora (a, b, c)
    |a > 24 || a < 0 = False
    |b > 59 || b < 0 = False
    |c > 59 || c < 0 = False
    |otherwise = True

horazero :: Hora -> Int
horazero (a, b, c)
    |verificaHora(a, b, c) = a*3600 + b*60 + c
    |otherwise = error "horario Invalido"

segundoHora :: Int -> Hora
segundoHora a = segundoHoraCurry (a, 0, 0, 0)

segundoHoraCurry :: (Int, Int, Int, Int) -> Hora
segundoHoraCurry (a, b, c, d)
    |a > 3600 = segundoHoraCurry (a- 3600, b+1, c, d)
    |a > 60 = segundoHoraCurry (a- 60, b, c+1, d)
    |otherwise = (a, b, c)

diffHoras :: (Hora, Hora) -> Int
diffHoras (a, b) = horazero a - horazero b
     