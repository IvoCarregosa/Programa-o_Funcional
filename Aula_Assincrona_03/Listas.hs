import Data.Char


tupla = (lista01, lista02, lista03)
    where
        lista01 = [3, 8]
        lista02 = [3.1, 8]
        lista03 = ['a'..'z']

-- Função para retornar se os elementos da lista são Impares 

    -- Forma 01 

        -- Sem função predefinida 

impar :: Int -> Bool
impar a = mod a 2 /= 0

ehImpar :: [Int] -> [Bool]
ehImpar x = [ impar a | a <- x]

    -- Forma 02

        --Com fução predefinida

            -- Função predefinida "odd" retorna True em caso de número Impar e False em caso de número Par, semelhante a função "even" 

ehImpar02 :: [Int] -> [Bool]
ehImpar02 x = [ odd a | a <- x]

-- Função para selecionar os inteiros de uma lista e retornar o triplo deles

ehImparX3 :: [Int] -> [Int]
ehImparX3 list = [3 * x | x <- list, odd x]

-- Função para transformar as letras minusculas de uma lista em maiusculas.

    -- Forma 01
        -- Sem função predefinida 

maiusc :: Char -> Char 
maiusc a 
           | ord a >= 97 && ord a <= 122 = chr(ord a - 32)
           | otherwise = a

convertMaiusc :: String -> String
convertMaiusc list = [ maiusc a | a <- list ]

    -- Forma 02
        -- Com função predefinida

convertMaiusc02 :: String -> String 
convertMaiusc02 list = [ toUpper a | a <- list ]

-- Função para retornar os números reais (dada em lista) no intervalo [0, 100]

    --Dados dois Float
        -- Exige menos memória, mas ha uma limitação menor no número de casas decimais

zeroCem :: [Float] -> [Float]
zeroCem list = [ a | a <- list, 0 <= a , a <= 100 ]

    --Dados dois Double
        -- Aceita números com mais casas decimais, mas exige mais memória.

zeroCem02 :: [Double] -> [Double]
zeroCem02 list = [ a | a <- list, 0 <= a , a <= 100 ]

-- Função para retornar uma String sem os caracteres que são digitos

    -- Forma 01
        -- Sem função predefinida

ehDigito :: Char -> Bool 
ehDigito a = not (48 <= ord a && ord a <= 57)

semDigit :: String -> String 
semDigit list = [ a | a <- list, ehDigito a]

    -- Forma 02
        -- Com função predefinida

semDigit02 :: String -> String 
semDigit02 list = [ a | a <- list, not(isDigit a) ]

-- Função para retornar a combinação de duas listas, uma de nome e outra de adjetivo
    -- OBS: Defini os tipos so por questão de organização

type Nome = String 
type Adjetivo = String
type NomeAdjetivo = String  

nomeAdj :: [Nome] -> [Adjetivo] -> [NomeAdjetivo]
nomeAdj nome adjetivo = [n ++ " " ++ a | n <- nome, a <- adjetivo]