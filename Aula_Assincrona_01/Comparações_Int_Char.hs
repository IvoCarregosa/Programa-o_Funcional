import Data.Char ( ord, chr )

-- Comparações Int


        -- Inteiros Iguais

inteirosIguais :: Int->Int->Bool
inteirosIguais a b = a==b


        -- Inteiros Iguais em valor ansoluto.

iguaisAbs :: Int->Int->Bool  
iguaisAbs a b = abs a == abs b

        -- Dados três inteiros retonar True caso sejam todos diferentes 
        
            -- Sem a negação

intTresDif :: Int -> Int -> Int -> Bool
intTresDif a b c = a /= b || b/= c  || a /= c

            -- Com a negação

intTresDif02 :: Int->Int->Int->Bool
intTresDif02 a b c = not(a==b && b==c)

        -- Dados um inteiro retonar 1 se for positivo, -1 se for negativo e 0 se for iguai a 0

                -- Com If Else

sinalInt :: Int -> Int 
sinalInt n = if n == 0 then 0 else div n n 

                -- Com Guards

sinalInt02 :: Int -> Int
sinalInt02 n 
                | n == 0 = 0
                | otherwise = div n n

-- Comparações Char


        -- Letras Iguais

charIguais :: Char -> Char -> Bool 
charIguais a b = a == b


        -- Dados dois Char retornar o menor deles (segundo a ordem alfabética) 

            -- Sem Condicionais

menorChar :: Char-> Char -> Char 
menorChar a b = chr( (ord a + ord b - abs (ord a - ord b)) `div` 2 )

            -- Com Condicionais 

menorChar02 :: Char -> Char -> Char 
menorChar02 a b = if a < b then a else b

menorChar03 :: Char -> Char -> Char 
menorChar03 a b 
                | a < b = a
                | otherwise = b

            -- Com Funções Reservadas

menorChar04 :: Char -> Char -> Char 
menorChar04 = min


        -- Dados dois Char retornar o maior deles (segundo a ordem alfabética)        

            -- Sem Condicionais

maiorChar :: Char-> Char -> Char 
maiorChar a b = chr( (ord a + ord b + abs (ord a - ord b)) `div` 2 )

            -- Com Condicionais 

maiorChar02 :: Char -> Char -> Char 
maiorChar02 a b = if a > b then a else b

maiorChar03 :: Char -> Char -> Char 
maiorChar03 a b 
                | a > b = a
                | otherwise = b

            -- Com Funções Reservadas

maiorChar04 :: Char -> Char -> Char 
maiorChar04 = max



-- Verificar se o Char digitado digito

ehDigito :: Char -> Bool
ehDigito x = ('0' <= x) && (x <= '9')
