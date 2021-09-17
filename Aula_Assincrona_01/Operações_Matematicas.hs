import Data.Char ( ord )

-- Soma de Dois Inteiros

soma :: Int->Int->Int 
soma a b = a + b

-- Diferença de Dois Inteiros

sub :: Int->Int->Int 
sub a b = a - b

-- Multiplique a Divisão Inteira de Dois Números por 3

triploD :: Int->Int->Int  
triploD a b = 3 * div a b 

-- Soma Char com Int e converte para Float

somaConverteCharInt :: Char->Int->Float  
somaConverteCharInt a b = fromIntegral(ord a + b)

-- Eleva um inteiro ao quadrado

intAoQuadrado :: Int->Int 
intAoQuadrado x = x*x

-- Soma Dois a um inteiro

soma2 :: Int->Int 
soma2 x = x + 2

-- Soma Dois a um inteiro e eleva ao quadrado

soma2AoQuadrado :: Int->Int 
soma2AoQuadrado x =  (x+2)*(x+2)

-- Média de quatro Floats digitados

media :: Float->Float->Float->Float->Float 
media a b c d = (a+b+c+d)/4