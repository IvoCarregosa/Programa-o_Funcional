-- Segunda Aula Assíncrona (Operadores e Função Composta) 

    -- Retonar True se todos os números digitados forem diferentes
     
tresDiferentes :: Float -> Float -> Float -> Bool
tresDiferentes a b c = a /= b && a /= c && b /= c
        
    -- Retorna False so em caso de todos os números serem iguais

        -- Forma 01

naoIguais :: Float -> Float -> Float -> Bool
naoIguais a b c = a /= b || a /= c || b /= c

        -- Forma 02

naoIguais02 :: Float -> Float -> Float -> Bool
naoIguais02 a b c = not(a == b && b == c)

    -- Dados tres inteiros conta quantos são unicos (diferente)

diferenteDe :: Int -> Int -> Int -> Int 
diferenteDe a b c = if a /= b && a /= c then 1 else 0

unicos :: Int -> Int -> Int -> Int
unicos a b c = diferenteDe a b c + diferenteDe b a c + diferenteDe c b a



menorEntreDois :: Float -> Float -> Float
menorEntreDois a b
                    | a < b = a
                    | otherwise = b

menorEntreTres :: Float -> Float -> Float -> Float
menorEntreTres a b c
                    | a < b && a < c = a
                    | b < a && b < c = b
                    | c < a && c < b = c
                    | a == b && a < c = a
                    | c == b && c < a = b
                    | otherwise = a

acimaDaMedia :: Float -> Float -> Float -> String
acimaDaMedia a b c
                    | a == b && b == c = "Nenhum esta acima da media."
                    | a > media && b < media && c < media  = "Um numero esta acima da media."
                    | b > media && a < media && c < media  = "Um numero esta acima da media."
                    | c > media && b < media && a < media  = "Um numero esta acima da media."
                    | otherwise = "Dois numeros estao acima da media."
                    where media = (a+b+c)/3

m :: Float -> Float -> Float -> Float
m a b c = (a+b+c)/3

acimaDaMedia02 :: Float -> Float -> Float -> String
acimaDaMedia02 a b c
                    | a == b && b == c = "Nenhum esta acima da media."
                    | a > m a b c && b < m a b c && c < m a b c  = "Um numero esta acima da media."
                    | b > m a b c && a < m a b c && c < m a b c  = "Um numero esta acima da media."
                    | c > m a b c && b < m a b c && a < m a b c  = "Um numero esta acima da media."
                    | otherwise = "Dois numeros estao acima da media."

-- Criando Operadores

        -- Multiplica a soma de dois inteiros por dois

(*+) :: Int -> Int -> Int
a *+ b = 2*(a+b)

        -- Recebe dois inteiros e eleva o produto deles ao quadrado

(***) :: Int -> Int -> Int
a *** b = a*b*a*b

        -- Recebe dois Booleanos e retorna False se forem iguais ou True se forem diferentes

(||&) :: Bool -> Bool -> Bool
a ||& b
    | a == b = False
    | otherwise = True

        -- Negando o valor logico de " E "

                -- Forma 01

nAnd :: Bool -> Bool -> Bool
nAnd True True = False
nAnd x y = True
                -- Forma 02

nAnd02 :: Bool -> Bool -> Bool
nAnd02 True True = False
nAnd02 _ _ = True
        
        -- " OU " exclusivo 

ouExclu :: Bool -> Bool -> Bool
ouExclu a b
    | a == b = False
    |otherwise = True

-- Dado um Inteiro verifica se esse é multiplo de 3 ou de 5

ehMultiplo :: Int -> String
ehMultiplo a
                | mod a 3 == 0 && mod a 5 == 0 = "Multiplo de 3 e de 5"
                | mod a 3 == 0 && mod a 5 /= 0 = "Multiplo de 3"
                | mod a 3 /= 0 && mod a 5 == 0 = "Multiplo de 5"
                | otherwise = "Nao eh multiplo de 3 nem de 5"

-- Dado a medida de dois lados perpendiculares de um retangulo verificar se é quadrado 

ehQuadrado :: Int -> Int -> String
ehQuadrado a b
                | a == b = "Eh Quadrado"
                | otherwise = "Eh Retangulo"

-- Dado tres Floats retornar o número do meio seguindo uma ordem crescente ou decrescente 

elementoMeio :: Float -> Float -> Float -> Float 
elementoMeio a b c 
              
              -- b ou c < a < c ou b  -> a 
              | (b <= a && a <= c) || (c <= a && a <= b) = a   
              
              -- a ou c < b < c ou a  -> b
              | (a <= b && b <= c) || (c <= b && b <= a) = b
              
              -- a ou b < c < b ou a  -> c
              | otherwise = c


-- Dado quatro números inteiros retorna quantos deles são pares

        -- verifica se o número é par e retorna 1 em caso verdadeiro ou 0 caso ele não seja par (even verifica se o numero é par)

ehPar :: Int -> Int
ehPar x
        | even x  = 1
        | otherwise = 0
        
        -- retorna a soma dos pares

quantPar :: Int -> Int -> Int -> Int -> Int
quantPar a b c d = ehPar a + ehPar b + ehPar c + ehPar d

-- Dado três números inteiros retornar quantos deles são iguais

        -- verifica se dois números são iguais

doisInt :: Int -> Int -> Int 
doisInt a b 
            | a == b = 1
            | otherwise = 0

        -- retorna a quantidade de números diferentes

tresInt ::  Int -> Int -> Int -> Int
tresInt a b c 
              | soma == 1 = 2
              | soma == 0 = 0
              | otherwise = soma  
              where soma = doisInt a b + doisInt a c + doisInt b c

-- Calculando o volume de um cuboide qualquer 

volumeCuboide :: Float -> Float -> Float ->Float 
volumeCuboide a b c = a * b * c

volumeCuboide02 :: Float -> Float -> Float ->Float
volumeCuboide02 a b c = 
                        let areaBase = a * b 
                            altura = c
                        in areaBase * altura

volumeCuboide03 :: Float -> Float -> Float -> Float 
volumeCuboide03 a b c = areaBase * c 
    where areaBase = a * b

-- Calculando a area superficial de um cuboide qualquer

areaCuboide :: Float -> Float -> Float -> Float 
areaCuboide a b c = 2 * (area01+ area02 + area03)
            where area01 = a *b ; area02 = a *c ; area03 = b*c

areaCuboide02 :: Float -> Float -> Float ->Float
areaCuboide02 a b c =
                        let produto01 = a * b
                            produto02 = b * c
                            produto03 = a * c
                        in 2 * (produto01 + produto02 + produto03) 

areaCuboide03 :: Float -> Float -> Float ->Float
areaCuboide03 a b c = 2 * (area01 + area02 + area03)
    where area01 = a *b ; area02 = a *c ; area03 = b*c

areaCuboide04 :: Float -> Float -> Float ->Float 
areaCuboide04 a b c = 2*(a*c + a*b + b*c )

-- Dando significado para cor digitada 

significadoCor :: String -> String 
significadoCor "branco" = "Paz"
significadoCor "amarelo" = "Alegria"
significadoCor "verde" = "Esperanca"
significadoCor "azul" = "Tranquilidade"
significadoCor "vermelho" = "Paixao"
significadoCor _ = "Desculpe, essa cor nao consta no nosso banco de dados"
