
todosDiferentes :: Float -> Float -> Float -> Bool
todosDiferentes a b c = a /= b && a /= c && b /= c

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

solRaiz01 :: Float -> Float -> Float -> Float
solRaiz01 a b c
                    | a == 0 = error "Não eh possivel calcular as raizes, por favor verifique os dados digitados e tente novamente: a = 0."
                    | delta >= 0 = ( sqrt delta - b)/2*a
                    | otherwise = error "Nenhuma Raiz Real"
                    where delta = b*b - 4*a*c

solRaiz02 :: Float -> Float -> Float -> Float
solRaiz02 a b c
                    | a == 0 = error "Não eh possivel calcular as raizes, por favor verifique os dados digitados e tente novamente: a = 0."
                    | delta >= 0 = (- sqrt delta - b)/2*a
                    | otherwise = error "Nenhuma Raiz Real"
                    where delta = b*b - 4*a*c

raizes :: Float -> Float -> Float -> String
raizes a b c
                | a == 0 = error "Não eh possivel calcular as raizes, por favor verifique os dados digitados e tente novamente: a = 0."
                | (b*b - 4*a*c) >= 0 = "As raizes da equacao sao: " ++ show( (sqrt(b*b - 4*a*c) -b)/2*a ) ++ " e " ++ show( (sqrt(b*b - 4*a*c) + b)/2*a )
                | otherwise = error "Nao ha raizes reais"

m :: Float -> Float -> Float -> Float
m a b c = (a+b+c)/3

acimaDaMedia02 :: Float -> Float -> Float -> String
acimaDaMedia02 a b c
                    | a == b && b == c = "Nenhum esta acima da media."
                    | a > m a b c && b < m a b c && c < m a b c  = "Um numero esta acima da media."
                    | b > m a b c && a < m a b c && c < m a b c  = "Um numero esta acima da media."
                    | c > m a b c && b < m a b c && a < m a b c  = "Um numero esta acima da media."
                    | otherwise = "Dois numeros estao acima da media."

(*+) :: Int -> Int -> Int
a *+ b = 2*(a+b)

(***) :: Int -> Int -> Int
a *** b = a*b*a*b

(||&) :: Bool -> Bool -> Bool
a ||& b
    | a == b = False
    | otherwise = True

nAnd :: Bool -> Bool -> Bool
nAnd True True = False
nAnd x y = True

nAnd02 :: Bool -> Bool -> Bool
nAnd02 True True = False
nAnd02 _ _ = True

ouExclu :: Bool -> Bool -> Bool
ouExclu a b
    | a == b = False
    |otherwise = True


ehMultiplo :: Int -> Int -> String
ehMultiplo a b
                | mod (a*b) 3 == 0 && mod (a*b) 5 == 0 = "Multiplo de 3 e de 5"
                | (mod (a*b) 3 == 0) && (mod (a*b) 5 /= 0) = "Multiplo de 3"
                | (mod (a*b) 3 /= 0) && (mod (a*b) 5 == 0) = "Multiplo de 5"
                | otherwise = "Nao eh multiplo de 3 nem de 5"

ehQuadrado :: Int -> Int -> String
ehQuadrado a b
                | a == b = "Eh Quadrado"
                | otherwise = "Eh Retangulo"

-- Considerando que quadrado não é retangulo
areaRetangulo :: Float -> Float -> Float
areaRetangulo a b
                    | a /= b = a * b
                    | otherwise = error "Não eh retangulo"

-- Considerando que todo quadrado é retangulo
areaRetangulo02 :: Float -> Float -> Float
areaRetangulo02 a b = a * b

elementoMeio :: Float -> Float -> Float -> Float 
elementoMeio a b c 
              
              -- b ou c < a < c ou b  -> a 
              | (b <= a && a <= c) || (c <= a && a <= b) = a   
              
              -- a ou c < b < c ou a  -> b
              | (a <= b && b <= c) || (c <= b && b <= a) = b
              
              -- a ou b < c < b ou a  -> c
              | (a <= c && c <= b) || (b <= c && c <= a) = c


-- even verifica se o numero é par

ehPar :: Int -> Int
ehPar x
        | even x  = 1
        | otherwise = 0


quantPar :: Int -> Int -> Int -> Int -> Int
quantPar a b c d = ehPar a + ehPar b + ehPar c + ehPar d




doisInt :: Int -> Int -> Int 
doisInt a b 
            | a == b = 1
            | otherwise = 0

tresInt ::  Int -> Int -> Int -> Int
tresInt a b c 
              | soma == 1 = 2
              | soma == 0 = 0
              | otherwise = soma  
              where soma = doisInt a b + doisInt a c + doisInt b c

-- Cuboide = Paralalepipedo 

-- Volume de um cuboide 

volumeCuboide :: Float -> Float -> Float ->Float 
volumeCuboide a b c = areaRetangulo02 a b * c

volumeCuboide02 :: Float -> Float -> Float ->Float
volumeCuboide02 a b c = 
                        let areaBase = a * b 
                            altura = c
                        in areaBase * altura

volumeCuboide03 :: Float -> Float -> Float -> Float 
volumeCuboide03 a b c = areaBase * c 
    where areaBase = a * b

-- Area superficial de um cuboide

areaCuboide :: Float -> Float -> Float -> Float 
areaCuboide a b c = 2 * (areaRetangulo02 a b + areaRetangulo02 b c + areaRetangulo02 a c)

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

significadoCor :: String -> String 
significadoCor "branco" = "Paz"
significadoCor "amarelo" = "Alegria"
significadoCor "verde" = "Esperanca"
significadoCor "azul" = "Tranquilidade"
significadoCor "vermelho" = "Paixao"
significadoCor _ = "Desculpe, essa cor nao consta no nosso banco de dados"

diferenteDe :: Int -> Int -> Int -> Int 
diferenteDe a b c = if a /= b && a /= c then 1 else 0

tresDiferentes :: Int -> Int -> Int -> Int 
tresDiferentes a b c = diferenteDe a b c + diferenteDe b a c + diferenteDe c b a 

unicos :: Int -> Int -> Int -> Int
unicos a b c = diferenteDe a b c + diferenteDe b a c + diferenteDe c b a




elementoMeio02 :: Float -> Float -> Float -> Float 
elementoMeio02 a b c 
              
              -- b ou c < a < c ou b  -> a 
              | (b <= a && a <= c) || (c <= a && a <= b) = a   
              
              -- a ou c < b < c ou a  -> b
              | (a <= b && b <= c) || (c <= b && b <= a) = b
              
              -- a ou b < c < b ou a  -> c
              | otherwise = c