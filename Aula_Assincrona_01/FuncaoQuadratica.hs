-- Funções quadraticas :  

        -- Entrada: Tres valores do conjunto dos IR
        -- Saida: Valores do conjunto dos IR para soluções (boolena na primeira função apenas para a verificação do delta).  


-- Forma mais primitica com a verificação do delta a parte das soluções (sem condicionais). 

verifiqueDelta :: Float->Float->Float->Bool 
verifiqueDelta a b c = (b*b -4*a*c) >= 0

solucaoX01 :: Float->Float->Float->Float 
solucaoX01 a b c = (sqrt (b*b -4*a*c) -b) / 2*a

solucaoX02 :: Float->Float->Float->Float 
solucaoX02 a b c = (-sqrt (b*b -4*a*c) - b) / 2*a

-- Soluções mais pratica (com condicional).

        -- Com duas funções

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

        -- Com apenas uma função


raizes :: Float -> Float -> Float -> String
raizes a b c
                | a == 0 = error "Não eh possivel calcular as raizes, por favor verifique os dados digitados e tente novamente: a = 0."
                | delta >= 0 = "As raizes da equacao sao: " ++ show( (sqrt delta -b)/2*a ) ++ " e " ++ show( (sqrt delta + b)/2*a )
                | otherwise = error "Nao ha raizes reais"
                where delta = b*b - 4*a*c
