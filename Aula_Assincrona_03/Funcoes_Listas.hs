import Data.Char

-- Calcula o produto de todos os elementos pares de uma dada função
    -- A uma limitação significativa no número de elementos da lista por conta da saida em Int

productPar :: [Int] -> Int
productPar list = product [x | x <- list, even x]

-- Função para repetir um dado elemento 30 vezes

repita_30 :: a -> [a]
repita_30 = replicate 30

-- Verificador de palindromos 

palindromo :: String -> String -> Bool
palindromo name01 name02 = name01 == reverse name01 && name02 == reverse name02


-- Função para concaternar duas listas (Nome, Idade)
    -- Função Zip : zip [type_01] [type_02] -> [(type_01, type_02)]
        -- Transforma duas listas em uma lista de tuplas.

type Nome = String
type Idade = Int
type Individuo = (Nome, Idade)

idBasic :: [Nome] -> [Idade] -> [Individuo]
idBasic = zip

-- Função para transformar uma lista de tuplas em duas listas
    -- Função Unip : unzip  [(type_01, type_02)] -> ([type_01], [type_02])
        -- Transforma uma lista de tuplas em duas listas. Processo inverso ao zip.

type Estado = String
type Capital = String
type Regiao = (Estado, Capital)

estadoCapital :: [Regiao] -> ([Estado] , [Capital])
estadoCapital = unzip

-- Função para calcular a soma dos elementos maiores que 5 de uma lista
    -- Recebe reais e retorna reais

somaMaiores05 :: [Float] -> Float
somaMaiores05 list = sum [x | x <- list, x >= 5 ]

-- Função para calcular a media dos valores da lista 
    -- Recebe reais e retorna reais

mediaList :: [Float] -> Float
mediaList list = sum list / fromIntegral(length list)

-- Função para devolver os valores acima da media de uma lista

acimaMedia :: [Float] -> [Float]
acimaMedia list = [ x | x <- list, x > mediaList list ]

-- Função que recebe uma lista de string e printa as string da lista com quebra de linha.
    
    -- Adiciona \n no final de uma string qualquer 
acrescentN :: String -> String
acrescentN list = list++"\n"

    -- Usando a função acima acrescenta-se \n no final de cada string da lista e junta tudo em uma unica string
acrescentN02 :: [String] -> String
acrescentN02 list = concat [ acrescentN x | x <- list]
    
    -- Usando as duas funções anteriores chegamos no objetivo final de acrescentar uma lista de string e printa-las na tela com quebra de linha.
printSpace :: [String] -> IO ()
printSpace list = putStrLn (acrescentN02 list)

-- Função para ajustar uma string em um determinado limite de caracteres 

acrescentSpace :: String -> Int -> String
acrescentSpace list n = replicate space ' ' ++ list
                        where
                                space = n - length list

-- Sistema Biblioteca 
    
    -- Declaração de Tipos e "Banco de Dados"

type Pessoa = String
type Livro = String
type Emprestimos = [(Pessoa, Livro)]
bancoDeEmprestimos = [("Julia","A CULPA EH DO CABRAL"), ("Julia","MORTE VIDA SEVERINA"), ("Julia","DOM CASMURRO, REI DELAS"), ("Eudardo", "MORTE VIDA SEVERINA"),("Quixote","DOM CASMURRO, REI DELAS")]

    -- Função para tratamento de string (para melhorar a comparação)

conMaiusc :: String -> String 
conMaiusc list = [ toUpper a | a <- list ]

    -- Função para saber quais livros emprestados uma dada pessoa possui 

livroEmprestado :: Pessoa -> Livro
livroEmprestado nome
                        | null books = "Essa pessoal não possui livros emprestados"
                        | length books == 1 = "Essa pessoa pegou emprestado apenas o livro: " ++ head books
                        | length books > 1 = "Essa pessoa pegou os seguintes livros emprestados: " ++ concat books01
                        
                        where 
                                books = [livro | (nome02, livro) <- bancoDeEmprestimos, conMaiusc nome == conMaiusc nome02]
                                books01 = [livro++"; " | (nome02, livro) <- bancoDeEmprestimos, conMaiusc nome == conMaiusc nome02]

    -- Função para saber quais pessoas pegaram exemplares de um dado livro

pessoaEmprest :: Livro -> Pessoa
pessoaEmprest livro
                        | null people = "Nenhum exemplar desse livro foi emprestado."
                        | length people == 1 = "Apenas " ++ head people ++ " pegou esse livro emprestado."
                        | length people > 1 = "As pessoas que pegaram esse livro emprestado foram: " ++ concat people01
                        
                        where 
                                people = [conMaiusc nome | (nome, livro02) <- bancoDeEmprestimos, conMaiusc livro == conMaiusc livro02]
                                people01 = [conMaiusc nome++"; " | (nome, livro02) <- bancoDeEmprestimos, conMaiusc livro == conMaiusc livro02]

    -- Função para saber quantos exemplares de um dado livro foram emprestados

saidaLivro :: Livro -> String 
saidaLivro livro 
                    | numBooks == 0 = "Este livro nao possui exemplares emprestados."
                    | numBooks == 1 = "Este livro possui um exemplar emprestado."
                    | otherwise = "Este livro possui um total de " ++ show numBooks ++ " exemplares emprestados."

                    where 
                            books = [livro | (nome, livro02) <- bancoDeEmprestimos, conMaiusc livro == conMaiusc livro02]
                            numBooks = length books
            
    --  Função para saber quantos exemplares uma dada pessoa pegou emprestado

saidaPorPessoa :: Pessoa -> String
saidaPorPessoa nome
                
                | numBooks == 0 = "Essa pessoa nao possui exemplares de livros."
                | numBooks == 1 = "Essa pessoa possui um exemplar de livro."
                | otherwise = "Essa pessoa tomou emprestado um total de " ++ show numBooks ++ " exemplares." 

                where   
                       people = [livro | (nome02, livro) <- bancoDeEmprestimos, conMaiusc nome == conMaiusc nome02]
                       numBooks = length people

    -- Função para cadastrar o emprestimo de um livro dada uma tupla (Pessoa, Livro)

emprestCadastro :: (Pessoa, Livro) -> Emprestimos 
emprestCadastro tupla = tupla:bancoDeEmprestimos 

    -- Função para remover uma pessoa ja cadastrada no banco de dados dada uma tupla (Pessoa, Livro)

removeCadastro :: (Pessoa, Livro) -> Emprestimos
removeCadastro tupla = [ (nome01, livro01) | (nome01, livro01) <- bancoDeEmprestimos, (nome01, livro01) /= tupla  ]

-- Spoiler da lista de Prog. Funcional

{-- cadastroSaida :: Emprestimos -> (Pessoa, Livro) -> [(Pessoa, Livro)]
cadastroSaida banco tupla = tupla:banco --} 
