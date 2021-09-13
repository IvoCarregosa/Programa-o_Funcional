-- Figuras Planas

    -- Areas

aQuadrado :: Float ->Float 
aQuadrado l = l*l

aRetangulo :: Float ->Float -> Float 
aRetangulo a b = a*b

aTriangulo :: Float -> Float -> Float 
aTriangulo h b = h*b/2 

aTrapezio :: Float -> Float -> Float -> Float  
aTrapezio bMa bMe h = (bMa + bMe)*h/2

aCirculo :: Float -> Float 
aCirculo r = r*r*pi 

    -- Perimetro

pQuadrado :: Float -> Float 
pQuadrado l = 4*l

pRetangulo :: Float -> Float -> Float 
pRetangulo a b = (a+b)*2

pTriangulo :: Float -> Float -> Float -> Float 
pTriangulo a b c = a+b+c

pTrapezio :: Float -> Float-> Float -> Float-> Float
pTrapezio bMa bMe l1 l2 = bMa + bMe + l1 + l2

pCirculo :: Float -> Float 
pCirculo r = 2*r*pi 

    -- Verifique se é triângulo

ehTriangulo :: Float -> Float -> Float -> Bool 
ehTriangulo a b c = a < b + c && b < a +c && c < a + b

