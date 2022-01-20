-- PEDRO VINÍCIUS DE ARAUJO BARRETO : 
-- LUCAS GAMA VIEIRA DE MATOS       : 

{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import CodeWorld.Sketches
type Linha = (Point, Point)
type Poligono = [Point]


main = activityOf navesInicial update visualization

data Nave = Nave {pos, vel :: Point,
                  giro, res :: Int,
                  ang, clock :: Double,
                  acelerando, visibilidade, atirando, estadoExp :: Bool
                  }
                  deriving Show
                  
data Bala = Bala {posB, posNT :: Point,
                  velB :: Vector,
                  vida, angB :: Double}
                  deriving (Show, Eq)
                  
data Asteroide = Asteroide {posA :: Point, 
                            giroA :: Int, 
                            angA :: Double
                            }
                  deriving Show
                  
data MundoNave =
   Naves {
   nave1, nave2 :: Nave,
   ast1, ast2 :: Asteroide,
   localExplosao, localExplosao2 :: Point,
   clockExp1 :: Double,
   balas :: [Bala]
   } 
   deriving Show
                                          

navesInicial = 
  Naves {
  nave1 = nave1Inicial,
  nave2 = nave2Inicial,
  ast1 = ast1In,
  ast2 = ast2In,
  localExplosao = (100, 100),
  localExplosao2 =  (100, 100),
  balas = [],
  clockExp1 = 0
  }
  
  where 
    nave1Inicial = Nave {pos = (-4, 4),
                         vel = (0,0),
                         giro = 0,
                         res = 0,
                         acelerando = False,
                         atirando = False,
                         visibilidade = True,
                         estadoExp = False,
                         ang = 0,
                         clock = 0}
                         
    nave2Inicial = nave1Inicial {pos = (4, -4)}
    
    ast1In = Asteroide {posA = (0,0),
                        giroA = 1,
                        angA = 0}
                        
    ast2In = ast1In {posA = (8, 8), giroA = -1}
                      
                      
                         
  
visualization :: MundoNave -> Picture
visualization naves@Naves {nave1 = n1, 
                           nave2 = n2,
                           ast1  = a1,
                           ast2  = a2,
                           localExplosao = (xp1, yp1),
                           localExplosao2  = (xp2, yp2),
                           balas = bs}
                           
                           
         = p1 & p2 & nave1 & nave2 & expl & expl2 & tiros
         
   where nave1 = constroiNave verticesNave n1
         nave2 = constroiNave verticesNave n2
         p1 =  constroiAsteroide 1 pontosAsteroides a1
         p2 =  constroiAsteroide 2 pontosAsteroides a2
         expl = translated xp1 yp1 sketchedExplosion
         expl2 = translated xp2 yp2 sketchedExplosion
         tiros = pictures $ map criaTiros bs
         
         
         
update :: Event -> MundoNave -> MundoNave
-- Nave 1
update (KeyPress "A")   naves  = naves { nave1 = (nave1 naves) {giro = 1}}
update (KeyRelease "A") naves  = naves { nave1 = (nave1 naves) {giro = 0}}
update (KeyPress "D")   naves  = naves { nave1 = (nave1 naves) {giro = -1}}
update (KeyRelease "D") naves  = naves { nave1 = (nave1 naves) {giro = 0}}
update (KeyPress "S")   naves 
        | atirando $ nave1 naves = naves
        | otherwise = naves { nave1 = (nave1 naves){ clock = 0, atirando = True}}
update (KeyRelease "S") naves  = naves { nave1 = (nave1 naves) {atirando = False}}
update (KeyPress "W")   naves  = naves { nave1 = (nave1 naves) {acelerando = True}}
update (KeyRelease "W") naves  = naves { nave1 = (nave1 naves) {acelerando = False}}

-- Nave 2
update (KeyPress "Left")    naves  = naves { nave2 = (nave2 naves) {giro = 1}}
update (KeyRelease "Left")  naves  = naves { nave2 = (nave2 naves) {giro = 0}}
update (KeyPress "Right")   naves  = naves { nave2 = (nave2 naves) {giro = -1}}
update (KeyRelease "Right") naves  = naves { nave2 = (nave2 naves) {giro = 0}}
update (KeyPress "Down")    naves 
        | atirando $ nave2 naves = naves
        | otherwise = naves { nave2 = (nave2 naves){ clock = 0, atirando = True}}
update (KeyRelease "Down") naves  = naves { nave2 = (nave2 naves) {atirando = False}}
update (KeyPress "Up")     naves  = naves { nave2 = (nave2 naves) {acelerando = True}}
update (KeyRelease "Up")   naves  = naves { nave2 = (nave2 naves) {acelerando = False}}

-- Tempo 

update (TimePassing t)  naves  =  explosoes . resistenciaNaves . destroiBalas t . 
       navesArmas t . disparaBalas t . confereImpactoAsteroide . giraAsteroides t . 
       aceleraNaves t $ naves

update _ naves = naves

--         ******  ACELERACAO INIDIVIDUAL DAS NAVES ****

aceleraNaves t naves@Naves {nave1 = n1, nave2 = n2} 
      = naves {nave1 = aceleraNave t n1,
               nave2 = aceleraNave t n2}

aceleraNave t nave@Nave  {pos = p,
                           giro = g,
                           acelerando = b,
                           vel = v,
                           ang = r} 
        = nave {pos = pN,
                vel = vN,
                ang = rN}
      where 
        (pN, vN) = calculaMRUV p v t r b
        rN = angMCU g r t
   
unitVector :: Double -> (Double, Double)
unitVector ang = (accNave * cos ang, accNave * sin ang)  

calculaMRUV p v t r b = (pN, vN)
 where pN = vectorSum p (vectorSum (scaledVector t v) 
                        (scaledVector (1/2 * t^2) acc))
       vN = vectorSum v (scaledVector t acc)
       acc 
        |b = scaledVector accNave (unitVector r)
        |otherwise = (0,0)

angMCU giro ang t = ang + velAngNave * fromIntegral giro * t
    



                         
--         **********  GIRO DOS ASTEROIDES ***********

giraAsteroides t naves@Naves {ast1 = p1,
                              ast2 = p2}
         = naves{ast1 = giraAsteroide t p1,
                 ast2 = giraAsteroide t p2}

giraAsteroide t ast@Asteroide {
                            posA = (x1, y1),
                            angA = ang,
                            giroA = g}
         = ast {angA = nAng}  
      where nAng = ang + velAngNave * fromIntegral g * t


constroiAsteroide n vertices ast@Asteroide {posA = (x1, y1),
                                          angA = ang }
                                           
        |n == 1 = dilated 1.2 . translated x1 y1 $ (marca & asteroide)
        |otherwise = translated x1 y1 . dilated 0.65 $ (marca & asteroide)
   where marca = thickPolygon 0.24 novosVertices
         asteroide = colored grey (solidPolygon novosVertices)
         novosVertices = mudaAng vertices ang
         
         

-- *********** EXPLOSOES            ********** 

explosoes naves@Naves{
                 nave1 = n1,
                 nave2 = n2, 
                 localExplosao = pExp,
                 localExplosao2 = pExp2} 
     |estadoExp n1 && estadoExp n2 = naves {nave1 = novaN1, nave2 = novaN2, localExplosao2 = pos n2,
                              localExplosao = pos n1}
     |estadoExp n1 = naves {nave1 = novaN1, localExplosao = pos n1}
     |estadoExp n2 = naves {nave2 = novaN2, localExplosao2 = pos n2}
     |otherwise = naves
     where novaN1 = n1 {pos = (1000, -1000), estadoExp = False, res = 0}
           novaN2 = n2 {pos = (-1000, 1000), estadoExp = False, res = 0}     
          
          
          
--        *********** CONFERE IMPACTO **********                                       

pontoPraNave :: Point -> Double -> Poligono
pontoPraNave (x', y') ang = map somaCentro . mudaAng [(2,0),(0,0.75),(0,-0.75)] $ ang 
  where 
  somaCentro (x, y) = (x+x', y+y')
  
  
pontoPraPoligono :: Point -> Double -> Poligono -> Poligono
pontoPraPoligono (x', y') ang poli = map somaCentro . mudaAng poli $ ang
  where somaCentro (x, y) = (x+x', y+y')
  
confereImpactoAsteroide naves@Naves { 
                               nave1 = n1,
                               nave2 = n2,
                               ast1 = p1,
                               ast2 = p2}
       | intersecPolygon nav1 asteroide1 ||
         intersecPolygon nav1 asteroide2 = naves{nave1 = (nave1 naves) {estadoExp = True}}
       | intersecPolygon nav2 asteroide1 ||
         intersecPolygon nav2 asteroide2    = naves{nave2 = (nave2 naves) {estadoExp = True}}
       | intersecPolygon nav1 nav2 = naves{nave1 = (nave1 naves) {estadoExp = True},
                                           nave2 = (nave2 naves) {estadoExp = True} }
       |otherwise = naves
       where 
       nav1 = pontoPraNave (pos n1) (ang n1)
       nav2 = pontoPraNave (pos n2) (ang n2)
       asteroide1  = pontoPraPoligono (0,0) (angA p1)  . map ( dilatedPoint 1.2) $ pontosAsteroides
       asteroide2  = pontoPraPoligono (8, 8) (angA p2) . map (dilatedPoint 0.65) $ pontosAsteroides


             
            
            -- ****** RESISTENCIA NAVES ******
            
checaImpactoTiroNave nave@Nave{pos = p,
                               ang = r,
                               res = ts} bala@Bala{posB = pB, angB = rB} 
     = intersecPolygon (pontoPraNave p r) tiro
     where tiro = pontoPraPoligono pB rB pontosTiro

retirandoBalas nave@Nave{res = ts} naves@Naves{balas = lbs} 
   = (nave {res = ts + nRes}, naves {balas = retiraBalas listaImpactos lbs})
 where
 listaImpactos = filter (checaImpactoTiroNave nave) lbs
 nRes = length listaImpactos
 retiraBalas [] bs = bs
 retiraBalas [x] bs = if x `elem` bs then filter (\b -> b /= x) bs else bs
 retiraBalas (x:xs) bs = retiraBalas xs bs
 
 
atualizaRes naves@Naves{nave1 = n1, nave2 = n2, balas = bs} 
   = naves {nave1 = nRes1, nave2 = nRes2, balas = balas nBS}
  where 
  nRes1 = fst . retirandoBalas n1 $ naves
  (nRes2, nBS) = retirandoBalas n2 . snd . retirandoBalas n1 $ naves
  novaN1 = n1 {estadoExp = True}
  novaN2 = n2 {estadoExp = True}
  
verificaRes naves@Naves {nave1 = n1, nave2 = n2} 
  |res n1 >= 10 = naves {nave1 = (nave1 naves) {estadoExp = True}}
  |res n2 >= 10 = naves {nave2 = (nave2 naves) {estadoExp = True}}
  |otherwise = naves

resistenciaNaves naves = verificaRes . atualizaRes $ naves                     
             
             
             
          
--             ****** PROJETEIS ********

destroiBalas t naves@Naves { balas = bs } =
    naves { balas = [ destroiBala t b| b <- bs, duracaoBala t b ] }
  where
    destroiBala t b@Bala{ vida = v } = b { vida = v - t }
    duracaoBala t b@Bala{ vida = v } = v > t       


disparaBalas t naves@Naves{ast1 = a1, ast2 = a2} = naves { balas = map disparaBala (balas naves) }
  where
     disparaBala b
       |intersecPolygon pedra1 tiroP  = b {velB = vNR1}
       |intersecPolygon pedra2 tiroP = b {velB = vNR2}
       |otherwise =  b { posB = mruPos t (posB b) (velB b),
                         vida = vida b - t} 
         where 
         tiroP = pontoPraPoligono (posB b) (angB b) pontosTiro
         pedra1 = pontoPraPoligono (0,0) (angA a1)  . map ( dilatedPoint 1.2) $ pontosAsteroides
         pedra2 = pontoPraPoligono (8, 8) (angA a2) . map (dilatedPoint 0.65) $ pontosAsteroides
         vNR1 = calculaReflexTiro tiroP pedra1 (velB b) (posNT b)
         vNR2 = calculaReflexTiro tiroP pedra2 (velB b) (posNT b)



navesArmas t naves@Naves { nave1 = n1,
                           nave2 = n2,
                           balas = bs } =
         naves { nave1 = confereCadencia t n1,
                 nave2 = confereCadencia t n2,
                 balas = ifAtira t n1 . 
                         ifAtira t n2 $ bs}

confereCadencia t nave@Nave { clock = tc, atirando = True } 
   | t + tc >= cadencia =  nave { clock = t + tc - cadencia } 
   | otherwise          =  nave { clock = t + tc }

confereCadencia t nave = nave 
    
ifAtira t nave@Nave { clock = tc, atirando = atr} naves 
   |  atr && (tc == 0 || t + tc >= cadencia) = disparar nave : naves
   | otherwise                               = naves
   
disparar nave@Nave { pos  = (x, y),
                     ang  = ag,
                     vel  = v,
                     giro = gir} =
   Bala { posB = (px + x, py + y),
          velB = vectorSum v (vectorSum (velTang gir ag) (cadArma ag)),
          angB = ag,
          vida = vidaBala,
          posNT = (x, y)}
          where 
          (px, py) = head $ mudaAng [(2.75, 0), (0, 0.75), (0,-0.75)] ag
          
 
localArma p ag = vectorSum p (scaledVector deslocaCentroide (unitVectorAll ag))
                        
velTang gir ag
   | gir == 0 = (0, 0)
   | otherwise = scaledVector magTangVel (unitVectorAll (ag + (fromIntegral gir) * pi/2))
               
cadArma ag = scaledVector magBala (unitVectorAll ag)                   

deslocaCentroide = 2 - x + 0.05
   where 
     (x, _) = centroide verticesNave    


-- Constantes
velSaidaArma = 0.5
worldLimit = 13.5
pontosAsteroides = [(0,2), (1,1.8), (1.5,1), (2,0), (1.5,-2), (1, -2),
                    (0, -3), (-1, -2), (-1.5, -0.5), (-1, 1)]
verticesNave = [(0,0.7),(0,-0.7),(2, 0)]
velAngNave = pi/3
accNave = 1.3
durExplosao = 3
disparos = 6
cadencia = 1/disparos
vidaBala = 4
magTangVel = velAngNave * deslocaCentroide
magBala = 5


mruPos :: Double -> Point -> Vector ->  Point 
mruPos t p v = mruvPos p v (0, 0) t

mruvPos :: Point -> Vector ->  Vector -> Double -> Point    
mruvPos p v acc t = vectorSum p  (vectorSum (scaledVector t v) 
                                 (scaledVector (1/2 * t^2) acc))

unitVectorAll :: Double -> Vector
unitVectorAll a = (cos a, sin a)

-- Outros

constroiNave vertices nave@Nave {pos = (x1, y1),
                                 ang = a}
                                 = translated x1 y1 $ solidPolygon novosVertices
    where novosVertices = mudaAng vertices a
     
     
mudaAng :: Poligono -> Double -> Poligono
mudaAng polig ang = poligonoRotacionado
        where (c1, c2) = centroide polig 
              trazOrigem = [(x-c1, y-c2) | (x, y) <- polig]

              matricial a b = (cos ang * a + (- sin ang * b), sin ang * a + cos ang * b) 
              listaMatricial = [matricial x1 y1 | (x1, y1) <- trazOrigem]
              poligonoRotacionado  = [(c1+xR, c2 + yR) | (xR, yR) <- listaMatricial]


pontosTiro = [(0.15, 0), (0, 0.15), (-0.15,0), (0, -0.15)]

tiro :: Picture
tiro = solidPolygon pontosTiro

criaTiros b@Bala {posB = (xB, yB), angB = ag} = translated xB yB (rotated ag tiro)


calculaReflexTiro :: Poligono -> Poligono -> Vector -> Point -> Vector
calculaReflexTiro tiroPoli poligonoImpacto vTiro pontoNave = reflection pontoNave vTiro linhaDeImpacto
  where 
  linhaDeImpacto = head [y | x<-formaLinhas tiroPoli, y<-formaLinhas poligonoImpacto, intersecta1 x y]
  pontoImpac =  head [pontoIntersec x y | x<-formaLinhas tiroPoli, y<-formaLinhas poligonoImpacto, intersecta1 x y]













-- Questão 1
onSegment :: Point -> Point -> Point -> Bool
onSegment p q r = (fst q <= max (fst p) (fst r)) && (fst q >= min (fst p) (fst r)) &&
                    (snd q <= max (snd p) (snd r)) && (snd q >= min (snd p) (snd r))
orientacao :: Point -> Point -> Point -> Int
orientacao p q r 
  |val > 0 = 1
  |val < 0 = 2
  |otherwise = 0
   where val = ((snd q - snd p ) * (fst r - fst q)) - ((fst q - fst p) * (snd r - snd q))

intersecta1 :: Linha -> Linha -> Bool
intersecta1 (p1, q1) (p2, q2)
  |o1 /= o2 && o3 /= o4 = True
  |o1 == 0 && onSegment p1 p2 q1 = True
  |o2 == 0 && onSegment p1 q2 q1 = True
  |o3 == 0 && onSegment p2 p1 q2 = True
  |o4 == 0 && onSegment p2 q1 q2 = True
  |otherwise = False
   where 
    o1 = orientacao p1 q1 p2
    o2 = orientacao p1 q1 q2
    o3 = orientacao p2 q2 p1
    o4 = orientacao p2 q2 q1
     
--Questão 2     
intersecLinhaPolygon :: Linha -> Poligono -> Bool 
intersecLinhaPolygon l1 xs = or [intersecta1 l1 x | x<-formaLinhas xs]
              {-where formaLinhas = [(xs !! n, xs !! (n+auxiliar xs n)) | n<-[0..length xs-1]]
                    auxiliar lista v
                      |v < last [0..length lista-1] = 1
                      |otherwise = - last [0..length lista-1]-}
                      
                      
formaLinhas :: Poligono -> [Linha]                     
formaLinhas xs = [((xs !! n, xs !! (n+auxiliar xs n))) | n<-[0..length xs-1]]
              where  auxiliar lista v
                      |v < last [0..length lista-1] = 1
                      |otherwise = - last [0..length lista-1]
--Questão 3
intersecPolygon :: Poligono -> Poligono -> Bool
intersecPolygon poli1 poli2 = or [intersecta1 x y 
                              | x<-formaLinhas poli1, y<-formaLinhas poli2]
                      
--Questão 4 Não há necessidade de passar o último ponto igual ao primeiro
-- no argumento da função. Apenas os vértices originais.
areaPolygon :: Poligono -> Double
areaPolygon xs = a
   where a = 0.5 * (sum [ (x*yi)-(xi*y) | n<-[0..length poli-2],
                          let x  = fst (poli !! n) 
                              y  = snd (poli !! n)
                              yi = snd (poli !! (n+1))
                              xi = fst (poli !! (n+1))
                      ])
         poli = xs ++ [head xs]
                      
--Questão 5 Não há necessidade de passar o último ponto igual ao primeiro
-- no argumento da função. Apenas os vértices originais.
centroide :: Poligono -> Point
centroide xs = (cx, cy)

             where area = 1/(6*areaPolygon xs)
                   indice = [0..length poli-2]
                   poli = xs ++ [head xs]
                   
                   cx = area * somatoria1
                   somatoria1 = sum [(x + xi)*((x*yi)-(xi*y)) | n<-indice,
                          let x = fst (poli !! n)
                              y = snd (poli !! n)
                              yi = snd (poli !! (n+1))
                              xi = fst (poli !! (n+1))]

                   cy = area * somatoria2
                   somatoria2 = sum [(y + yi)*((x*yi)-(xi*y)) | n<-indice,
                          let x = fst (poli !! n)
                              y = snd (poli !! n)
                              yi = snd (poli !! (n+1))
                              xi = fst (poli !! (n+1))]
                   
                      
                      
 -- Questão 6: Nessa questão, o vetor reflexão parte da origem. 
 -- Para partir do ponto de intersecção do vetor com o segmento, é só somar 
 -- (rx + x, rx + y)
reflection :: Point -> Vector -> Linha -> Vector
reflection (p1, p2) (i, j) ((a, b), (c, d)) 
   |intersecta1 linhaVetor ((a, b), (c, d)) = (rx, ry) -- ou (rx+x, rx+y)
   |otherwise                          = (i, j)
      where (rx, ry)      = vectorDifference (-x, -y) ((scaledVector (2*dp) normal))
            (x, y)        = pontoIntersec linhaVetor ((a, b), (c, d))
            parameter     =  (a-c, b-d)
            normal        =  (fst parameter/vectorLength parameter, snd parameter/vectorLength parameter)
            dp            =   dotProduct (-x, -y) normal
            linhaVetor        = ((p1, p2),(i,j)) 
 
--Questão 7 
verificaCasoEspecial :: (Point, Point, Point) -> Bool
verificaCasoEspecial (a, b, c) =
    (fst b <= max (fst a) (fst c) && fst b >= min (fst a) (fst c) &&
        snd b <= max (snd a) (snd c) && snd b >= min (snd a) (snd c))

pontoIntersec :: Linha -> Linha -> Point
pontoIntersec (p1, p2) (p3, p4)
  |denomina == 0 && verificaCasoEspecial (p1, p3, p2) = p3
  |denomina == 0 && verificaCasoEspecial (p1, p4, p2) = p4
  |denomina == 0 && verificaCasoEspecial (p3, p1, p4) = p1
  |denomina == 0 && verificaCasoEspecial (p3, p2, p4) = p2
  |intersecta1 (p1, p2) (p3, p4) = (fst p1 + t*(fst p2 - fst p1), snd p1 + t*(snd p2 - snd p1)) 
  |otherwise = error "Não se intersectam"
    where 
     t  = ((fst p1 - fst p3)*(snd p3 - snd p4) - (snd p1 - snd p3)*(fst p3 - fst p4)) / denomina
     denomina = (fst p1 - fst p2)*(snd p3 - snd p4) - (snd p1 - snd p2)*(fst p3 - fst p4) 
