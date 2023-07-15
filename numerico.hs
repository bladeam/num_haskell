module Numerico where 
-- Este un cometario de linea
{-
  Este es un comentario multilínea
-}
-- mostrar una lista de tuplas en columnas
-- se debe mejorar, vi que si se puede imprimir en un archivo, reconoce a este '\n'
mostrar :: (Show a, Show b) => [(a, b)] -> String
mostrar ((x,y):t) = show (x,y) ++ "\n" ++ mostrar t
mostrar [] = ""

-- método de bisección
-- creo que falta mejorar
ff x = x*x-4
biseccion a b e | ((b-a)/2) < e = (a+b)/2
                | ((ff b)*(ff ((a+b)/2)))>0 = biseccion a ((a+b)/2) e
                | ((ff a)*(ff ((a+b)/2)))>0 = biseccion ((a+b)/2) b e 
                | otherwise = biseccion ((a+b)/2) b e 
-- método de Newton para ecuaciones mio
f x = x*x-4
df x = 2*x
newton x n = if n==0 then x 
             else 
                newton (x-(f x)/(df x)) (n-1)

-- método de Newton para ecuaciones
newton2 x 0 = [x]
newton2 x n = [x] ++ newton2 (x-(f x)/(df x)) (n-1)
-- método de Newton para ecuaciones, con guardas      
newton3 x n
      | n == 0 = [x]
      | otherwise = [x] ++ newton3 (x-(f x)/(df x)) (n-1)
-----------------------------------------------------------------------
-- Método de punto fijo par una ecuación
-----------------------------------------------------------------------
-- este metodo depende de la funcion, es decir de cual es la variable a despejar
g x = x+x*x
punto_fijo x 0 = [x]
punto_fijo x n = [x] ++ punto_fijo (g x) (n-1)
-----------------------------------------------------------------------
-- Método de la Falsa posición para resolución de ecuación
-----------------------------------------------------------------------
-- ejecutar con : falsa_posicion 0 1 0.01 40000 
f3 x = (exp (3*x)) - 4
falsa_posicion a b tol max = falsa_p a b tol max 0 []
falsa_p a b tol 0 x l = l
falsa_p a b tol n x l = falsa_p aa bb tol nn x1 (l++[(x1,e)])
                         where x1 = (a*(f3 b)-b*(f3 a))/((f3 b) -(f3 a))
                               aa = if (f3 x1)*(f3 b)<0 then x1 else a
                               bb = if (f3 x1)*(f3 a)<0 then x1 else b
                               nn = if (abs (x1-x))<tol then 0 else (n-1)
                               e = 100*(abs (x1-x))
                                    

-----------------------------------------------------------------------
-- 2x2 Método de Newton-Raphson para sistemas de ecuaciones no lineales 2x2
-----------------------------------------------------------------------
fr x y = x*x*x-y*y*y-19
gr x y = x*x*y-x*y*y-6
-- primeras derivadas
dfr x y = (3*x*x,-3*y*y)
dgr x y = (2*x*y-y*y,x*x-2*x*y)
-- la función principal
newton_raphson2x2 a b max = newton_r2x2 a b max []
newton_r2x2 a b 0 r = r
newton_r2x2 a b n r = newton_r2x2 x y (n-1) (r++[(x,y)])
                      where aa = (fst (dfr a b))
                            bb = (snd (dfr a b))
                            cc = (fst (dgr a b))
                            dd = (snd (dgr a b))
                            x = a - (dd/(aa*dd-bb*cc))*(fr a b) - ((-1)*bb/(aa*dd-bb*cc))*(gr a b)
                            y = b - ((-1)*cc/(aa*dd-bb*cc))*(fr a b) - (aa/(aa*dd-bb*cc))*(gr a b)
-----------------------------------------------------------------------
-- 3x3 Método de Newton-Raphson para sistemas de ecuaciones no lineales 3x3
-----------------------------------------------------------------------
fr3 x y z = x+y+z-9
gr3 x y z = x*x+y*y+z*z-29
hr3 x y z = x*y+y*z-x*z-2
-- primeras derivadas
dfr3 x y z = (1,1,1)
dgr3 x y z = (2*x,2*y,2*z)
dhr3 x y z = (y-z,x+z,y-x)
-- extras
fst3 (a,_,_) = a
snd3 (_,b,_) = b
thr3 (_,_,c) = c
-- la función principal
newton_raphson3x3 a b c max = newton_r3x3 a b c max []
newton_r3x3 a b c 0 r = r
newton_r3x3 a b c n r = newton_r3x3 x y z (n-1) (r++[(x,y,z)])
                      where a1 = (fst3 (dfr3 a b c))
                            b1 = (snd3 (dfr3 a b c))
                            c1 = (thr3 (dfr3 a b c))
                            (a2,b2,c2) = (fst3 (dgr3 a b c),snd3 (dgr3 a b c),thr3 (dgr3 a b c))
                            --b2 = (snd3 (dgr3 a b c))
                            --c2 = (thr3 (dgr3 a b c))
                            a3 = (fst3 (dhr3 a b c))
                            b3 = (snd3 (dhr3 a b c))
                            c3 = (thr3 (dhr3 a b c))
                            det = a1*b2*c3-a1*b3*c2-a2*b1*c3+a2*b3*c1+a3*b1*c2-a3*b2*c1
                            x = a - ((b2*c3-b3*c2)/det)*(fr3 a b c) - ((b3*c1-b1*c3)/det)*(gr3 a b c) - ((b1*c2-b2*c1)/det)*(hr3 a b c)
                            y = b - ((a3*c2-a2*c3)/det)*(fr3 a b c) - ((a1*c3-a3*c1)/det)*(gr3 a b c) - ((a2*c1-a1*c2)/det)*(hr3 a b c)
                            z = c - ((a2*b3-a3*b2)/det)*(fr3 a b c) - ((a3*b1-a1*b3)/det)*(gr3 a b c) - ((a1*b2-a2*b1)/det)*(hr3 a b c)
-----------------------------------------------------------------------
-- 2x2 Método de Punto-Fijo para sistemas de ecuaciones no lineales 2x2
-----------------------------------------------------------------------
-- ejecutar con: punto_fijo_2x2 1.5 3.5 7
--funx x y = sqrt (10-x*y)
--funy x y = sqrt ((57-y)/(3*x))
-- la función principal
punto_fijo_2x2 a b max = punto_fijo_2x2_aux a b max []
punto_fijo_2x2_aux a b 0 r = r 
punto_fijo_2x2_aux a b n r = punto_fijo_2x2_aux x y (n-1) (r++[(x,y)])
                             where x = funx a b
                                   y = funy x b
                                   -- mejor aqui podemos colocar las funciones
                                   funx x y = sqrt (10-x*y)
                                   funy x y = sqrt ((57-y)/(3*x))
-----------------------------------------------------------------------
-- 3x3 Método de Jacobi, para sistemas lineales 3x3
-----------------------------------------------------------------------
-- ejecutar con: jacobi 0 0 0 10
--x y z = (2*y-3*z+5)/8
--y x z = (3*x+5*z-2)/9
--z x y = (2*x-3*y-3)/12
jacobi a b c 0 = [(a,b,c)]
jacobi a b c n = [(a,b,c)] ++ jacobi (x b c) (y a c) (z a b) (n-1)
      where x y z = (2*y-3*z+5)/8
            y x z = (3*x+5*z-2)/9
            z x y = (2*x-3*y-3)/12
-----------------------------------------------------------------------
-- 3x3 Método de Gauss-Seidel, para sistemas lineales 3x3
-----------------------------------------------------------------------
-- ejecutar con: gauss_seidel 0 0 0 10
x2 y z = (2*y-3*z+5)/8
y2 x z = (3*x+5*z-2)/9
z2 x y = (2*x-3*y-3)/12
gauss_seidel a b c 0 = [(a,b,c)]
gauss_seidel a b c n = [(a,b,c)] ++ gauss_seidel (x2 b c) (y2 a1 c) (z2 a1 b1) (n-1)
                       where a1 = x2 b c
                             b1 = y2 a1 c
-----------------------------------------------------------------------
-- Mínimos cuadrados
-----------------------------------------------------------------------
-- ejecutar con: minimos_cuadrados [7,1,10,5,4,3,13,10,2] [2,9,2,5,7,11,2,5,14]
----extra, por problemas con tipos
--len [] = 0
--len (_:xs) = 1 + len xs
-- la función principal
minimos_cuadrados l1 l2 = min_cu l1 l2 (length l1) (fromIntegral (length l1)) 0 0 0 0                              
min_cu l1 l2 0 nn sx sy sxy sx2 = (m,b)
                                  where m = (sxy - sx * sy / nn) / (sx2 - sx * sx / nn)
                                        b = (sy - m * sx) * (1/nn)
min_cu (x:xs) (y:ys) n nn sx sy sxy sx2 = min_cu xs ys (n-1) nn (sx+x) (sy+y) (sxy+x*y) (sx2+x*x)
-----------------------------------------------------------------------
-- Mínimos cuadrados, mejor opción
-----------------------------------------------------------------------
-- Hacer correr con: min_cua [1,2,7] [3,4,8]
min_cua l1 l2 =        
                 let sx = sum l1; sy = sum l2
                     n = fromIntegral (length l1)-- no funciona con length l1
                                                 -- ya corregi ese error
                                                 -- fromIntegral convierte a uno mas general
                     xy = sum ((\(x,y) -> x*y) <$> (zip l1 l2))--sum [x*y|(x,y)<-zip l1 l2]
                     x2 = sum ((\ x -> x*x) <$> l1)--sum [x*x|x<-l1]
                     m = (sx*sy-n*xy)/(sx*sx-n*x2)
                     b = (sy-m*sx)/n
                 in (m,b) 
-----------------------------------------------------------------------
-- Interpolación de Newton
-----------------------------------------------------------------------
dif [x] = []
dif (x:y:xs) = [y-x] ++ dif (y:xs)
newton_dif l1 l2 = newt l2 []
newt [] r = init r
newt t@(x:xs) r = newt d (r ++ [d])
                  where d = dif t
mapear [] l2 = []
mapear (x:xs) (y:ys) = [lista] ++ mapear xs ys
                            where lista = producto7 x y
producto7 [] l2 = []
producto7 (x:xs) (y:ys) = [x*y]++producto7 xs ys
---------------------
primero l1 = segundo l1 ((length l1)-1) [1..((length l1))]
segundo l1 0 i = []
segundo l1 n (i:is) = [a] ++ segundo l1 (n-1) is
                     where a = [(x,x)|(x,y)<-(magia l1 [1..(length l1)]), y `mod` i == 0]


-- primero [5,4,3,2] 
--[(5,4)(4,3)(3,2)],[(5,3)(4,2)],[(5,2)]
----------------------------------------------fin interpolacion de Newton

-----------------------------------------------------------------------
-- Interpolacion de Lagrange
-----------------------------------------------------------------------
-- ejecutar con :
-- Multiplicar_polinomios 
-- forma un lista de tuplas (coef,grado) del producto de dos polinomios
multi [] _ = []
multi t@(x:xs) l2 = aux ++ multi xs l2
                  where aux = zip absc grado
                        absc = [x*y|y<-l2]
                        grado = reverse [0..(l-1)]
                        l = (length t) + (length l2) - 1
-- suma de terminos (coef,grado), elimina los terminos semejantes ya sumados
sumar [] = []
sumar todo@((_,y):_) = [s] ++ sumar resto
                   where s = sum [a |(a,b)<-todo,b == y]
                         resto = [(a,b)|(a,b)<-todo,b /= y]                         
-- multiplica polinomios
multiplicar_polinomios l1 l2 = sumar (multi l1 l2)
-- solo numerador de un termino, para interpolacion de lagramge
multip [] y = [1]
multip t@(x:xs) y | x==y = multiplicar_polinomios [1] (multip xs2 y)
                  | otherwise = multiplicar_polinomios [1,(-1)*x] (multip xs2 y)
                  where xs2 = [z |z<-xs,y/=z]
-- función principal
lagrange l1 l2= lagrange1 l1 l1 (den0 l1) l2
lagrange1 l1 [y] (z:zs) (m:ms) = map (*(m/z)) (multip l1 y)   
lagrange1 l1 (y:ys) (z:zs) (m:ms)= sm (lagrange1 l1 ys zs ms) (map (*(m/z)) (multip l1 y))   
-- sumar listas del mismo tamaño, 1ero con 1ero , ...       
sm [] _ = []
sm (x:xs) (y:ys) = [x+y] ++ sm xs ys
-- solo los denominadres en una lista, para interpolacion de lagramge
den0 l1 = den l1 l1
den [] l1 = []
den (x:xs) l1 = [product [(x-z)|z<-l1,x /= z]] ++ den xs l1
-----------------------------------------------------------------------
-- Derivación: Solo la centrada de la primera derivada
-----------------------------------------------------------------------
fder x = x*x
derivacion h x = (8*(fder (x+h))-(fder (x+2*h))-8*(fder (x-h))+(fder (x-2*h)))/(12*h)
-----------------------------------------------------------------------
-- Integración por Rectángulos
-----------------------------------------------------------------------
-- ejecutar con: rectangulos 0 1 50
finte1 x = exp (x*x)
-- la función principal
rectangulos a b n = rectangulos_aux a b n ((b-a)/n) a 0
rectangulos_aux a b 0 h ax suma = suma*h
rectangulos_aux a b n h ax suma = rectangulos_aux a b (n-1) h (ax+h) (suma+(finte1 ((2*ax+h)/2)))
-----------------------------------------------------------------------
-- Integración por Trapecios
-----------------------------------------------------------------------
-- ejecutar con : trapecios (-1) 1 5
finte2 x = exp (x*x*x*x)
-- la función principal
trapecios a b n = trapecios_aux a b (n-1) h (a+h) (finte2 a)
                  where h = (b-a)/n
trapecios_aux a b 0 h ax suma = (suma+(finte2 b))*h/2
trapecios_aux a b n h ax suma = trapecios_aux a b (n-1) h (ax+h) (suma+2*(finte2 ax))
-----------------------------------------------------------------------
-- Integración por Simpson
-----------------------------------------------------------------------
-- ejecutar con : simpson 0 1 4
finte3 x = 3*x*x*(4-2*x)*(cos (1+x*x*x)) - (exp ((4-2*x)*(4-2*x))) - 1
-- la función principal
simpson a b n = simpson_aux a b (n-1) h (a+h) (finte3 a) [2..]
                where h = (b-a)/n
simpson_aux _ b 0 h _ suma _ = (suma+(finte3 b))*h/3
simpson_aux a b n h ax suma (m:ms) = simpson_aux a b (n-1) h (ax+h) suma2 ms
                              where suma2 = if ((m `mod` 2) == 0) then (suma+4*(finte3 ax)) else (suma+2*(finte3 ax))
-----------------------------------------------------------------------
-- Runge-Kutta para ED de primer orden
-----------------------------------------------------------------------
-- Resolver: y'=e^t    y(0)=1    Hallar y(1)
-- Ejecutar con: runge_ed1 5 0 1 1
g1 t x = exp t
runge_ed1 n t x a = runge_ed1_aux n t x ((a-t)/n)
runge_ed1_aux 0 t x h = x
runge_ed1_aux n t x h = runge_ed1_aux (n-1) (t+h) (x+(1/6)*(k11+2*k21+2*k31+k41)) h
                     where k11 = h*(g1 t x)
                           k21 = h*(g1 (t+h/2) (x+k11/2))
                           k31 = h*(g1 (t+h/2) (x+k21/2))
                           k41 = h*(g1 (t+h) (x+k31))
                           
-----------------------------------------------------------------------
--Runge-Kutta para ED de segundo orden
-----------------------------------------------------------------------
-- Resolver: y''+2y'+y=e^t    y(0)=1   y'(0)=0   Hallar y(1)
-- Ejecutar con: runge_ed2 5 0 0 1 1
h1 t x y = (exp t) - y - 2*x
h2 t x y = x
runge_ed2 n t x y a = runge_ed2_aux n t x y ((a-t)/n)
runge_ed2_aux 0 t x y h = [(x,y)]
runge_ed2_aux n t x y h = [(x,y)] ++ runge_ed2_aux (n-1) (t+h) (x+(1/6)*(k11+2*k21+2*k31+k41)) (y+(1/6)*(k12+2*k22+2*k32+k42)) h
                     where k11 = h*(h1 t x y)
                           k12 = h*(h2 t x y)
                           k21 = h*(h1 (t+h/2) (x+k11/2) (y+k12/2))
                           k22 = h*(h2 (t+h/2) (x+k11/2) (y+k12/2))
                           k31 = h*(h1 (t+h/2) (x+k21/2) (y+k22/2))
                           k32 = h*(h2 (t+h/2) (x+k21/2) (y+k22/2))
                           k41 = h*(h1 (t+h) (x+k31) (y+k32))
                           k42 = h*(h2 (t+h) (x+k31) (y+k32))
                           
-----------------------------------------------------------------------
-- Runge-Kutta para ED de tercer orden
-----------------------------------------------------------------------
-- Resolver: y'''+ 4y''+ y'-6y=e^t    y(0)=1   y'(0)=0   y''(0)=2    Hallar y(1)
-- ejecutar con: runge_ed3 50 0 0 1 2 1
p1 t x y z = z
p2 t x y z = x
p3 t x y z = (exp t) + 6*y - x - 4*z
runge_ed3 n t x y z a = runge_ed3_aux n t x y z ((a-t)/n)
runge_ed3_aux 0 t x y z h = [(x,y,z)]
runge_ed3_aux n t x y z h = [(x,y,z)] ++ runge_ed3_aux (n-1) (t+h) (x+(1/6)*(k11+2*k21+2*k31+k41)) (y+(1/6)*(k12+2*k22+2*k32+k42)) (z+(1/6)*(k13+2*k23+2*k33+k43)) h
                     where k11 = h*(p1 t x y z)
                           k12 = h*(p2 t x y z)
                           k13 = h*(p3 t x y z)
                           k21 = h*(p1 (t+h/2) (x+k11/2) (y+k12/2) (z+k13/2))
                           k22 = h*(p2 (t+h/2) (x+k11/2) (y+k12/2) (z+k13/2))
                           k23 = h*(p3 (t+h/2) (x+k11/2) (y+k12/2) (z+k13/2))
                           k31 = h*(p1 (t+h/2) (x+k21/2) (y+k22/2) (z+k23/2))
                           k32 = h*(p2 (t+h/2) (x+k21/2) (y+k22/2) (z+k23/2))
                           k33 = h*(p3 (t+h/2) (x+k21/2) (y+k22/2) (z+k23/2))
                           k41 = h*(p1 (t+h) (x+k31) (y+k32) (z+k33))
                           k42 = h*(p2 (t+h) (x+k31) (y+k32) (z+k33))
                           k43 = h*(p3 (t+h) (x+k31) (y+k32) (z+k33))
                           
-----------------------------------------------------------------------
--Runge-Kutta para sistemas de ED 2x2
-----------------------------------------------------------------------
-- Resolver: x'= 3*x+2*y+2*t
--           y'= x+4*y-7
-- Con el valor inicial: x(0)=0    y(0)=1 
-- ejecutar con: kutta_2x2 5 0 0 1 1
f1 t x y = 3*x+2*y+2*t
f2 t x y = x+4*y-7
kutta_2x2 n t x y a = kutta_2x2_aux n t x y ((a-t)/n)
kutta_2x2_aux 0 t x y h = [(x,y)]
kutta_2x2_aux n t x y h= [(x,y)] ++ kutta_2x2_aux (n-1) (t+h) (x+(1/6)*(k11+2*k21+2*k31+k41)) (y+(1/6)*(k12+2*k22+2*k32+k42)) h
                     where k11 = h*(f1 t x y)
                           k12 = h*(f2 t x y)
                           k21 = h*(f1 (t+h/2) (x+k11/2) (y+k12/2))
                           k22 = h*(f2 (t+h/2) (x+k11/2) (y+k12/2))
                           k31 = h*(f1 (t+h/2) (x+k21/2) (y+k22/2))
                           k32 = h*(f2 (t+h/2) (x+k21/2) (y+k22/2))
                           k41 = h*(f1 (t+h) (x+k31) (y+k32))
                           k42 = h*(f2 (t+h) (x+k31) (y+k32))
                           
-----------------------------------------------------------------------
--Runge-Kutta para sistemas de ED 3x3
-----------------------------------------------------------------------
-- Resolver: x'= z+y-x
--           y'= z+x-y
--           z'= x+y+z
-- Con el valor inicial: x(0)=1    y(0)=0     z(0)=0
-- ejecutar con: kutta_3x3 10 0 1 0 0 1
q1 t x y z = z+y-x
q2 t x y z = z+x-y
q3 t x y z = x+y+z
kutta_3x3 n t x y z a = kutta_3x3_aux n t x y z ((a-t)/n)
kutta_3x3_aux 0 t x y z h = [(x,y,z)]
kutta_3x3_aux n t x y z h = [(x,y,z)] ++ kutta_3x3_aux (n-1) (t+h) (x+(1/6)*(k11+2*k21+2*k31+k41)) (y+(1/6)*(k12+2*k22+2*k32+k42)) (z+(1/6)*(k13+2*k23+2*k33+k43)) h
                     where k11 = h*(q1 t x y z)
                           k12 = h*(q2 t x y z)
                           k13 = h*(q3 t x y z)
                           k21 = h*(q1 (t+h/2) (x+k11/2) (y+k12/2) (z+k13/2))
                           k22 = h*(q2 (t+h/2) (x+k11/2) (y+k12/2) (z+k13/2))
                           k23 = h*(q3 (t+h/2) (x+k11/2) (y+k12/2) (z+k13/2))
                           k31 = h*(q1 (t+h/2) (x+k21/2) (y+k22/2) (z+k23/2))
                           k32 = h*(q2 (t+h/2) (x+k21/2) (y+k22/2) (z+k23/2))
                           k33 = h*(q3 (t+h/2) (x+k21/2) (y+k22/2) (z+k23/2))
                           k41 = h*(q1 (t+h) (x+k31) (y+k32) (z+k33))
                           k42 = h*(q2 (t+h) (x+k31) (y+k32) (z+k33))
                           k43 = h*(q3 (t+h) (x+k31) (y+k32) (z+k33))
                           