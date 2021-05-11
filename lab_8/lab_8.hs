{-
    tipuri de evaluari:
        - aplicativa (stricta)
        - normala (non-stricta)
    
    function f (int x, int y) {
        for (int i = 0; ...) {

        }
        for (...)

        return x + y;
    }

    function g (int x) {
        return x + 1;
    }

    function h (int x) {
        return 2 * x;
    }

    int main () {
        printf(f(g(2), h(5)));
    }
-}

naturals x = x : naturals (x + 1)
nat = naturals 0

nat2 :: [Integer]
nat2 = 0 : map (+1) nat2

-- 0: (map (+1) [0]) => 0:[1] =? [0, 1]
-- 0: (map (+1) [0, 1]) => 0:[1, 2] => [0, 1, 2]

-- 8.1.2
odds1 :: [Integer]
odds1 = filter (\x -> x `mod` 2 == 1) nat

odds2 :: [Integer]
odds2 = map (\x->x * 2 + 1) nat

odds3 :: [Integer]
odds3 = 1 : map (+2) odds3

odds4 :: [Integer]
odds4 = map (+1) (zipWith (+) nat nat)

-- 8.1.3
-- 0, 1, 1, 2, 3, 5, 8

fibo x y = (x + y) : fibo y (x + y)

-- (1 + 2)
    -- 3 : (fibo 2 3)
    -- 3 : (2 + 3) : (fibo 3 5)
    -- 3 : 5 : (3 + 5) : fibo 

fib :: [Integer]
fib = 0:1:fibo 0 1

-- 8.2.1
-- [a0, (g a0), g (g a0), ...]
build :: (a -> a) -> a -> [a]
build g a0 = a0 : map g (build g a0)

-- a0 : [(g a0)] => [a0, (g a0)]
-- a0 : [(g a0), ((g g a0))] => [a0, g a0 , g(g a0)]

nat3 = build (+1) 0

odds5 = build (+2) 1

select :: Float -> [Float] -> Float
select e [x] = -1
select e [] = -1
select e (x:xs)
    | abs (x - head xs) < e = x
    | otherwise = select e xs

-- 8.2.3
fiboLim = zipWith (/) (map fromIntegral (tail fib)) (map fromIntegral fib)

phi :: Float 
phi = select 0.001 fiboLim

-- 8.2.4
a0 = 1
an x = x:an (x + sin x)

myPi :: Float
myPi = select 0.001 (an a0)

anSquare :: Float -> Float -> [Float]
anSquare x k = x:anSquare (1/2 * (x + k / x)) k

squareRoot :: Float -> Float 
squareRoot k = select 0.001 (anSquare a0 k)

f x = x ** 3 + sin x - 1

f' x = 3 * x ** 2 + cos x

xn f1 f2 x = x:xn f1 f2 (x - f1 x / f2 x)

sol f f' = select 0.001 (xn f f' 0)

-- 8.3.6
h0 = 10
hn = build (/2) h0

f'a a (h:hs) = ((f (a + h) - f a) / h) : f'a a hs

solution = select 0.001 (f'a 2 hn)

-- 8.3.8
trapezoid :: (Float -> Float) -> Float -> Float -> Float 
trapezoid f a b = (b - a) * (f a + f b) / 2

insertInList :: [Float] -> [Float]
insertInList [] = []
insertInList [x] = [x]
insertInList (x:xs) = x : (x + head xs) / 2 : insertInList xs

trapezoidsList :: (Float -> Float) -> [Float] -> [Float]
trapezoidsList _ [x] = []
trapezoidsList _ [] = []
trapezoidsList f (p:ps) = trapezoid f p (head ps) : trapezoidsList f ps

pointsList a b = build insertInList [a, b]
integrate f a b = map (sum.trapezoidsList f) (pointsList a b)
solutionIntegral f a b = select 0.001 (integrate f a b)