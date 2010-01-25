{-# LANGUAGE BangPatterns#-}
{-# OPTIONS -fexcess-precision -funbox-strict-fields -feager-blackholing -O2#-}
module Main(main) where
import Graphics.UI.GLUT
import Data.Time.Clock.POSIX(getPOSIXTime)
import Data.IORef
import Data.Complex hiding (magnitude)
import Control.Monad
import Control.Parallel.Strategies
import System.IO.Unsafe(unsafePerformIO)
import System.IO(hFlush,stdout)

import Templates

{-# NOINLINE func#-}
func = unsafePerformIO $ newIORef mandel
{-# NOINLINE finc#-}
finc = unsafePerformIO $ newIORef 100
{-# NOINLINE fiva#-}
fiva = unsafePerformIO $ newIORef 2
{-# NOINLINE xyold#-}
xyold = unsafePerformIO $ newIORef (-2,-2,4)
{-# NOINLINE xynew#-}
xynew = unsafePerformIO $ newIORef (0,0)
{-# NOINLINE xydrt#-}
xydrt = unsafePerformIO $ newIORef False

main = do
	getArgsAndInitialize
	initialWindowSize $= Size 600 600
	createWindow ""
	displayCallback $= displayMap
	keyboardMouseCallback $= Just displayZoom
	reshapeCallback $= Just reshaper
	mouseWheelCallback $= Just detailZoom
	attachMenu RightButton $ Menu [
		MenuEntry "Reset" $ xyold$=(-2,-2,4),
		MenuEntry "Julia" $ do
			re <- getPrompt "Real coordinate"
			im <- getPrompt "Imag coordinate"
			putStrLn $ re++" "++im
			meop (julia $ readDoub re:+readDoub im) 100,
		SubMenu "Fantou" $ Menu [
			MenuEntry "Mandelbrot" $ meop mandel 100,
			MenuEntry "Multibrot" $ do
			re <- getPrompt "Real component"
			im <- getPrompt "Imag component"
			putStrLn $ re++" "++im
			meop (multibrot $ readDoub re:+readDoub im) 100,
			MenuEntry "Tricorn" $ meop tricorn 100,
			MenuEntry "Burningship" $ meop burningship 100,
			MenuEntry "Half I" $ meop nodoub 100,
			MenuEntry "Dagger" $ meop dagger 100,
			MenuEntry "XxY" $ meop yxmandel 100
		],
		SubMenu "Newton" $ Menu [
			MenuEntry "Poly>>" $ do
				f <- getPrompt "Coefficients"
				putStrLn f
				meop (newton (makePolyF $ readPoly f) (makePolyF $ diffPoly $ readPoly f)) 5,
			MenuEntry "x5-1" $ meop (newton (\x->x^5-1) (\x->5*x^4)) 5,
			MenuEntry "x5+3x3-x2-1" $ meop (newton (\x->x^5+3*x^3-x^2-1) (\x->5*x^4+9*x^2+2*x)) 5,
			MenuEntry "2x3-2x+2" $ meop (newton (\x->2*x^3-2*x+2) (\x->6*x^2-2)) 5,
			MenuEntry "(sin x)3-1" $ meop (newton (\x->sin x^3-1) (\x->3*cos x*sin x^2)) 5,
			MenuEntry "asin" $ meop (newton asin (\x->1/sqrt(1-x*x))) 5,
			MenuEntry "phase" $ meop (newton ((:+0) . phase) (\x->1/(1+x*x))) 5,
			MenuEntry "xx" $ meop (newton (\x->x**x) (\x->exp(x*log x)*(1+log x))) 5,
			MenuEntry "xx-1" $ meop (newton (\x->x**x-1) (\x->exp(x*log x)*(1+log x))) 5,
			MenuEntry "xx+x2-x" $ meop (newton (\x->x**x+x*x-1) (\x->exp(x*log x)*(1+log x)+x+x)) 5,
			MenuEntry "xx-sin x" $ meop (newton (\x->x**x-sin x) (\x->exp(x*log x)*(1+log x)+cos x)) 5
		],
		SubMenu "Complex" $ Menu [
			MenuEntry "Poly>>" $ do
				f <- getPrompt "Coefficients"
				putStrLn f
				meop (complex $ makePolyF $ readPoly f) 1,
			MenuEntry "x" $ meop (complex id) 1,
			MenuEntry "xx" $ meop (complex (\x->x**x)) 1,
			MenuEntry "(x2-1)(x-2-i)2/(x2+2+2i)" $ meop (complex (\x->(x^2-1)*(x-(2:+(-1)))^2/(x^2+(2:+2)))) 1,
			MenuEntry "sin" $ meop (complex sin) 1,
			MenuEntry "sin . cos" $ meop (complex (sin . cos)) 1
		]]
	mainLoop
	where
		meop x y = func $= x >> finc $= y
		getPrompt x = putStr x >> putStr ": " >> hFlush stdout >> getLine

detailZoom _ dir _ = do
	fiva $~ (max 0 . (+) dir)
	get fiva >>= (windowTitle$=) . show
	postRedisplay Nothing

reshaper (Size xx yy) = let x=min xx yy in do
	windowSize $= Size x x
	loadIdentity
	viewport $= (Position 0 0,Size x x)
	ortho 0 (fromIntegral x) (fromIntegral x) 0 0 1

zoomAdjust :: Position -> IO (Double,Double)
zoomAdjust (Position x y) = do
	(Size w _) <- get windowSize
	(a,b,c) <- get xyold
	return $! (a+(fromIntegral x/fromIntegral w)*c,b+(fromIntegral y/fromIntegral w)*c)
displayZoom (Char ' ') Down _ _ = func $= (\_ _ -> Color3 0 0 0) >> finc $= 1 >> postRedisplay Nothing
displayZoom (MouseButton LeftButton) Down _ xy = xydrt $= True >> zoomAdjust xy >>= (xynew$=)
displayZoom (MouseButton LeftButton) Up _ xy = do
	xyd <- get xydrt
	when xyd $ do
		xydrt $= False
		(xn,yn) <- get xynew
		(x,y) <- zoomAdjust xy
		xyn <- if x==xn && y==yn then get xyold >>= (\(_,_,c)->return (x-c,y-c,c*2))
			else return (min x xn,min y yn,max (abs $ x-xn) (abs $ y-yn))
		xyold $= xyn
		windowTitle $= show xyn
		postRedisplay Nothing
displayZoom (MouseButton MiddleButton) Down _ xy = do
	(x,y) <- zoomAdjust xy
	func $= julia (x:+y)
	finc $= 100
	postRedisplay Nothing
displayZoom _ _ _ _ = xydrt $= False

pts :: (Double,Double,Double) -> GLshort -> [Complex Double]
pts (x1,y1,c) wid = [(x1+(c/w)*x):+(y1+(c/w)*y)|x<-[0..w],y<-[0..w]] where w = fromIntegral wid
displayMap = do
	w <- get windowSize >>= return . fromIntegral . (\(Size w _)-> w-1)
	xy <- get xyold
	t <- getPOSIXTime
	func <- get func
	finc <- get finc
	fiva <- get fiva
	unsafeRenderPrimitive Points $ zipWithM_ (\v c->vertex v >> color c) [Vertex2 a b|a<-[0..w],b<-[0..w]] $ parBuffer 600 rwhnf $ map (func $ fiva*finc) $ pts xy w
	getPOSIXTime >>= putStrLn . show . subtract t
	flush

diffPoly :: [Complex Double] -> [Complex Double]
readPoly :: String -> [Complex Double]
makePolyF :: [Complex Double] -> Complex Double -> Complex Double
readDoub :: String -> Double
diffPoly x = zipWith (*) (tail x) (map (:+0) [1..])
readPoly x = read $ '[':conv x False
	where
		conv [] cm = if cm then "]" else ":+0]"
		conv (x:xs) cm = (if x==',' && not cm then ":+0," else if x=='+' then ":+ " else [x])++conv xs (x/=','&&(cm||x=='+'))
makePolyF [] y = 0
makePolyF (xh:x) y = xh+(sum $ zipWith (*) x $ map (y^) ([1..]::[Int]))
readDoub [] = 0
readDoub x@(h:t) =
	if h == '-' then negate $ readDoub t
	else if all (flip elem $ '.':' ':['0'..'9']) x && (sum $ map (fromEnum . (==) '.') x)<2 then read x else 0
