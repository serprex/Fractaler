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
import GHC.Float(double2Float)
import Unsafe.Coerce(unsafeCoerce)
import System.IO(hFlush,stdout)

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
	ovp <- get overlayPossible
	putStrLn $ show ovp
	when ovp (hasOverlay $= True)
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
				meop (newton (makePolyF (readPoly f)) (makePolyF (diffPoly (readPoly f)))) 5,
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
				meop (complex (makePolyF (readPoly f))) 1,
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

windowLength = get windowSize >>= (\(Size w h)->return w)
zoomAdjust (Position x y) = do
	w <- windowLength
	(a,b,c) <- get xyold
	return $! (a+(fromIntegral x/fromIntegral w)*c,b+(fromIntegral y/fromIntegral w)*c)
displayZoom (Char ' ') Down _ _ = func $= (\_ _ -> Color3 0 0 0) >> finc $= 1
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
	w <- windowLength >>= return . fromIntegral . subtract 1
	xy <- get xyold
	t <- getPOSIXTime
	func <- get func
	finc <- get finc
	fiva <- get fiva
	unsafeRenderPrimitive Points $ zipWithM_ (\v c->vertex v >> color c) [Vertex2 a b|a<-[0..w],b<-[0..w]] $ parBuffer 600 rwhnf $ map (func $ fiva*finc) $ pts xy w
	getPOSIXTime >>= putStrLn . show . subtract t
	flush

diffPoly :: [Complex Double] -> [Complex Double]
diffPoly [] = []
diffPoly (x:xs) = zipWith (*) xs (map (:+0) [1..])
readPoly :: String -> [Complex Double]
readPoly x = read $ '[':conv x False
	where
		conv [] cm = if cm then "]" else ":+0]"
		conv (x:xs) cm = (if x==',' && not cm then ":+0," else if x=='+' then ":+ " else [x])++conv xs (x/=','&&(cm||x=='+'))
makePolyF :: [Complex Double] -> Complex Double -> Complex Double
makePolyF !x !y = sum $ zipWith (*) x (map (y^) ([1..]::[Int]))

readDoub x@(h:t) =
	if h == '-' then negate $ readDoub t
	else if x/="" && all (flip elem $ '.':' ':['0'..'9']) x && (sum $ map (fromEnum . (==) '.') x)<2 then read x else 0
doubleToGF = unsafeCoerce . double2Float
hvrgb :: Complex Double -> Double -> Color3 GLfloat
hvrgb !hc !v = (\(a,b,c)->Color3 (doubleToGF a) (doubleToGF b) (doubleToGF c)) $ case truncate h of
	0->(v,v*hf,0)
	1->(v*(1-hf),v,0)
	2->(0,v,v*hf)
	3->(0,v*(1-hf),v)
	4->(v*hf,0,v)
	5->(v,0,v*(1-hf))
	x->if x>0 then (0.75,0.75,0.75) else (0.25,0.25,0.25)
	where
		h=3+phase hc*3/pi
		hf=h-(fromIntegral . truncate) h

magsqr,magnitude :: Complex Double -> Double
magsqr (a:+b) = a*a+b*b
magnitude = sqrt . magsqr

complex :: (Complex Double -> Complex Double) -> Int -> Complex Double -> Color3 GLfloat
complex f 0 xy = hvrgb (f xy) 1
complex f 1 xy = hvrgb (f xy) $ magnitude $ f xy
complex f z xy = hvrgb (f xy) $ logBase (fromIntegral z) $ magnitude (f xy)+1

newton :: (Complex Double -> Complex Double) -> (Complex Double -> Complex Double) -> Int -> Complex Double -> Color3 GLfloat
newton f g z xy = hvrgb (x:+y) $ fromIntegral zz/fromIntegral z
	where
		newraph f g m x = if m==0 then (x,0) else if magsqr(f x)<1/fromIntegral m then (x,m) else newraph f g (m-1) (x-f x/g x)
		(x:+y,zz)=newraph f g z xy

julia :: (Complex Double) -> Int -> Complex Double -> Color3 GLfloat
julia (x:+y) zz (xx:+yy) = f zz xx yy
	where f z zr zi
		|z==0 = Color3 0 0 0
		|zr*zr*zi*zi<4 = f (z-1) (zr*zr-zi*zi+x) (2*zr*zi+y)
		|True = Color3 ((fromIntegral z/fromIntegral zz)^3) ((fromIntegral z/fromIntegral zz)^2) (fromIntegral z/fromIntegral zz)

dagger :: Int -> Complex Double -> Color3 GLfloat
dagger zz (x:+y) = f zz x y
	where f z zr zi
		|z==0 = Color3 0 0 0
		|zr*zr*zi*zi<4 = f (z-1) (zr*zr-zi*zi+cos (atan2 y x)) (2*zr*zi+sin (atan2 y x))
		|True = Color3 ((fromIntegral z/fromIntegral zz)^3) ((fromIntegral z/fromIntegral zz)^2) (fromIntegral z/fromIntegral zz)

mandel :: Int -> Complex Double -> Color3 GLfloat
mandel ii (x:+y) = f ii x y
	where f i !zr !zi
		|i==0 = Color3 0 0 0
		|zr*zr+zi*zi<4 = f (i-1) (zr*zr-zi*zi+x) (2*zr*zi+y)
		|True = Color3 ((fromIntegral i/fromIntegral ii)^3) ((fromIntegral i/fromIntegral ii)^2) (fromIntegral i/fromIntegral ii)

multibrot :: Complex Double -> Int -> Complex Double -> Color3 GLfloat
multibrot ex ii xy = f ii xy
	where f i z
		|i==0 = Color3 0 0 0
		|magsqr z<4 = f (i-1) (z**ex+xy)
		|True = Color3 ((fromIntegral i/fromIntegral ii)^3) ((fromIntegral i/fromIntegral ii)^2) (fromIntegral i/fromIntegral ii)

yxmandel :: Int -> Complex Double -> Color3 GLfloat
yxmandel zz (x:+y) = f zz x y
	where f z !zr !zi
		|z==0 = Color3 0 0 0
		|zr*zr+zi*zi<4 = f (z-1) (zr*zr-zi*zi+y*x) (2*zr*zi+y)
		|True = Color3 ((fromIntegral z/fromIntegral zz)^3) ((fromIntegral z/fromIntegral zz)^2) (fromIntegral z/fromIntegral zz)

nodoub :: Int -> Complex Double -> Color3 GLfloat
nodoub zz (x:+y) = f zz x y
	where f z !zr !zi
		|z==0 = Color3 0 0 0
		|zr*zr+zi*zi<4 = f (z-1) (zr*zr-zi*zi+x) (zr*zi+y)
		|True = Color3 ((fromIntegral z/fromIntegral zz)^3) ((fromIntegral z/fromIntegral zz)^2) (fromIntegral z/fromIntegral zz)

tricorn :: Int -> Complex Double -> Color3 GLfloat
tricorn zz (x:+y) = f zz x y
	where f z !zr !zi
		|z==0 = Color3 0 0 0
		|zr*zr+zi*zi<4 = f (z-1) (zi*zi-zr*zr+x) (2*zr*zi+y)
		|True = Color3 ((fromIntegral z/fromIntegral zz)^3) ((fromIntegral z/fromIntegral zz)^2) (fromIntegral z/fromIntegral zz)

burningship :: Int -> Complex Double -> Color3 GLfloat
burningship zz (x:+y) = f zz x y
	where f z !zr !zi
		|z==0 = Color3 0 0 0
		|zr*zr+zi*zi<4 = f (z-1) (zi*zi-zr*zr+x) (2*abs (zr*zi)+y)
		|True = Color3 ((fromIntegral z/fromIntegral zz)^3) ((fromIntegral z/fromIntegral zz)^2) (fromIntegral z/fromIntegral zz)
