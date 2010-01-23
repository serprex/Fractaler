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

i=(0:+1)
{-# NOINLINE func#-}
func = unsafePerformIO $ newIORef $ mandel
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
		MenuEntry "reset" $ xyold$=(-2,-2,4) >> xydrt$=True,
		SubMenu "Fantou" $ Menu [
			MenuEntry "Generic" $ meop mandel 100,
			MenuEntry "Tricorn" $ meop tricorn 100,
			MenuEntry "Burningship" $ meop burningship 100,
			MenuEntry "Half I" $ meop nodoub 100,
			MenuEntry "Dagger" $ meop dagger 100,
			MenuEntry "XxY" $ meop yxmandel 100
		],
		SubMenu "Julia" $ Menu [
			MenuEntry "-.35 .75" $ meop (julia (-0.35) 0.75) 100,
			MenuEntry ".285 0" $ meop (julia 0.285 0) 100,
			MenuEntry ".285 .01" $ meop (julia 0.285 0.01) 100,
			MenuEntry "-.8 .156" $ meop (julia (-0.8) 0.156) 100
		],
		SubMenu "Newton" $ Menu [
			MenuEntry "x5-1" $ meop (newton (\x->x^5-1) (\x->5*x^4)) 5,
			MenuEntry "x5+3x3-x2-1" $ meop (newton (\x->x^5+3*x^3-x^2-1) (\x->5*x^4+9*x^2+2*x)) 5,
			MenuEntry "(sin x)3-1" $ meop (newton (\x->sin x^3-1) (\x->3*cos x*sin x^2)) 5,
			MenuEntry "asin" $ meop (newton asin (\x->1/sqrt(1-x*x))) 5,
			MenuEntry "phase" $ meop (newton ((:+0) . phase) (\x->1/(1+x*x))) 5
		],
		SubMenu "Complex" $ Menu [
			MenuEntry "(x2-1)(x-2-i)2/(x2+2+2i)" $ meop (complex (\x->(x^2-1)*(x-2-i)^2/(x^2+2+2*i))) 1,
		]]
	mainLoop
	where meop x y = func $= x >> finc $= y >> xydrt $= True

detailZoom _ dir _ = do
	fiva $~ (max 1 . (+) dir)
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
displayZoom (MouseButton LeftButton) Down _ xy = zoomAdjust xy >>= (xynew$=)
displayZoom (MouseButton LeftButton) Up _ xy = do
	xyd <- get xydrt
	if xyd then xydrt $= False else do
		(xn,yn) <- get xynew
		(x,y) <- zoomAdjust xy
		xyn <- if x==xn && y==yn then get xyold >>= (\(_,_,c)->return (x-c,y-c,c*2))
			else return (min x xn,min y yn,max (abs $ x-xn) (abs $ y-yn))
		xyold $= xyn
		windowTitle $= show xyn
		postRedisplay Nothing
displayZoom _ _ _ _ = return ()

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

doubleToGF = unsafeCoerce . double2Float
hvrgb :: Complex Double -> Double -> Color3 GLfloat
hvrgb !hc !v = Color3 (l 0) (l 4) (l 2) where
	h=3+phase hc*3/pi
	l=doubleToGF . (!!) [v,v-h*v*v,0,0,h*v,v] . (flip rem) 6 . (+(truncate h))
magsqr,magnitude :: Complex Double -> Double
magsqr (a:+b) = a*a+b*b
magnitude = sqrt . magsqr

complex :: (Complex Double -> Complex Double) -> Int -> Complex Double -> Color3 GLfloat
complex f z xy = hvrgb (f xy) $ logBase (fromIntegral z) $ magnitude (f xy)+1

newton :: (Complex Double -> Complex Double) -> (Complex Double -> Complex Double) -> Int -> Complex Double -> Color3 GLfloat
newton f g z xy = hvrgb (x:+y) (fromIntegral zz/fromIntegral z)
	where
		newraph f g m x = if m==0 then (x,0) else if magsqr(f x)<1/fromIntegral m then (x,m) else newraph f g (m-1) (x-f x/g x)
		(x:+y,zz)=newraph f g z xy

julia :: Double -> Double -> Int -> Complex Double -> Color3 GLfloat
julia x y zz (xx:+yy) = f zz xx yy
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
