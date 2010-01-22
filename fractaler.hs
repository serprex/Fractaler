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
		MenuEntry "mandel" $ meop mandel 100,
		MenuEntry "newtoneg" $ meop newtoneg 5,
		MenuEntry "juliaeg" $ meop juliaeg 100,
		MenuEntry "tricorn" $ meop tricorn 100,
		MenuEntry "newtontrig" $ meop newtontrig 5,
		MenuEntry "juliadagger" $ meop juliadagger 100,
		MenuEntry "newtontrigright" $ meop newtontrigright 5,
		MenuEntry "burningship" $ meop burningship 100,
		MenuEntry "nodoub" $ meop nodoub 100,
		MenuEntry "yxmandel" $ meop yxmandel 100]
	mainLoop
	where meop x y = do
		func $= x
		finc $= y
		xydrt $= True
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
displayZoom (MouseButton LeftButton) Down _ xy = zoomAdjust xy >>= (xynew$=)
displayZoom (MouseButton LeftButton) Up _ xy = do
	xyd <- get xydrt
	if xyd then xydrt $= False else do
		(xn,yn) <- get xynew
		(x,y) <- zoomAdjust xy
		xyn <- if x==xn && y==yn then do
				(_,_,c) <- get xyold
				return (x-c,y-c,c*2)
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
	unsafeRenderPrimitive Points $ zipWithM_ (\v c->vertex v >> color c) [Vertex2 a b|a<-[0..w],b<-[0..w]] $ parBuffer 600 rwhnf $ map (func (fiva*finc)) $ pts xy w
	getPOSIXTime >>= putStrLn . show . subtract t

doubleToGF = unsafeCoerce . double2Float
hsvrgb :: Double -> Double -> Double -> Color3 GLfloat
hsvrgb !h !s !v = let
	p=doubleToGF $ v-s*v
	q=doubleToGF $ v-6*h*s*v
	t=doubleToGF $ v-s*v+6*h*s*v
	x=doubleToGF v
	in case mod (floor $ 6*h) 6 of
		0->Color3 x t p
		1->Color3 q x p
		2->Color3 p x t
		3->Color3 p q x
		4->Color3 t p x
		5->Color3 x p q

magsqr,magnitude :: Complex Double -> Double
magsqr (a:+b) = a*a+b*b
magnitude = sqrt . magsqr

newraph f g 0 x = (x,0)
newraph f g m !x = if magnitude(f x)<0.00000005 then (x,m) else newraph f g (m-1) (x-f x/g x)

newton :: (Complex Double -> Complex Double) -> (Complex Double -> Complex Double) -> Int -> Complex Double -> Color3 GLfloat
newton f g z xy = hsvrgb (0.5+atan2 y x/(2*pi)) 1 (fromIntegral zz/fromIntegral z)
	where (x:+y,zz)=newraph f g z xy
newtoneg = newton (\x -> x^5-1) (\x -> 5*x^4)
newtontrig = newton (\x -> sin x^3-1) (\x -> 3*cos x^2)
newtontrigright = newton (\x -> sin x^3-1) (\x -> 3*cos x*sin x^2)

julia :: Double -> Double -> Int -> Complex Double -> Color3 GLfloat
julia x y zz (xx:+yy) = f zz xx yy
	where f z zr zi
		|z==0 = Color3 0 0 0
		|zr*zr*zi*zi<4 = f (z-1) (zr*zr-zi*zi+x) (2*zr*zi+y)
		|True = Color3 ((fromIntegral z/fromIntegral zz)^3) ((fromIntegral z/fromIntegral zz)^2) (fromIntegral z/fromIntegral zz)
juliaeg = julia (-0.35) 0.75

juliadagger :: Int -> Complex Double -> Color3 GLfloat
juliadagger zz (x:+y) = f zz x y
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
