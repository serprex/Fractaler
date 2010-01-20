{-# LANGUAGE BangPatterns#-}
{-# OPTIONS -fexcess-precision -funbox-strict-fields -feager-blackholing -O2 #-}
module Main(main) where
import Graphics.UI.GLUT
import Data.Time.Clock.POSIX(getPOSIXTime)
import Data.IORef
import Data.Complex
import Control.Monad
import Control.Parallel.Strategies
import System.IO.Unsafe(unsafePerformIO)
import GHC.Float
import Unsafe.Coerce

doubleToGF = unsafeCoerce . double2Float

finc = 100
{-# NOINLINE func #-}
func = unsafePerformIO $ newIORef finc
{-# NOINLINE xyold #-}
xyold = unsafePerformIO $ newIORef (-2,-2,2,2)
{-# NOINLINE xynew #-}
xynew = unsafePerformIO $ newIORef (0,0)

main = do
	getArgsAndInitialize
	initialWindowSize $= Size 600 600
	createWindow ""
	displayCallback $= displayMap
	keyboardMouseCallback $= Just displayZoom
	reshapeCallback $= Just reshaper
	mouseWheelCallback $= Just detailZoom
	mainLoop
detailZoom _ dir _ = do
	func $~ (max 0 . (+) (dir*finc))
	get func >>= (windowTitle$=) . show
	postRedisplay Nothing

reshaper (Size xx yy) = let x=min xx yy in do
	windowSize $= Size x x
	loadIdentity
	viewport $= (Position 0 0,Size x x)
	ortho 0 (fromIntegral x) (fromIntegral x) 0 0 1

windowLength = get windowSize >>= (\(Size w h)->return w)
zoomAdjust (Position x y) = do
	w <- windowLength
	(a,c,b,d) <- get xyold
	return $! (a+(fromIntegral x/fromIntegral w)*(b-a),c+(fromIntegral y/fromIntegral w)*(d-c))
displayZoom (MouseButton LeftButton) Down _ xy = zoomAdjust xy >>= (xynew$=)
displayZoom (MouseButton LeftButton) Up _ xy = do
	(xn,yn) <- get xynew
	(x,y) <- zoomAdjust xy
	(x1,y1,x2,y2) <- get xyold
	(x1,y1,x2,y2) <- return (if x==xn && y==yn then (x-x2+x1,y-y2+y1,x+x2-x1,y+y2-y1)
		else (min x xn,min y yn,max x xn,max y yn))
	xyold $= (x1,y1,x1+max (x2-x1) (y2-y1),y1+max (x2-x1) (y2-y1))
	xyon <- get xyold
	windowTitle $= show xyon
	postRedisplay Nothing
displayZoom _ _ _ _ = return ()

displayMap = do
	w <- windowLength >>= (return . fromIntegral . subtract 1)
	f <- get func
	xy <- get xyold
	t <- getPOSIXTime
	unsafeRenderPrimitive Points $ zipWithM_ (\v c->vertex v >> color c) ([Vertex2 a b|a<-[0..w],b<-[0..w]]::[Vertex2 GLshort]) $ parBuffer 600 rwhnf $ map (fractal f) $ pts xy w
	getPOSIXTime >>= putStrLn . show . subtract t
pts :: (Double,Double,Double,Double) -> GLshort -> [Complex Double]
pts (x1,y1,x2,y2) wid = [(x1+((x2-x1)/w)*x):+(y1+((y2-y1)/w)*y)|x<-[0..w],y<-[0..w]] where w = fromIntegral wid

magsqr (x:+y) = x^2+y^2
fractal = mandel--newtondeg

derive f x = (f (x+d)-f x)/d where d=0.0000000000005
newraph 0 f x = x
newraph !m f !x = if magsqr (f x) < 0.5 then x else newraph (m-1) f (x-f x/derive f x)
newraphic 0 f !x = (x,0)
newraphic !m f !x = if magsqr (f x) < 0.5 then (x,m) else newraphic (m-1) f (x-f x/derive f x)

newraphd 0 f g x = x
newraphd !m f g !x = if realPart(f x)^2+imagPart(f x)^2<0.5 then x else newraphd (m-1) f g (x-f x/g x)
newraphicd 0 f g !x = (x,0)
newraphicd !m f g !x = if realPart(f x)^2+imagPart(f x)^2<0.5 then (x,m) else newraphicd (m-1) f g (x-f x/g x)

newton :: (Complex Double -> Complex Double) -> Int -> Complex Double -> Color3 GLfloat
newton f z xy = Color3 (fromIntegral zz/fromIntegral z) (doubleToGF x) (doubleToGF y)
	where ((x:+y),zz)=newraphic z f xy
newtoneg = newton (\x -> x^3-1)

newtond :: (Complex Double -> Complex Double) -> (Complex Double -> Complex Double) -> Int -> Complex Double -> Color3 GLfloat
newtond f g z xy = Color3 (fromIntegral zz) (doubleToGF x) (doubleToGF y)
	where ((x:+y),zz)=newraphicd z f g xy
newtondeg = newtond (\x -> x^3-1) (\x -> 3*x^2)

julia :: Double -> Double -> Int -> Complex Double -> Color3 GLfloat
julia x y zz (xx:+yy) = f zz xx yy
	where f !z zr zi
		|z==0 = Color3 0 0 0
		|zr*zr*zi*zi<4 = f (z-1) (zr*zr-zi*zi+x) (2*zr*zi+y)
		|True = Color3 ((fromIntegral z/fromIntegral zz)^3) ((fromIntegral z/fromIntegral zz)^2) (fromIntegral z/fromIntegral zz)
juliaeg = julia (-1) (0.2) 

juliadagger :: Int -> Complex Double -> Color3 GLfloat
juliadagger zz (x:+y) = f zz (x:+y)
	where f !z (zr:+zi)
		|z==0 = Color3 0 0 0
		|zr*zr*zi*zi<4 = f (z-1) ((zr*zr-zi*zi+cos (atan2 y x)):+(2*zr*zi+sin (atan2 y x)))
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
