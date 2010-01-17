{-# LANGUAGE BangPatterns, MagicHash #-}
{-# OPTIONS -fexcess-precision -funbox-strict-fields -feager-blackholing -O2 #-}
import Graphics.UI.GLUT
import Data.Time.Clock.POSIX(getPOSIXTime)
import Data.StateVar
import Data.IORef
import Control.Monad
import Control.Parallel.Strategies
import System.IO.Unsafe(unsafePerformIO)

{-# NOINLINE func #-}
func = unsafePerformIO $ newIORef 1000
{-# NOINLINE xyold #-}
xyold = unsafePerformIO $ newIORef ((-2),(2),(-2),(2))
{-# NOINLINE xynew #-}
xynew = unsafePerformIO $ newIORef (0,0)
{-# NOINLINE ptsg #-}
ptsg = unsafePerformIO $ newIORef ([]::[Vertex2 GLshort])

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
	f <- get func
	func $= max 1 (f+dir*100)
	windowTitle $= show f
	postRedisplay Nothing

reshaper (Size !x !y) = do
	(x,y) <- return (600,600)
	windowSize $= Size (min x y) (min x y)
	loadIdentity
	ortho 0 (fromIntegral (min x y)) (fromIntegral (min x y)) 0 0 1
	ptsg $= [Vertex2 x y|x<-w,y<-w] where w=[0..fromIntegral (min x y)-1]

windowLength = get windowSize >>= (\(Size w h)->return w)
zoomAdjust (Position x y) xyo = do
	w <- windowLength
	(a,b,c,d) <- get xyo
	return $! (a+(fromIntegral x/fromIntegral w)*(b-a),c+(fromIntegral y/fromIntegral w)*(d-c))
displayZoom (MouseButton LeftButton) Down _ xy = zoomAdjust xy xyold >>= (xynew$=)
displayZoom (MouseButton LeftButton) Up _ xy = do
	(xn,yn) <- get xynew
	(x,y) <- zoomAdjust xy xyold
	(x1,x2,y1,y2) <- get xyold
	(x1,x2,y1,y2) <- return (if x==xn && y==yn then (x-x2+x1,x+x2-x1,y-y2+y1,y+y2-y1)
		else (min x xn,max x xn,min y yn,max y yn))
	xyold $= (x1,x1+max (x2-x1) (y2-y1),y1,y1+max (x2-x1) (y2-y1))
	xyon <- get xyold
	windowTitle $= show xyon
	postRedisplay Nothing
displayZoom _ _ _ _ = return ()

displayMap = do
	w <- windowLength
	ptsg <- get ptsg
	f <- get func
	xy <- get xyold
	t <- getPOSIXTime
	unsafeRenderPrimitive Points $ zipWithM_ (\ !v !c->vertex v >> color c) ptsg $ parBuffer 600 rwhnf $ map (mandel f) $ pts xy w
	getPOSIXTime >>= putStrLn . show . subtract t

pts :: (Double,Double,Double,Double) -> GLsizei -> [(Double,Double)]
pts (!x1,!x2,!y1,!y2) !wid = [(x1+((x2-x1)/w)*x,y1+((y2-y1)/w)*y)|x<-[0..w],y<-[0..w]] where w=fromIntegral wid-1
mandel :: Int -> (Double,Double) -> Color3 GLfloat
mandel !zz (!x,!y) = mandel zz x y
	where mandel !z !zr !zi
		|z==0 = Color3 0 0 0
		|zr*zr+zi*zi<4 = mandel (z-1) (zr*zr-zi*zi+x) (2*zr*zi+y)
		|True = Color3 ((fromIntegral z/fromIntegral zz)^3) ((fromIntegral z/fromIntegral zz)^2) (fromIntegral z/fromIntegral zz)
