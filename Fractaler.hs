{-# LANGUAGE BangPatterns#-}
{-# OPTIONS -fexcess-precision -funbox-strict-fields -feager-blackholing -O2#-}
module Main(main) where
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL
import Data.Time.Clock.POSIX(getPOSIXTime)
import Data.IORef
import Data.Complex hiding (magnitude) --Too correct. Faster magnitude in Templates
import Control.Monad
import Control.Parallel.Strategies(parBuffer,rseq,withStrategy)
import System.IO(hFlush,stdout)
import System.Random(randomRIO,randomIO,randomRs,mkStdGen)
import System.Environment(getArgs)
import Unsafe.Coerce(unsafeCoerce)
import Templates
doUntil :: (a -> Bool) -> IO a -> IO a
doUntil p x = do
	r <- x
	if p r then return r else doUntil p x
rancom :: (Double,Double) -> (Double,Double) -> IO (Complex Double)
ranpol :: IO [Complex Double]
rancom x y = randomRIO x >>= return . (:+) >>= (randomRIO y >>=) . (return .)
ranpol = randomIO >>= return . flip take . zipIt . randomRs (-4,4) . mkStdGen >>= (randomRIO (3,8) >>=) . (return .)
	where zipIt (x:y:xs) = (x:+y):zipIt xs
winpr x = windowTitle $= show x >> print x
getmouseflip = do
	w <- get windowSize >>= \(Size w _) -> return ((unsafeCoerce :: GLsizei -> GLint) w)
	get mousePos >>= \(Position x y) -> return (Position x (w-y))
main = do
	initialize
	disableSpecial AutoPollEvent
	openWindow (Size 512 512) [DisplayAlphaBits 8] Window
	blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
	args <- getArgs
	rnd <- return $ null args
	xydrt <- newIORef False
	xyold <- newIORef (-2,-2,4)
	xynew <- newIORef (0,0)
	fdrt <- newIORef True
	fiva <- newIORef 2
	finc <- newIORef 25
	func <- newIORef mandel
	quit <- newIORef False
	let meop x y = func $= x >> finc $= y in main' xydrt xyold xynew fdrt fiva finc func quit [
		("Reset", xyold$=(-2,-2,4)),
		("Julia",
		do
			cm<-if rnd then rancom (-1.5,1.4) (-1.7,1.7) else getPrompt "Coordinate" >>= return . readComp
			winpr cm
			meop (julia cm) 100),
		("JuliaInMan",
		do
			cm<-doUntil ((==Color3 0 0 0) . mandel 2) $ rancom (-2,1.5) (-2,2)
			winpr cm
			meop (julia cm) 100),
		("JuliaInManButOut",
		do
			cm<-doUntil (\x->mandel 9 x==Color3 0 0 0&&mandel 999 x/=Color3 0 0 0) $ rancom (-2,1.5) (-2,2)
			winpr cm
			meop (julia cm) 100),
		("Mandelbrot",meop mandel 100),
		("Multi",
		do
			ex<-if rnd then randomRIO (3,8) else getPrompt "Exponent" >>= return . (truncate::Double->Int) . readDoub
			winpr ex
			meop (multibrot ex) 25),
		("Tricorn",meop tricorn 100),
		("Burningship",meop burningship 100),
		("Half I",meop nodoub 100),
		("Dagger",meop dagger 100),
		("Collatz",meop collatz 10),
		("XxY",meop yxmandel 100),
		("Poly",
		do
			pl<-if rnd then ranpol else getPrompt "Coefficients" >>= return . readPoly
			winpr pl
			meop (newton (makePolyF pl) (makePolyF $ diffPoly pl)) 5),
		("GeneralPoly",
		do
			pl<-if rnd then ranpol else getPrompt "Coefficients" >>= return . readPoly
			cm<-if rnd then randomRIO (-2,2) else getPrompt "Multiplier" >>= return . readDoub
			winpr $ show cm++show pl
			meop (newton ((*(cm:+0)) . makePolyF pl) (makePolyF $ diffPoly pl)) 5),
		("x5-1",meop (newton (\x->x^5-1) (\x->5*x^4)) 5),
		("x5+3x3-x2-1",meop (newton (\x->x^5+3*x^3-x*x-1) (\x->5*x^4+9*x*x+x+x)) 5),
		("2x3-2x+2",meop (newton (\x->2*x*x*x-2*x+2) (\x->6*x*x-2)) 5),
		("sin3-1",meop (newton (\x->sin x^3-1) (\x->3*cos x*sin x*sin x)) 5),
		("asin",meop (newton asin (\x->1/sqrt(1-x*x))) 5),
		("",meop (newton ((:+0) . phase) (\x->1/(1+x*x))) 5),
		("+",meop (newton ((:+0) . phase) (\x->1+x+x)) 5),
		("*",meop (newton ((:+0) . phase) (\x->1+x*x)) 5),
		("^",meop (newton ((:+0) . phase) (\x->1+x**x)) 5),
		("i",meop (newton ((0:+) . phase) (\x->1/(1+x*x))) 5),
		("+i",meop (newton ((0:+) . phase) (\x->1+x+x)) 5),
		("*i",meop (newton ((0:+) . phase) (\x->1+x*x)) 5),
		("^i",meop (newton ((0:+) . phase) (\x->1+x**x)) 5),
		("xx",meop (newton (\x->x**x) (\x->exp(x*log x)*(1+log x))) 5),
		("xx-1",meop (newton (\x->x**x-1) (\x->exp(x*log x)*(1+log x))) 5),
		("xx+x2-x",meop (newton (\x->x**x+x*x-1) (\x->x**x*(1+log x)+x+x)) 5),
		("xx-sin x",meop (newton (\x->x**x-sin x) (\x->x**x*(1+log x)+cos x)) 5),
		("x.5-x",meop (newton (\x->sqrt x-x) (\x->2/sqrt x-1)) 5),
		("Bulbs",meop (newton (\x->2*(x-1)*(x-(0:+1))) (\x->x-(1:+(-1)))) 5),
		("Julia",meop (newton (\x->2*(x-1)*(x-(0:+1))) (\x->4)) 5),
		("Sheath",meop (newton (\x->2*(x-1)*(x-(1:+1))) (\x->2*x*x-((-1):+1)*x-1)) 5),
		("Tricorn",meop (newton conjugate id) 5),
		("1",meop (newton (\(x:+y)->(x*x):+(x-y)) (\(x:+y)->(x+y*x):+(y+x))) 5),
		("2",meop (newton (\(x:+y)->(x*x-1):+(x-y-1)) (\(x:+y)->(x+y*x+1):+(y+x))) 5),
		("Loops",meop (newton id conjugate) 5),
		("Spiral",meop (newton id (\x->phase x:+magnitude x)) 5),
		("Hand",meop (newton (\(x:+y)->(x*x-1):+(x-y+1)) (\(x:+y)->(x+y*x+1):+(y+x))) 5),
		("Sierpinski's Carpet",meop sicarp 1),
		("Sierpinski's Tablecloth",meop sitabl 1),
		("Cantor Dust",meop cantor 1),
		("Cantor Ecstasy",meop cantox 1),
		("Poly",
		do
			pl<-if rnd then ranpol else getPrompt "Coefficients" >>= return . readPoly
			winpr pl
			meop (complex $ makePolyF pl) 1),
		("x",meop (complex id) 1),
		("xx",meop (complex (\x->x**x)) 1),
		("(x2-1)(x-2-i)2/(x2+2+2i)",meop (complex (\x->(x^2-1)*(x-(2:+(-1)))^2/(x^2+(2:+2)))) 1),
		("sin",meop (complex sin) 1),
		("sin . cos",meop (complex (sin . cos)) 1)]
	where
		getPrompt x = putStr (x++": ") >> hFlush stdout >> getLine
main' xydrt xyold xynew fdrt fiva finc func quit menu = do
	keyCallback $= keyZoom fdrt fiva quit
	windowSizeCallback $= reshaper
	mouseButtonCallback $= displayZoom xydrt xyold xynew fdrt finc func menu
	mouseWheelCallback $= \dir -> detailZoom fdrt fiva (if dir>16777216 || dir<0 then (-1) else 1)
	windowCloseCallback $= (quit $= True >> return True)
	mainLoop quit menu
	where
		evalMenu :: [(String, IO ())] -> GLfloat -> GLfloat -> IO ()
		evalMenu ((a,f):as) x y = do
			(Size w _) <- get windowSize
			diffx <- return $ if y>=fromIntegral w then 128 else 0
			diffy <- return $ if y>=fromIntegral w then -y else 16
			(translate $ Vector3 diffx diffy (0 :: GLfloat)) >> renderString Fixed8x16 a >> (if null as then return () else evalMenu as (x+diffx) (y+diffy))
		mainLoop quit menu = do
			get fdrt >>= (flip when $ do
				fdrt $= False
				displayMap xyold fiva finc func)
			getMouseButton ButtonRight >>= (flip (when.(Press==)) $ preservingMatrix $ do
				color $ Color3 (1::GLfloat) 0 0
				translate $ Vector3 0 (-16) (0::GLfloat)
				blend $= Enabled
				evalMenu menu 0 0
				blend $= Disabled)
			swapBuffers >> waitEvents >> get quit >>= (flip unless $ mainLoop quit menu)
detailZoom fdrt fiva dir = do
	fiva $~ (max 0 . (+) dir)
	get fiva >>= winpr
	fdrt $= True
reshaper (Size xx yy) = let x=min xx yy in do
	windowSize $= Size x x
	viewport $= (Position 0 0,Size x x)
	loadIdentity
	ortho 0 (fromIntegral x) 0 (fromIntegral x) 0 1
zoomAdjust :: IORef (Double, Double, Double) -> Position -> IO (Double,Double)
zoomAdjust xyold (Position x y) = do
	(Size w _) <- get windowSize
	(a,b,c) <- get xyold
	return $! (a+(fromIntegral x/fromIntegral w)*c,b+(fromIntegral y/fromIntegral w)*c)
keyZoom _ _ _ (CharKey '.') Press = do
	(Position x y) <- get mousePos
	winpr $ show x++' ':show y
keyZoom fdrt fiva _ (SpecialKey UP) Press = detailZoom fdrt fiva 1
keyZoom fdrt fiva _ (SpecialKey DOWN) Press = detailZoom fdrt fiva (-1)
keyZoom _ _ quit (SpecialKey ESC) Press = quit $= True
keyZoom _ _ _ _ _ = do return ()
displayZoom xydrt xyold xynew _ _ _ _ ButtonLeft Press = do
	xy <- getmouseflip
	xydrt $= True >> zoomAdjust xyold xy >>= (xynew$=)
displayZoom xydrt xyold xynew fdrt _ _ _ ButtonLeft Release = do
	xy <- getmouseflip
	xyd <- get xydrt
	when xyd $ do
		xydrt $= False
		(xn,yn) <- get xynew
		(x,y) <- zoomAdjust xyold xy
		xyn <- if x==xn && y==yn then get xyold >>= (\(_,_,c)->return (x-c,y-c,c*2))
			else return (min x xn,min y yn,max (abs $ x-xn) (abs $ y-yn))
		xyold $= xyn
		winpr xyn
		fdrt $= True
displayZoom _ xyold _ fdrt finc func _ ButtonMiddle Press = do
	xy <- getmouseflip
	(x,y) <- zoomAdjust xyold xy
	func $= julia (x:+y)
	finc $= 100
	fdrt $= True
displayZoom _ _ _ fdrt _ _ menu ButtonRight Release = do
	w <- get windowSize >>= \(Size w _) -> return ((unsafeCoerce :: GLsizei -> GLint) w)
	(Position x y) <- get mousePos
	mid <- return $ fromIntegral $ (div (w-y) 16)+(if x<128 then 0 else (div w 16)+1)
	print mid
	when (mid < length menu) $ do
		snd $ menu !! mid
		fdrt $= True
displayZoom xydrt _ _ _ _ _ _ _ _ = xydrt $= False
pts :: (Double,Double,Double) -> GLshort -> [Complex Double]
pts (x1,y1,c) wid = [(x1+(c/w)*x):+(y1+(c/w)*y)|x<-[0..w],y<-[0..w]] where w = fromIntegral wid
displayMap xyold fiva finc func = do
	w <- get windowSize >>= return . (\(Size w _)->fromIntegral (w-1))
	xy <- get xyold
	func <- get func
	inc <- get finc
	vc <- get fiva >>= \iva -> return $ iva*inc
	t <- getPOSIXTime
	unsafeRenderPrimitive Points $ zipWithM_ (\v c->color c >> vertex v) [Vertex2 a b|a<-[0..w],b<-[0..w]] $ withStrategy (parBuffer 512 rseq) . map (func vc) $ pts xy w
	getPOSIXTime >>= print . subtract t
readDoub :: String -> Double
readComp :: String -> Complex Double
readPoly :: String -> [Complex Double]
diffPoly :: [Complex Double] -> [Complex Double]
makePolyF :: [Complex Double] -> Complex Double -> Complex Double
readDoub [] = 0
readDoub (' ':x) = readDoub x
readDoub ('-':x) = negate $ readDoub x
readDoub x = if all (flip elem "0123456789. ") x && sum(map (fromEnum . (=='.')) x)<2 then read ('0':x) else 0
readComp x = (\(x,y)->(if x==[] then 0 else readDoub x):+if y==[] then 0 else readDoub $ tail y) $ break (=='+') x
readPoly x = readPoly $ break (==',') x
	where
		readPoly (x,[]) = [readComp x]
		readPoly (x,_:xs) = readComp x:(readPoly $ break (==',') xs)
diffPoly [] = []
diffPoly [x] = []
diffPoly (_:x:xs) = x:zipWith (\(x:+y) z->(x*z):+(y*z)) xs [2..]
makePolyF [] y = 0
makePolyF (xh:x) y = xh+(sum $ zipWith (*) x $ iterate (y*) y)
