{-# LANGUAGE BangPatterns#-}
{-# OPTIONS -fexcess-precision -funbox-strict-fields -feager-blackholing -O2#-}
module Main(main) where
import Prelude hiding (init)
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.FTGL
import Graphics.UI.GLFW
import Data.IORef
import Data.Complex hiding (magnitude) --Too correct. Faster magnitude in Templates
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Monad
import Control.Parallel.Strategies(parBuffer,rseq,withStrategy)
import System.IO(hFlush,stdout)
import System.Random(randomRIO,randomIO,randomRs,mkStdGen)
import System.Environment(getArgs)
import GHC.Float(double2Float)
import Numeric.FastMath
import Templates

doUntil :: (a -> Bool) -> IO a -> IO a
doUntil p x = x >>= \r -> if p r then return r else doUntil p x
rancom :: (Double,Double) -> (Double,Double) -> IO (Complex Double)
ranpol :: IO [Complex Double]
rancom x y = randomRIO x >>= return . (:+) >>= (randomRIO y >>=) . (return .)
ranpol = randomIO >>= return . flip take . zipIt . randomRs (-4,4) . mkStdGen >>= (randomRIO (3,8) >>=) . (return .)
	where zipIt (x:y:xs) = (x:+y):zipIt xs
ranpolmag = ranpol >>= mapM (\(x:+y) -> randomRIO (1,99) >>= (\z-> return (((x/2):+(y/2)),z)))

winpr :: Show a => Window -> a -> IO ()
winpr wnd x = setWindowTitle wnd (show x) >> print x

getwinwid :: Window -> IO Double
getwinwid wnd = getWindowSize wnd >>= return . fromIntegral . fst

get :: IORef a -> IO a
get = readIORef

($=) :: IORef a -> a -> IO ()
($=) = writeIORef

($~) :: IORef a -> (a -> a) -> IO ()
($~) = modifyIORef

main = do
	setErrorCallback $ Just (\e s -> putStrLn $ unwords [show e, show s])
	init
	mapM_ windowHint [WindowHint'ContextVersionMajor 3, WindowHint'ContextVersionMinor 0]
	(Just wnd) <- createWindow 512 512 "Fractaler" Nothing Nothing
	makeContextCurrent (Just wnd)
	mapM_ glEnableClientState [gl_VERTEX_ARRAY, gl_TEXTURE_COORD_ARRAY]
	vbotx <- alloca (\x -> glGenBuffers 2 x >> peek x)
	mapM_ (\(vbo,v,glf) -> do
		glBindBuffer gl_ARRAY_BUFFER vbo
		withArray v (\v -> glBufferData gl_ARRAY_BUFFER 48 v gl_STATIC_DRAW)
		glf 2 gl_FLOAT 0 nullPtr) [(vbotx, [(-1::Float),-1,1,-1,1,1,1,1,-1,1,-1,-1], glVertexPointer), (vbotx+1, [(0::Float),0,0,1,1,1,1,1,1,0,0,0], glTexCoordPointer)]
	glEnable gl_TEXTURE_2D
	font <- createTextureFont "DejaVuSansMono.ttf"
	setFontFaceSize font 16 0
	gfxtx <- alloca (\x -> glGenTextures 1 x >> peek x)
	glBindTexture gl_TEXTURE_2D gfxtx
	mapM_ (\wrap -> glTexParameteri gl_TEXTURE_2D wrap $ fromIntegral gl_CLAMP_TO_EDGE) [gl_TEXTURE_WRAP_T, gl_TEXTURE_WRAP_S] 
	mapM_ (\mfilt -> glTexParameteri gl_TEXTURE_2D mfilt $ fromIntegral gl_NEAREST) [gl_TEXTURE_MAG_FILTER, gl_TEXTURE_MIN_FILTER]
	reshaper wnd 512 512
	args <- getArgs
	rnd <- return $ null args
	xydrt <- newIORef False
	xyold <- newIORef (-2,-2,4)
	xynew <- newIORef (0,0)
	fdrt <- newIORef True
	fiva <- newIORef 2
	finc <- newIORef 25
	func <- newIORef mandel
	let meop x y = func $= x >> finc $= y in main' wnd xydrt xyold xynew fdrt fiva finc func gfxtx font [
		("Reset", xyold$=(-2,-2,4)),
		("Julia",
		do
			cm<-if rnd then rancom (-1.5,1.4) (-1.7,1.7) else getPrompt "Coordinate" >>= return . readComp
			winpr wnd cm
			meop (julia cm) 100),
		("JuliaInMan",
		do
			cm<-doUntil ((==(0,0,0)) . mandel 2) $ rancom (-2,1.5) (-2,2)
			winpr wnd cm
			meop (julia cm) 100),
		("JuliaInManButOut",
		do
			cm<-doUntil (\x->mandel 9 x==(0,0,0)&&mandel 999 x/=(0,0,0)) $ rancom (-2,1.5) (-2,2)
			winpr wnd cm
			meop (julia cm) 100),
		("Mandelbrot",meop mandel 100),
		("Multi",
		do
			ex<-if rnd then randomRIO (3,8) else getPrompt "Exponent" >>= return . (truncate::Double->Int) . readDoub
			winpr wnd ex
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
			winpr wnd pl
			meop (newton (makePolyF pl) (makePolyF $ diffPoly pl)) 5),
		("GeneralPoly",
		do
			pl<-if rnd then ranpol else getPrompt "Coefficients" >>= return . readPoly
			cm<-if rnd then randomRIO (-2,2) else getPrompt "Multiplier" >>= return . readDoub
			winpr wnd $ show cm++show pl
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
			winpr wnd pl
			meop (complex $ makePolyF pl) 1),
		("x",meop (complex id) 1),
		("xx",meop (complex (\x->x**x)) 1),
		("(x2-1)(x-2-i)2/(x2+2+2i)",meop (complex (\x->(x^2-1)*(x-(2:+(-1)))^2/(x^2+(2:+2)))) 1),
		("sin",meop (complex sin) 1),
		("sin . cos",meop (complex (sin . cos)) 1),
		("metaballs",
		do
			bs <- if rnd then ranpolmag else (do
				pol <- getPrompt "Balls" >>= return . readPoly
				mag <- getPrompt "Size" >>= return . readDoubs
				return $ zip pol mag)
			meop (metaball bs) 80)]
	where
		getPrompt x = putStr (x++": ") >> hFlush stdout >> getLine

main' :: Window -> IORef Bool -> IORef (Double,Double,Double) -> IORef (Double,Double) -> IORef Bool -> IORef Int -> IORef Int -> IORef FractalCb -> GLuint -> Font -> [(String, IO ())] -> IO ()
main' wnd xydrt xyold xynew fdrt fiva finc func gfxtx font menu = do
	setKeyCallback wnd $ Just $ keyZoom fdrt fiva
	setWindowSizeCallback wnd $ Just $ reshaper
	setMouseButtonCallback wnd $ Just $ displayZoom xydrt xyold xynew fdrt finc func menu
	setScrollCallback wnd $ Just $ \wnd dx dy -> detailZoom fdrt fiva (if dy<0 then (-1) else 1) wnd
	setWindowCloseCallback wnd $ Just $ (flip setWindowShouldClose) True
	mainLoop
	where
		evalMenu :: [(String, IO ())] -> Int -> Int -> IO ()
		evalMenu ((a,f):as) x y = let (diffx,diffy) = (if y==0 then (196, 496) else (0, -16)) in do
			renderFont font a Front
			glTranslatef (fromIntegral diffx) (fromIntegral diffy) 0
			(if null as then return () else evalMenu as (x+diffx) (y+diffy))
		mainLoop :: IO ()
		mainLoop = do
			get fdrt >>= (flip when $ do
				fdrt $= False
				displayMap xyold fiva finc func wnd)
			glDrawArrays gl_TRIANGLES 0 6
			getMouseButton wnd MouseButton'2 >>= (flip (when.(MouseButtonState'Pressed==)) $ do
				glColor3f 1 0 0
				glPushMatrix
				glOrtho 0 511 0 511 (-1) 1
				glTranslatef 0 496 0
				evalMenu menu 0 496
				glPopMatrix
				glColor3f 1 1 1
				glBindTexture gl_TEXTURE_2D gfxtx)
			swapBuffers wnd
			glClear gl_COLOR_BUFFER_BIT
			waitEvents
			windowShouldClose wnd >>= (flip unless) mainLoop

detailZoom :: IORef Bool -> IORef Int -> Int -> Window -> IO ()
detailZoom fdrt fiva dir wnd = do
	fiva $~ (max 0 . (+) dir)
	get fiva >>= winpr wnd
	fdrt $= True

reshaper :: WindowSizeCallback
reshaper wnd xx yy = let x=min xx yy in do
	glViewport 0 0 (toEnum x) (toEnum x)

zoomAdjust :: IORef (Double,Double,Double) -> Window -> (Double,Double) -> IO (Double,Double)
zoomAdjust xyold wnd (x,y) = do
	w <- getwinwid wnd
	(a,b,c) <- get xyold
	return $! (a+x*(c/w),b+c-y*(c/w))

keyZoom :: IORef Bool -> IORef Int -> KeyCallback
keyZoom fdrt fiva wnd (Key'Up) _ KeyState'Pressed _ = detailZoom fdrt fiva 1 wnd
keyZoom fdrt fiva wnd (Key'Down) _ KeyState'Pressed _ = detailZoom fdrt fiva (-1) wnd
keyZoom _ _ wnd (Key'Escape) _ KeyState'Pressed _ = setWindowShouldClose wnd True
keyZoom _ _ _ _ _ _ _ = do return ()

displayZoom :: IORef Bool -> IORef (Double,Double,Double) -> IORef (Double,Double) -> IORef Bool -> IORef Int -> IORef FractalCb -> [(String, IO ())] -> MouseButtonCallback
displayZoom xydrt xyold xynew _ _ _ _ wnd MouseButton'1 MouseButtonState'Pressed _ = do
	xy <- getCursorPos wnd
	xydrt $= True >> zoomAdjust xyold wnd xy >>= (xynew$=)
displayZoom xydrt xyold xynew fdrt _ _ _ wnd MouseButton'1 MouseButtonState'Released _ = do
	xy <- getCursorPos wnd
	xyd <- get xydrt
	when xyd $ do
		xydrt $= False
		(xn,yn) <- get xynew
		(x,y) <- zoomAdjust xyold wnd xy
		xyn <- if x==xn && y==yn then get xyold >>= (\(_,_,c)->return (x-c,y-c,c*2))
			else return (min x xn,min y yn,max (abs $ x-xn) (abs $ y-yn))
		xyold $= xyn
		winpr wnd xyn
		fdrt $= True
displayZoom _ xyold _ fdrt finc func _ wnd MouseButton'3 MouseButtonState'Pressed _ = do
	xy <- getCursorPos wnd
	(x,y) <- zoomAdjust xyold wnd xy
	func $= julia (x:+y)
	finc $= 100
	fdrt $= True
displayZoom _ _ _ fdrt _ _ menu wnd MouseButton'2 MouseButtonState'Released _ = do
	(x,y) <- getCursorPos wnd
	mid <- return $ truncate $ (y/16)+(if x<196 then 0 else 32)
	print mid
	when (mid < length menu) $ do
		snd $ menu !! mid
		fdrt $= True
displayZoom xydrt _ _ _ _ _ _ _ _ _ _ = xydrt $= False

pts :: (Double,Double,Double) -> Double -> [Complex Double]
pts (x1,y1,c) w = let cw = c/w in [(x1+x):+(y1+y)|x<-[0,cw..c],y<-[0,cw..c]]

displayMap :: IORef (Double,Double,Double) -> IORef Int -> IORef Int -> IORef FractalCb -> Window -> IO ()
displayMap xyold fiva finc func wnd = do
	w <- getwinwid wnd >>= return . (subtract 1)
	tw <- return $ truncate w
	xy <- get xyold
	func <- get func
	inc <- get finc
	vc <- get fiva >>= return . (inc*)
	(Just t) <- getTime
	colors <- return $ map double2Float $ concatMap (\(r,g,b) -> [r,g,b]) $ withStrategy (parBuffer 512 rseq) . map (func vc) $ pts xy w
	withArray colors $ glTexImage2D gl_TEXTURE_2D 0 (fromIntegral gl_RGB) (tw+1) (tw+1) 0 gl_RGB gl_FLOAT
	getTime >>= print . subtract t . (\(Just x) -> x)

readDoub :: String -> Double
readComp :: String -> Complex Double
readMulti :: (String -> a) -> String -> [a]
readDoubs :: String -> [Double]
readPoly :: String -> [Complex Double]
diffPoly :: [Complex Double] -> [Complex Double]
makePolyF :: [Complex Double] -> Complex Double -> Complex Double
readDoub [] = 0
readDoub (' ':x) = readDoub x
readDoub ('-':x) = negate $ readDoub x
readDoub x = if all (flip elem "0123456789. ") x && sum(map (fromEnum . (=='.')) x)<2 then read ('0':x) else 0
readComp x = (\(x,y)->(if x==[] then 0 else readDoub x):+if y==[] then 0 else readDoub $ tail y) $ break (=='+') x
readMulti f x = readM $ break (==',') x
	where
		readM (x,[]) = [f x]
		readM (x,_:xs) = f x:(readM $ break (==',') xs)
readDoubs = readMulti readDoub
readPoly = readMulti readComp
diffPoly [] = []
diffPoly [x] = []
diffPoly (_:x:xs) = x:zipWith (\(x:+y) z->(x*z):+(y*z)) xs [2..]
makePolyF [] !y = 0
makePolyF (xh:x) !y = xh+(sum $ zipWith (*) x $ iterate (y*) y)
