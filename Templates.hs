{-# LANGUAGE BangPatterns#-}
{-# OPTIONS -fexcess-precision -funbox-strict-fields -feager-blackholing -O2#-}
module Templates(complex,newton,multibrot,julia,tricorn,burningship,nodoub,yxmandel,dagger,mandel,magnitude,magsqr) where
import Graphics.Rendering.OpenGL(GLfloat)
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Data.Complex hiding (magnitude)
import GHC.Float(double2Float,significand,isNaN,isInfinite)
import Unsafe.Coerce(unsafeCoerce)

doubleToGF = unsafeCoerce . double2Float :: Double -> GLfloat
sqr x=x*x
cub x=x*x*x
hvrgb :: Complex Double -> Double -> Color3 GLfloat
hvrgb hc vv = (\(a,b,c)->Color3 (doubleToGF a) (doubleToGF b) (doubleToGF c)) $ case truncate h::Int of
	0->(0,v-hf,v)
	1->(hf,0,v)
	2->(v,0,v-hf)
	3->(v,hf,0)
	4->(v-hf,v,0)
	5->(0,v,hf)
	_->if isNaN h then (0.5,0.5,0.5)
		else if isInfinite h then (0.75,0.75,0.75)
		else (0.25,0.25,0.25)
	where
		v=min vv 1
		--v=vv --Fast. Good enough for Newtons, not so much for Complexes
		--v=abs $ significand vv --Discontinuities at pretty intervals. Good for Complexes, not so much for Newtons
		h=3+phase hc*3/pi
		hf=v*(h-fromIntegral (truncate h::Int))

magsqr,magnitude :: Complex Double -> Double
magsqr (a:+b) = a*a+b*b
magnitude = sqrt . magsqr

complex :: (Complex Double -> Complex Double) -> Int -> Complex Double -> Color3 GLfloat
newton :: (Complex Double -> Complex Double) -> (Complex Double -> Complex Double) -> Int -> Complex Double -> Color3 GLfloat
tricorn,burningship,nodoub,yxmandel,dagger,mandel :: Int -> Complex Double -> Color3 GLfloat
julia :: Complex Double -> Int -> Complex Double -> Color3 GLfloat
multibrot :: Int -> Int -> Complex Double -> Color3 GLfloat
complex !f z xy = hvrgb (f xy) $ case z of
	0->1
	1->magnitude $ f xy
	_->logBase (fromIntegral z) $ magnitude (f xy)+1
newton !f !g z xy = hvrgb x $ fromIntegral zz/fromIntegral z
	where
		newraph 0 _ = (0,0)
		newraph m x = if magsqr(f x)<=1/fromIntegral m then (x,m) else newraph (m-1) (x-f x/g x)
		(x,zz)=newraph z xy
julia x zz xx = f zz xx
	where f z !xx
		|z==0 = Color3 0 0 0
		|magsqr xx<4 = f (z-1) (xx*xx+x)
		|True = Color3 (cub $ fromIntegral z/fromIntegral zz) (sqr $ fromIntegral z/fromIntegral zz) (fromIntegral z/fromIntegral zz)
mandel ii x = f ii x
	where f i !z
		|i==0 = Color3 0 0 0
		|magsqr z<4 = f (i-1) (z*z+x)
		|True = Color3 (cub $ fromIntegral i/fromIntegral ii) (sqr $ fromIntegral i/fromIntegral ii) (fromIntegral i/fromIntegral ii)
dagger zz (x:+y) = f zz x y
	where f z zr zi
		|z==0 = Color3 0 0 0
		|zr*zr*zi*zi<4 = f (z-1) (zr*zr-zi*zi+cos (atan2 y x)) (2*zr*zi+sin (atan2 y x))
		|True = Color3 (cub $ fromIntegral z/fromIntegral zz) (sqr $ fromIntegral z/fromIntegral zz) (fromIntegral z/fromIntegral zz)
multibrot ex ii xy = f ii xy
	where f i z
		|i==0 = Color3 0 0 0
		|magsqr z<4 = f (i-1) (z^ex+xy)
		|True = Color3 (cub $ fromIntegral i/fromIntegral ii) (sqr $ fromIntegral i/fromIntegral ii) (fromIntegral i/fromIntegral ii)
yxmandel zz (x:+y) = f zz x y
	where f z !zr !zi
		|z==0 = Color3 0 0 0
		|zr*zr+zi*zi<4 = f (z-1) (zr*zr-zi*zi+y*x) (2*zr*zi+y)
		|True = Color3 (cub $ fromIntegral z/fromIntegral zz) (sqr $ fromIntegral z/fromIntegral zz) (fromIntegral z/fromIntegral zz)
nodoub zz (x:+y) = f zz x y
	where f z !zr !zi
		|z==0 = Color3 0 0 0
		|zr*zr+zi*zi<4 = f (z-1) (zr*zr-zi*zi+x) (zr*zi+y)
		|True = Color3 (cub $ fromIntegral z/fromIntegral zz) (sqr $ fromIntegral z/fromIntegral zz) (fromIntegral z/fromIntegral zz)
tricorn zz (x:+y) = f zz x y
	where f z !zr !zi
		|z==0 = Color3 0 0 0
		|zr*zr+zi*zi<4 = f (z-1) (zi*zi-zr*zr+x) (2*zr*zi+y)
		|True = Color3 (cub $ fromIntegral z/fromIntegral zz) (sqr $ fromIntegral z/fromIntegral zz) (fromIntegral z/fromIntegral zz)
burningship zz (x:+y) = f zz x y
	where f z !zr !zi
		|z==0 = Color3 0 0 0
		|zr*zr+zi*zi<4 = f (z-1) (zi*zi-zr*zr+x) (2*abs (zr*zi)+y)
		|True = Color3 (cub $ fromIntegral z/fromIntegral zz) (sqr $ fromIntegral z/fromIntegral zz) (fromIntegral z/fromIntegral zz)
