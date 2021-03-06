{-# LANGUAGE BangPatterns#-}
{-# OPTIONS -fexcess-precision -funbox-strict-fields -feager-blackholing -O2#-}
module Templates(Color3,FractalCb,complex,collatz,newton,multibrot,julia,tricorn,burningship,nodoub,yxmandel,dagger,mandel,magnitude,magsqr,sicarp,sitabl,cantor,cantox,metaball) where
import Data.Complex hiding (magnitude)
import Numeric.FastMath
import GHC.Float(int2Double,double2Int,significand,isNaN,isInfinite)
type Color3 = (Double, Double, Double)
type FractalCb = Int -> Complex Double -> Color3
sqr !x=x*x
cub !x=x*x*x
magsqr,magnitude :: Complex Double -> Double
magsqr (a:+b) = a*a+b*b
magnitude = sqrt . magsqr
hvrgb :: Complex Double -> Double -> Color3
hvrgb hc vv = case double2Int h of
	0->(0,v-hf,v)
	1->(hf,0,v)
	2->(v,0,v-hf)
	3->(v,hf,0)
	4->(v-hf,v,0)
	5->(0,v,hf)
	_->let hv = if isNaN h then 0.5 else if isInfinite h then 0.75 else 0.25
		in (hv,hv,hv)
	where
		v=min vv 1
		--v=vv --Fast. Good enough for Newtons, not so much for Complexes
		--v=abs $ significand vv --Discontinuities at pretty intervals. Good for Complexes, not so much for Newtons
		h=3+phase hc*3/pi
		hf=v*(h-(int2Double.double2Int)h)
bluegrad :: Int -> Int -> Color3
bluegrad' :: Double -> Color3
bluegrad z zz = bluegrad' $ fromIntegral z/fromIntegral zz
bluegrad' !x = (cub x, sqr x, x)
complex :: (Complex Double -> Complex Double) -> FractalCb
newton :: (Complex Double -> Complex Double) -> (Complex Double -> Complex Double) -> FractalCb
cantor,sicarp,sitabl,collatz,tricorn,burningship,nodoub,yxmandel,dagger,mandel :: FractalCb
julia :: Complex Double -> FractalCb
multibrot :: Int -> FractalCb
metaball :: [(Complex Double, Double)] -> FractalCb
complex !f z xy = hvrgb (f xy) $ case z of
	_->logBase (fromIntegral (z+1)) $ magnitude (f xy)+1
newton !f !g z xy = hvrgb x $ fromIntegral zz/fromIntegral z
	where
		newraph 0 _ = (0,0)
		newraph m x = if magsqr(f x)<=1/fromIntegral m then (x,m) else newraph (m-1) (x-f x/g x)
		(x,zz)=newraph z xy
julia x zz xx = f zz xx
	where f z !xx
		|z==0 = (0,0,0)
		|magsqr xx<4 = f (z-1) (xx*xx+x)
		|True = bluegrad z zz
mandel ii x = f ii x
	where f i !z
		|i==0 = (0,0,0)
		|magsqr z<4 = f (i-1) (z*z+x)
		|True = bluegrad i ii
dagger zz (x:+y) = f zz x y
	where f z zr zi
		|z==0 = (0,0,0)
		|zr*zr*zi*zi<4 = f (z-1) (zr*zr-zi*zi+cos (atan2 y x)) (2*zr*zi+sin (atan2 y x))
		|True = bluegrad z zz
multibrot ex ii xy = f ii xy
	where f i z
		|i==0 = (0,0,0)
		|magsqr z<4 = f (i-1) (z^ex+xy)
		|True = bluegrad i ii
yxmandel zz (x:+y) = f zz x y
	where f z !zr !zi
		|z==0 = (0,0,0)
		|zr*zr+zi*zi<4 = f (z-1) (zr*zr-zi*zi+y*x) (2*zr*zi+y)
		|True = bluegrad z zz
nodoub zz (x:+y) = f zz x y
	where f z !zr !zi
		|z==0 = (0,0,0)
		|zr*zr+zi*zi<4 = f (z-1) (zr*zr-zi*zi+x) (zr*zi+y)
		|True = bluegrad z zz
tricorn zz (x:+y) = f zz x y
	where f z !zr !zi
		|z==0 = (0,0,0)
		|zr*zr+zi*zi<4 = f (z-1) (zi*zi-zr*zr+x) (2*zr*zi+y)
		|True = bluegrad z zz
burningship zz (x:+y) = f zz x y
	where f z !zr !zi
		|z==0 = (0,0,0)
		|zr*zr+zi*zi<4 = f (z-1) (zi*zi-zr*zr+x) (2*abs(zr*zi)+y)
		|True = bluegrad z zz
collatz zz xy = f zz xy
	where f z !x
		|z==0 = (0,0,0)
		|magsqr x<1/0 = f (z-1) $ 0.25+x-(cos $ x*pi)*(0.25+x*0.5)
		|True = bluegrad z zz
sicarp zz (x:+y) = f (zz+1) 0.0 1.0 0.0 1.0
	where f i !x1 !x2 !y1 !y2
		|i==0 = (0,0,0)
		|x<x13 && y<y13 = fi x1 x13 y1 y13
		|x<x13 && y>y23 = fi x1 x13 y23 y2
		|x>x23 && y<y13 = fi x23 x2 y1 y13
		|x>x23 && y>y23 = fi x23 x2 y23 y2
		|x>x13 && x<x23 && y<y13 = fi x13 x23 y1 y13
		|x>x13 && x<x23 && y>y23 = fi x13 x23 y23 y2
		|y>y13 && y<y23 && x<x13 = fi x1 x13 y13 y23
		|y>y13 && y<y23 && x>x23 = fi x23 x2 y13 y23
		|True = (1,1,1)
		where
			x3=(x2-x1)/3
			y3=(y2-y1)/3
			x13=x1+x3
			x23=x2-x3
			y13=y1+y3
			y23=y2-y3
			fi=f (i-1)
sitabl zz (x:+y) = f (zz+1) True 0.0 1.0 0.0 1.0
	where f i !h !x1 !x2 !y1 !y2
		|i==0 = if h then (0,0,0) else (1,1,1)
		|x<x13 && y<y13 = fi x1 x13 y1 y13
		|x<x13 && y>y23 = fi x1 x13 y23 y2
		|x>x23 && y<y13 = fi x23 x2 y1 y13
		|x>x23 && y>y23 = fi x23 x2 y23 y2
		|x>x13 && x<x23 && y<y13 = fi x13 x23 y1 y13
		|x>x13 && x<x23 && y>y23 = fi x13 x23 y23 y2
		|y>y13 && y<y23 && x<x13 = fi x1 x13 y13 y23
		|y>y13 && y<y23 && x>x23 = fi x23 x2 y13 y23
		|True = f (i-1) (not h) x13 x23 y13 y23
		where
			x3=(x2-x1)/3
			y3=(y2-y1)/3
			x13=x1+x3
			x23=x2-x3
			y13=y1+y3
			y23=y2-y3
			fi=f (i-1) h
cantor zz (x:+y) = f (zz+1) True 0.0 1.0 0.0 1.0
	where f i !h !x1 !x2 !y1 !y2
		|i==0 = if h then (0,0,0) else (1,1,1)
		|x<x13 && y<y13 = f1 x1 x13 y1 y13
		|x<x13 && y>y23 = f1 x1 x13 y23 y2
		|x>x23 && y<y13 = f1 x23 x2 y1 y13
		|x>x23 && y>y23 = f1 x23 x2 y23 y2
		|x>x13 && x<x23 && y<y13 = f0 x13 x23 y1 y13
		|x>x13 && x<x23 && y>y23 = f0 x13 x23 y23 y2
		|y>y13 && y<y23 && x<x13 = f0 x1 x13 y13 y23
		|y>y13 && y<y23 && x>x23 = f0 x23 x2 y13 y23
		|True = f0 x13 x23 y13 y23
		where
			x3=(x2-x1)/3
			y3=(y2-y1)/3
			x13=x1+x3
			x23=x2-x3
			y13=y1+y3
			y23=y2-y3
			f0=f (i-1) False
			f1=f (i-1) h
cantox zz (x:+y) = f (zz+1) True 0.0 1.0 0.0 1.0
	where f i !h !x1 !x2 !y1 !y2
		|i==0 = if h then (0,0,0) else (1,1,1)
		|x<x13 && y<y13 = f1 x1 x13 y1 y13
		|x<x13 && y>y23 = f1 x1 x13 y23 y2
		|x>x23 && y<y13 = f1 x23 x2 y1 y13
		|x>x23 && y>y23 = f1 x23 x2 y23 y2
		|x>x13 && x<x23 && y<y13 = f0 x13 x23 y1 y13
		|x>x13 && x<x23 && y>y23 = f0 x13 x23 y23 y2
		|y>y13 && y<y23 && x<x13 = f0 x1 x13 y13 y23
		|y>y13 && y<y23 && x>x23 = f0 x23 x2 y13 y23
		|True = f1 x13 x23 y13 y23
		where
			x3=(x2-x1)/3
			y3=(y2-y1)/3
			x13=x1+x3
			x23=x2-x3
			y13=y1+y3
			y23=y2-y3
			f0=f (i-1) False
			f1=f (i-1) h
metaball bs zz xy = bluegrad' $ (f bs xy 0)/(fromIntegral zz)
	where
		f [] !_ !a = a
		f ((b,t):bs) xy a = f bs xy (a+t/(sqrt $ magnitude $ xy-b))