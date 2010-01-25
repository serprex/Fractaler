{-# LANGUAGE BangPatterns#-}
{-# OPTIONS -fexcess-precision -funbox-strict-fields -feager-blackholing -O2#-}
module Templates where
import Graphics.UI.GLUT
import Data.Complex hiding (magnitude)
import GHC.Float(double2Float)
import Unsafe.Coerce(unsafeCoerce)
doubleToGF :: Double -> GLfloat
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
newton :: (Complex Double -> Complex Double) -> (Complex Double -> Complex Double) -> Int -> Complex Double -> Color3 GLfloat
tricorn,burningship,nodoub,yxmandel,dagger,mandel :: Int -> Complex Double -> Color3 GLfloat
multibrot,julia :: Complex Double -> Int -> Complex Double -> Color3 GLfloat
complex f z xy = hvrgb (f xy) $ case z of
	0->1
	1->magnitude $ f xy
	_->logBase (fromIntegral z) $ magnitude (f xy)+1
newton f g z xy = hvrgb (x:+y) $ fromIntegral zz/fromIntegral z
	where
		newraph f g m x = if m==0 then (x,0) else if magsqr(f x)<1/fromIntegral m then (x,m) else newraph f g (m-1) (x-f x/g x)
		(x:+y,zz)=newraph f g z xy
julia (x:+y) zz (xx:+yy) = f zz xx yy
	where f z zr zi
		|z==0 = Color3 0 0 0
		|zr*zr*zi*zi<4 = f (z-1) (zr*zr-zi*zi+x) (2*zr*zi+y)
		|True = Color3 ((fromIntegral z/fromIntegral zz)^3) ((fromIntegral z/fromIntegral zz)^2) (fromIntegral z/fromIntegral zz)
mandel ii (x:+y) = f ii x y
	where f i !zr !zi
		|i==0 = Color3 0 0 0
		|zr*zr+zi*zi<4 = f (i-1) (zr*zr-zi*zi+x) (2*zr*zi+y)
		|True = Color3 ((fromIntegral i/fromIntegral ii)^3) ((fromIntegral i/fromIntegral ii)^2) (fromIntegral i/fromIntegral ii)
dagger zz (x:+y) = f zz x y
	where f z zr zi
		|z==0 = Color3 0 0 0
		|zr*zr*zi*zi<4 = f (z-1) (zr*zr-zi*zi+cos (atan2 y x)) (2*zr*zi+sin (atan2 y x))
		|True = Color3 ((fromIntegral z/fromIntegral zz)^3) ((fromIntegral z/fromIntegral zz)^2) (fromIntegral z/fromIntegral zz)
multibrot ex ii xy = f ii xy
	where f i z
		|i==0 = Color3 0 0 0
		|magsqr z<4 = f (i-1) (z**ex+xy)
		|True = Color3 ((fromIntegral i/fromIntegral ii)^3) ((fromIntegral i/fromIntegral ii)^2) (fromIntegral i/fromIntegral ii)
yxmandel zz (x:+y) = f zz x y
	where f z !zr !zi
		|z==0 = Color3 0 0 0
		|zr*zr+zi*zi<4 = f (z-1) (zr*zr-zi*zi+y*x) (2*zr*zi+y)
		|True = Color3 ((fromIntegral z/fromIntegral zz)^3) ((fromIntegral z/fromIntegral zz)^2) (fromIntegral z/fromIntegral zz)
nodoub zz (x:+y) = f zz x y
	where f z !zr !zi
		|z==0 = Color3 0 0 0
		|zr*zr+zi*zi<4 = f (z-1) (zr*zr-zi*zi+x) (zr*zi+y)
		|True = Color3 ((fromIntegral z/fromIntegral zz)^3) ((fromIntegral z/fromIntegral zz)^2) (fromIntegral z/fromIntegral zz)
tricorn zz (x:+y) = f zz x y
	where f z !zr !zi
		|z==0 = Color3 0 0 0
		|zr*zr+zi*zi<4 = f (z-1) (zi*zi-zr*zr+x) (2*zr*zi+y)
		|True = Color3 ((fromIntegral z/fromIntegral zz)^3) ((fromIntegral z/fromIntegral zz)^2) (fromIntegral z/fromIntegral zz)
burningship zz (x:+y) = f zz x y
	where f z !zr !zi
		|z==0 = Color3 0 0 0
		|zr*zr+zi*zi<4 = f (z-1) (zi*zi-zr*zr+x) (2*abs (zr*zi)+y)
		|True = Color3 ((fromIntegral z/fromIntegral zz)^3) ((fromIntegral z/fromIntegral zz)^2) (fromIntegral z/fromIntegral zz)
