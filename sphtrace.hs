{-# LANGUAGE BangPatterns #-}
import Data.Complex
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

data Vec = Vec { vecx, vecy, vecz :: {-# UNPACK #-} !Double }

{-# INLINE liftBinary #-}
liftBinary :: (Double -> Double -> Double) -> Vec -> Vec -> Vec
liftBinary f !(Vec ux uy uz) !(Vec vx vy vz) = Vec (f ux vx) (f uy vy) (f uz vz)

{-# INLINE liftUnary #-}
liftUnary :: (Double -> Double) -> Vec -> Vec
liftUnary f !(Vec ux uy uz) = Vec (f ux) (f uy) (f uz)

{-# INLINE foldlVec #-}
foldlVec :: (Double -> Double -> Double) -> Vec -> Double
foldlVec f !(Vec ux uy uz) = ux `f` uy `f` uz

instance Num Vec where
   (+) = liftBinary (+)
   (*) = liftBinary (*)
   (-) = liftBinary (-)
   abs = liftUnary abs
   signum = liftUnary signum
   fromInteger i = undefined

dot :: Vec -> Vec -> Double
dot !u !v = foldlVec (+) (u * v)

scalarMul :: Double -> Vec -> Vec
scalarMul !c !u = liftUnary (c *) u

vecMax !(Vec x y z) = maximum [x, y, z]

cross :: Vec -> Vec -> Vec
cross !(Vec ux uy uz) !(Vec vx vy vz) = Vec (uy * vz - uz * vy) (uz * vx - ux * vz) (ux * vy - uy * vx)

normsq :: Vec -> Double
normsq !v = v `dot` v

norm :: Vec -> Double
norm !v = sqrt $ normsq v

normalize :: Vec -> Vec
normalize !v = scalarMul (1 / len) v
   where len = norm v

transpose :: Vec -> Vec -> Vec -> (Vec, Vec, Vec)
transpose !u !v !w = (x, y, z)
   where !x = Vec (vecx u) (vecx v) (vecx w)
         !y = Vec (vecy u) (vecy v) (vecy w)
         !z = Vec (vecz u) (vecz v) (vecz w)

data Camera = Cam {
   cam_size :: (Int, Int),
   cam_pos :: Vec,
   cam_scale :: (Double, Double),
   cam_aspect, cam_cz :: Double,
   cam_dir :: (Vec, Vec, Vec)
}

calc_camera :: Int -> Int -> Double -> Vec -> Vec -> Vec -> Camera
calc_camera w h fov pos center up = Cam (w, h) pos (scale_x, scale_y) aspect cz (transpose di dj dk)
   where (!w', !h') = (fromIntegral w, fromIntegral h)
         (!scale_x, !scale_y) = (2 / (w' - 1), 2 / (h' - 1))
         !aspect = h' / w'
         !cz = 1 / tan(fov * pi / 360)
         !dk = normalize $ center - pos
         !di = normalize $ dk `cross` up
         !dj = di `cross` dk

trace_ray :: (Vec -> Double) -> Int -> Int -> Double -> Camera -> Maybe Vec
trace_ray dist_fn sx sy max_t cam = ray_loop 0
   where !pos = cam_pos cam
         (!scale_x, !scale_y) = cam_scale cam
         (!dir_x, !dir_y, !dir_z) = cam_dir cam
         !cx = fromIntegral sx * scale_x - 1
         !cy = (1 - fromIntegral sy * scale_y) * (cam_aspect cam)
         !c = Vec cx cy (cam_cz cam)
         !m = normalize $ Vec (dir_x `dot` c) (dir_y `dot` c) (dir_z `dot` c)
         ray_loop !t
            | t > max_t = Nothing
            | d < eps = Just r
            | otherwise = ray_loop (t + d)
            where !r = scalarMul t m + pos
                  !d = dist_fn r
                  !eps = 0.001

data Light = Light { light_pos :: !Vec, light_val :: !Double }

trace_shadow :: (Vec -> Double) -> Light -> Vec -> Double -> Maybe Double
trace_shadow !dist_fn !light !pos !k = ray_loop 0.02 1
   where !lpos = light_pos light
         !dp = lpos - pos
         !dist = norm dp
         !m = scalarMul (1 / dist) dp
         ray_loop !t !acc
            | t > dist = Just s
            | d < eps = Nothing
            | otherwise = (ray_loop $! (t + d)) $! s
            where !r = scalarMul t m + pos
                  !d = dist_fn r
                  !s = min acc $ k * d / t
                  !eps = 0.001

calc_diffuse :: Light -> Vec -> Vec -> Double
calc_diffuse !light !pos !snorm = col * il * int / dsq
   where !col = light_val light
         !d = light_pos light - pos
         !dsq = normsq d
         !int = max 0 $ snorm `dot` d
         !il = 1 / sqrt dsq

gradient :: (Vec -> Double) -> Vec -> Vec
gradient !f !p = normalize $ Vec dx dy dz
   where !dx = f (p + epsx) - f (p - epsx)
         !dy = f (p + epsy) - f (p - epsy)
         !dz = f (p + epsz) - f (p - epsz)
         (!epsx, !epsy, !epsz) = (Vec eps 0 0, Vec 0 eps 0, Vec 0 0 eps)
         !eps = 1e-6

data Image a = Image !Int !Int [a]

render :: (Vec -> Double) -> Camera -> Light -> Double -> Double -> Image Double
render scene !cam !light !ambient !max_t = Image w h [pixel x y | y <- [0 .. h-1], x <- [0 .. w-1]]
   where (!w, !h) = cam_size cam
         pixel !sx !sy = clamp 0 1 $ maybe 0 color $ trace_ray scene sx sy max_t cam
            where color !hit_pos = ambient + maybe 0 (*diffuse) shadow
                     where diffuse = calc_diffuse light hit_pos $ gradient scene hit_pos
                           shadow = trace_shadow scene light hit_pos 8

clamp !mn !mx z = max mn (min mx z)

to_ppm :: RealFloat a => Image a -> B.ByteString
to_ppm !(Image w h pixels) = B.append header img_bin
   where header = C.pack $ "P5\n" ++ show w ++ " " ++ show h ++ "\n255\n"
         img_bin = B.pack $ map color_val pixels
         color_val x = truncate $ x * 255

-- Scene definition
translate :: (Double, Double, Double) -> (Vec -> Double) -> Vec -> Double
translate (!dx, !dy, !dz) f !p = f (p - Vec dx dy dz)

mandel_dist :: Int -> Double -> Double -> Double
mandel_dist !max_it !cr !ci = mandel_loop 0 1 0
   where mandel_loop !z !dz it
            | it >= max_it = 0
            | zsq > 1024 = sqrt (zsq / sqr dz') * log zsq * 0.5
            | otherwise = mandel_loop z' dz' (it + 1)
           where !zsq = sqr z'
                 !dz' = z * dz * 2 + 1
                 !z' = z * z + c
         c = cr :+ ci

sqr :: Complex Double -> Double
sqr (re :+ im) = re * re + im * im

scene :: Vec -> Double
scene !p =
   let !u0 = translate (2, -0.5, 0) (sphere 0.6) p
       !u1 = translate (3.2, 0, 0.5) (cone (pi/6)) p
       !u2 = translate (-1, 0, 0) (torus 1.5 0.3) p
       !u3 = max (box (Vec 1 1 1) p) (-sphere 1.3 p)
       !u4 = max (translate (0, -1, 0) (cylinder_xy 0.6) p) (translate (0, -1, 2.5) (cylinder_yz 0.6) p)
       !u5 = max (translate (2.1, 0, 1.5) mandel p) (plane (Vec 0 1 0) 0.6 p)
       !u6 = plane (Vec 1 0 0) 2.1 p
       !u7 = plane (Vec 0 1 0) 1 p
       !u8 = plane (Vec 0 0 1) 1.5 p
   in minimum [u0, u1, u2, u3, u4, u5, u6, u7, u8]
   where sphere !r !p = norm p - r
         torus !r1 !r2 !(Vec x y z) = let !q = sqrt (x*x + z*z) - r1 in sqrt (q*q + y*y) - r2
         plane !n !b !p = p `dot` n + b
         cone !t !(Vec x y z) = let !q = sqrt (x*x + z*z) in q * cos(t) + y * sin(t)
         box !b !p = let !d = abs p - b in min 0 (vecMax d) + norm (liftUnary (max 0) d)
         cylinder_xy !r !(Vec x y _) = sqrt (x*x + y*y) - r
         cylinder_yz !r !(Vec _ y z) = sqrt (y*y + z*z) - r
         mandel !(Vec re _ im) = mandel_dist 256 re im

main :: IO ()
main = B.writeFile "dist/sphtrace-hs.ppm" $ to_ppm $ render scene cam light 0.03 42
   where cam = calc_camera 640 480 60 (Vec 3 3 5) (Vec 0 (-1) 0) (Vec 0 1 0)
         light = Light (Vec 3 2 2) 7
