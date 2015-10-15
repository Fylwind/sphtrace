import Data.Complex
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

data Vec a = Vec { vecx, vecy, vecz :: !a }

instance Functor Vec where
   fmap f (Vec x y z) = Vec (f x) (f y) (f z)

instance Applicative Vec where
   pure x = Vec x x x
   (Vec f g h) <*> (Vec x y z) = Vec (f x) (g y) (h z)

instance Foldable Vec where
   foldr f a (Vec x y z) = f x $ f y $ f z a
   foldl1 f (Vec x y z) = f z $ f y x

instance Num a => Num (Vec a) where
   u + v = (+) <$> u <*> v
   u * v = (*) <$> u <*> v
   u - v = (-) <$> u <*> v
   abs v = abs <$> v
   signum v = signum <$> v
   fromInteger i = pure $ fromInteger i

dot :: Num a => Vec a -> Vec a -> a
dot u v = foldl1 (+) $ u * v

cross :: Num a => Vec a -> Vec a -> Vec a
cross (Vec ux uy uz) (Vec vx vy vz) = Vec (uy * vz - uz * vy) (uz * vx - ux * vz) (ux * vy - uy * vx)

normsq :: Num a => Vec a -> a
normsq v = v `dot` v

norm :: Floating a => Vec a -> a
norm v = sqrt $ normsq v

normalize :: Floating a => Vec a -> Vec a
normalize v = fmap (/ len) v
   where len = norm v

transpose :: Vec a -> Vec a -> Vec a -> (Vec a, Vec a, Vec a)
transpose u v w = (x, y, z)
   where x = Vec (vecx u) (vecx v) (vecx w)
         y = Vec (vecy u) (vecy v) (vecy w)
         z = Vec (vecz u) (vecz v) (vecz w)

data Camera a = Cam {
   cam_size :: (Int, Int),
   cam_pos :: Vec a,
   cam_scale :: (a, a),
   cam_aspect, cam_cz :: a,
   cam_dir :: (Vec a, Vec a, Vec a)
}

calc_camera :: Floating a => Int -> Int -> a -> Vec a -> Vec a -> Vec a -> Camera a
calc_camera w h fov pos center up = Cam (w, h) pos (scale_x, scale_y) aspect cz (transpose di dj dk)
   where (w', h') = (fromIntegral w, fromIntegral h)
         (scale_x, scale_y) = (2 / (w' - 1), 2 / (h' - 1))
         aspect = h' / w'
         cz = 1 / tan(fov * pi / 360)
         dk = normalize $ center - pos
         di = normalize $ dk `cross` up
         dj = di `cross` dk

trace_ray :: RealFloat a => (Vec a -> a) -> Int -> Int -> a -> Camera a -> Maybe (Vec a)
trace_ray dist_fn sx sy max_t cam = ray_loop 0
   where pos = cam_pos cam
         (scale_x, scale_y) = cam_scale cam
         (dir_x, dir_y, dir_z) = cam_dir cam
         cx = fromIntegral sx * scale_x - 1
         cy = (1 - fromIntegral sy * scale_y) * (cam_aspect cam)
         c = Vec cx cy (cam_cz cam)
         m = normalize $ Vec (dir_x `dot` c) (dir_y `dot` c) (dir_z `dot` c)
         ray_loop t
            | t > max_t = Nothing
            | d < eps = Just r
            | otherwise = ray_loop (t + d)
            where r = fmap (*t) m + pos
                  d = dist_fn r
                  eps = 0.001

data Light a = Light { light_pos :: Vec a, light_val :: a }

trace_shadow :: RealFloat a => (Vec a -> a) -> Light a -> Vec a -> a -> Maybe a
trace_shadow dist_fn light pos k = ray_loop 0.02 1
   where lpos = light_pos light
         dp = lpos - pos
         dist = norm dp
         m = (/ dist) <$> dp
         ray_loop t acc
            | t > dist = Just s
            | d < eps = Nothing
            | otherwise = ray_loop (t + d) s
            where r = fmap (*t) m + pos
                  d = dist_fn r
                  s = min acc $ k * d / t
                  eps = 0.001

calc_diffuse :: RealFloat a => Light a -> Vec a -> Vec a -> a
calc_diffuse light pos snorm = col * il * int / dsq
   where col = light_val light
         d = light_pos light - pos
         dsq = normsq d
         int = max 0 $ snorm `dot` d
         il = 1 / sqrt dsq

gradient :: Floating a => (Vec a -> a) -> Vec a -> Vec a
gradient f p = normalize $ Vec dx dy dz
   where dx = f (p + epsx) - f (p - epsx)
         dy = f (p + epsy) - f (p - epsy)
         dz = f (p + epsz) - f (p - epsz)
         (epsx, epsy, epsz) = (Vec eps 0 0, Vec 0 eps 0, Vec 0 0 eps)
         eps = 1e-6

data Image a = Image Int Int [a]

render :: RealFloat a => (Vec a -> a) -> Camera a -> Light a -> a -> a -> Image a
render scene cam light ambient max_t = Image w h [pixel x y | y <- [0 .. h-1], x <- [0 .. w-1]]
   where (w, h) = cam_size cam
         pixel sx sy = clamp 0 1 $ maybe 0 color $ trace_ray scene sx sy max_t cam
            where clamp mn mx = max mn . min mx
                  color hit_pos = ambient + maybe 0 (*diffuse) shadow
                     where diffuse = calc_diffuse light hit_pos $ gradient scene hit_pos
                           shadow = trace_shadow scene light hit_pos 8

to_ppm :: RealFloat a => Image a -> B.ByteString
to_ppm (Image w h pixels) = B.append header img_bin
   where header = C.pack $ "P5\n" ++ show w ++ " " ++ show h ++ "\n255\n"
         img_bin = B.pack $ map color_val pixels
         color_val x = truncate $ x * 255

-- Scene definition
translate :: RealFloat a => (a, a, a) -> (Vec a -> a) -> Vec a -> a
translate (dx, dy, dz) f p = f (p - Vec dx dy dz)

mandel_dist :: RealFloat a => Int -> Complex a -> a
mandel_dist max_it c = mandel_loop 0 1 0
   where mandel_loop z dz it
            | it >= max_it = 0
            | zsq > 1024 = dist
            | otherwise = mandel_loop z' dz' (it + 1)
            where z' = z * z + c
                  dz' = z * dz * 2 + 1
                  zsq = sqr z'
                  dist = sqrt (zsq / sqr dz') * log zsq * 0.5
                  sqr (re :+ im) = re * re + im * im

scene :: RealFloat a => Vec a -> a
scene p = minimum [translate (2, -0.5, 0) (sphere 0.6) p,
                   translate (3.2, 0, 0.5) (cone (pi/6)) p,
                   translate (-1, 0, 0) (torus 1.5 0.3) p,
                   max (box (Vec 1 1 1) p) (-sphere 1.3 p),
                   max (translate (0, -1, 0) (cylinder_xy 0.6) p) (translate (0, -1, 2.5) (cylinder_yz 0.6) p),
                   max (translate (2.1, 0, 1.5) mandel p) (plane (Vec 0 1 0) 0.6 p),
                   plane (Vec 1 0 0) 2.1 p, plane (Vec 0 1 0) 1 p, plane (Vec 0 0 1) 1.5 p]
   where sphere r p = norm p - r
         torus r1 r2 (Vec x y z) = let q = sqrt (x*x + z*z) - r1 in sqrt (q*q + y*y) - r2
         plane n b p = p `dot` n + b
         cone t (Vec x y z) = let q = sqrt (x*x + z*z) in q * cos(t) + y * sin(t)
         box b p = let d = abs p - b in min 0 (maximum d) + norm (fmap (max 0) d)
         cylinder_xy r (Vec x y _) = sqrt (x*x + y*y) - r
         cylinder_yz r (Vec _ y z) = sqrt (y*y + z*z) - r
         mandel (Vec re _ im) = mandel_dist 256 (re :+ im)

main :: IO ()
main = B.writeFile "out.ppm" $ to_ppm $ render scene cam light 0.03 42
   where cam = calc_camera 640 480 60 (Vec 3 3 5) (Vec 0 (-1) 0) (Vec 0 1 0)
         light = Light (Vec 3 2 2) 7
