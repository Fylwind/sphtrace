extern crate num;
extern crate nalgebra as na;

use std::io::{self, Write};
use std::fs::File;
use std::f64::consts;
use num::complex::Complex64;
use num::{Zero, One};
use na::{Vec2, Vec3, Mat3, Dot, Cross, Norm};

type Vec3f = Vec3<f64>;
type Mat3f = Mat3<f64>;

struct Camera
{
    size: Vec2<i32>,
    pos: Vec3f,
    scale: Vec3f,
    aspect: f64,
    dir: Mat3f,
}

impl Camera
{
    fn new(width: i32, height: i32, fov: f64, pos: Vec3f, center: Vec3f, up: Vec3f) -> Camera
    {
        let (w, h) = (width as f64, height as f64);
        let cz = 1. / (fov * consts::PI / 360.).tan();
        let dk = (center - pos).normalize();
        let di = dk.cross(&up).normalize();
        let dj = di.cross(&dk);
        Camera{
            size: Vec2::new(width, height),
            pos: pos,
            scale: Vec3::new(2. / (w - 1.), 2. / (h - 1.), cz),
            aspect: h / w,
            dir: Mat3::new(di.x, dj.x, dk.x,
                           di.y, dj.y, dk.y,
                           di.z, dj.z, dk.z),
        }
    }

    fn trace_ray<F>(&self, dist_fn: &F, sx: i32, sy: i32, max_t: f64) -> Option<Vec3f>
        where F: Fn(Vec3f) -> f64
    {
        let xy = Vec3::new(sx as f64, sy as f64, 1.);
        let c = (xy * self.scale - Vec3::new(1., 1., 0.)) * Vec3::new(1., -self.aspect, 1.);
        let m = (self.dir * c).normalize();
        let eps = 0.001;
        let mut t = 0.;
        while t < max_t
        {
            let r = m * t + self.pos;
            let d = dist_fn(r);
            if d < eps { return Some(r) }
            t += d;
        }
        None
    }
}

struct Light
{
    pos: Vec3f,
    val: f64,
}

impl Light
{
    fn new(p: Vec3f, v: f64) -> Light
    {
        Light{ pos: p, val: v }
    }

    fn calc_diffuse(&self, pos: Vec3f, snorm: Vec3f) -> f64
    {
        let d = self.pos - pos;
        let dsq = d.sqnorm();
        let il = 1. / dsq.sqrt();
        self.val * il * snorm.dot(&d).max(0.) / dsq
    }

    fn trace_shadow<F>(&self, dist_fn: &F, pos: Vec3f, k: f64) -> Option<f64>
        where F: Fn(Vec3f) -> f64
    {
        let dp = self.pos - pos;
        let dist = dp.norm();
        let m = dp / dist;
        let eps = 0.001;
        let mut s = 1.0_f64;
        let mut t = 0.02;
        while t < dist
        {
            let r = m * t + pos;
            let d = dist_fn(r);
            s = s.min(k * d / t);
            if d < eps { return None }
            t += d;
        }
        Some(s)
    }
}

struct Image
{
    width: i32,
    height: i32,
    pixels: Vec<u8>,
}

impl Image
{
    fn new(w: i32, h: i32, p: Vec<u8>) -> Image
    {
        Image{ width: w, height: h, pixels: p }
    }

    fn write_ppm<F: Write>(&self, mut file: F) -> io::Result<()>
    {
        try!(write!(file, "P5\n{} {}\n{}\n", self.width, self.height, std::u8::MAX));
        file.write_all(&self.pixels)
    }
}

fn gradient<F>(f: &F, p: Vec3f) -> Vec3f
    where F: Fn(Vec3f) -> f64
{
    let eps = 1e-6;
    let (epsx, epsy, epsz) = (Vec3::x() * eps, Vec3::y() * eps, Vec3::z() * eps);
    Vec3::new(f(p + epsx) - f(p - epsx),
              f(p + epsy) - f(p - epsy),
              f(p + epsz) - f(p - epsz)).normalize()
}

fn render<F>(scene: F, cam: &Camera, light: &Light, ambient: f64, max_t: f64) -> Image
    where F: Fn(Vec3f) -> f64
{
    let mut buf = Vec::new();
    for y in 0 .. cam.size.y
    {
        for x in 0 .. cam.size.x
        {
            let color = cam.trace_ray(&scene, x, y, max_t).map(|hit_pos| {
                let val = light.trace_shadow(&scene, hit_pos, 8.).map(|shadow| {
                    shadow * light.calc_diffuse(hit_pos, gradient(&scene, hit_pos))
                }).unwrap_or(0.) + ambient;
                (val.max(0.).min(1.) * 255.) as u8
            }).unwrap_or(0);
            buf.push(color);
        }
    }
    Image::new(cam.size.x, cam.size.y, buf)
}

// Scene definition

fn sphere(p: Vec3f, r: f64) -> f64
{
    p.norm() - r
}

fn torus(Vec3{x, y, z}: Vec3f, r1: f64, r2: f64) -> f64
{
    let q = (x*x + z*z).sqrt() - r1;
    (q*q + y*y).sqrt() - r2
}

fn plane(p: Vec3f, n: Vec3f, b: f64) -> f64
{
    p.dot(&n) + b
}

fn cone(Vec3{x, y, z}: Vec3f, t: f64) -> f64
{
    let q = (x*x + z*z).sqrt();
    q * t.cos() + y * t.sin()
}

fn box_(p: Vec3f, b: Vec3f) -> f64
{
    let d = na::abs(&p) - b;
    let dm = d.x.max(d.y).max(d.z).min(0.);
    let dn = Vec3::new(d.x.max(0.), d.y.max(0.), d.z.max(0.));
    dm + dn.norm()
}

fn cylinder_xy(Vec3{x, y, ..}: Vec3f, r: f64) -> f64
{
    (x*x + y*y).sqrt() - r
}

fn cylinder_yz(Vec3{y, z, ..}: Vec3f, r: f64) -> f64
{
    (y*y + z*z).sqrt() - r
}

fn mandel_dist(Vec3{x, z, ..}: Vec3f, max_it: u32) -> f64
{
    let c = Complex64::new(x, z);
    let mut z = Complex64::zero();
    let mut dz = Complex64::one();
    for _ in 0..max_it
    {
        dz = (z * dz).scale(2.) + Complex64::one();
        z = z * z + c;
        let zsq = z.norm_sqr();
        if zsq > 1024.
        {
            return (zsq / dz.norm_sqr()).sqrt() * zsq.ln() * 0.5
        }
    }
    0.
}

fn scene(p: Vec3f) -> f64
{
    sphere(p - Vec3::new(2., -0.5, 0.), 0.6)
    .min(cone(p - Vec3::new(3.2, 0., 0.5), consts::FRAC_PI_6))
    .min(torus(p - Vec3::new(-1., 0., 0.), 1.5, 0.3))
    .min(box_(p, Vec3::new(1., 1., 1.))
        .max(-sphere(p, 1.3)))
    .min(cylinder_xy(p - Vec3::new(0., -1., 0.), 0.6)
        .max(cylinder_yz(p - Vec3::new(0., -1., 2.5), 0.6)))
    .min(mandel_dist(p - Vec3::new(2.1, 0., 1.5), 255)
        .max(plane(p, Vec3::y(), 0.6)))
    .min(plane(p, Vec3::x(), 2.1))
    .min(plane(p, Vec3::y(), 1.))
    .min(plane(p, Vec3::z(), 1.5))
}

fn main()
{
    let cam = Camera::new(640, 480, 60., Vec3::new(3., 3., 5.), Vec3::new(0., -1., 0.), Vec3::y());
    let light = Light::new(Vec3::new(3., 2., 2.), 7.);
    let img = render(scene, &cam, &light, 0.03, 42.);
    File::create("dist/sphtrace-rs.ppm").and_then(|f| img.write_ppm(f)).unwrap();
}
