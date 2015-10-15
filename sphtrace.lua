#!/usr/bin/env luajit
local abs, log, max, min, sqrt, tan = math.abs, math. log, math.max, math.min, math.sqrt, math.tan

local function normalize3(x, y, z)
    local len = sqrt(x*x + y*y + z*z)
    return { x / len, y / len, z / len }
end

local function normalize(vec)
    return normalize3(vec[1], vec[2], vec[3])
end

local function cross(u, v)
    return {
        u[2] * v[3] - u[3] * v[2],
        u[3] * v[1] - u[1] * v[3],
        u[1] * v[2] - u[2] * v[1],
    }
end

local function gradient(fn, x, y, z)
    local eps = 1e-6
    return normalize3(
        fn(x+eps, y, z) - fn(x-eps, y, z),
        fn(x, y+eps, z) - fn(x, y-eps, z),
        fn(x, y, z+eps) - fn(x, y, z-eps)
    )
end

local camera_mt = {}
camera_mt.__index = camera_mt

local function camera(x, y, z)
    local obj = {
        pos = { x or 0, y or 0, z or 0 },
        cz = 1,
    }
    return setmetatable(obj, camera_mt)
end

function camera_mt:set_fov(fov)
    self.cz = 1 / tan(fov * math.pi / 360)
    return self
end

function camera_mt:set_size(w, h)
    self.w, self.h = w, h
    self.scale_x = 2 / (w - 1)
    self.scale_y = 2 / (h - 1)
    self.aspect = h / w
    return self
end

function camera_mt:look_at(center, up)
    self.K = normalize3(center[1] - self.pos[1], center[2] - self.pos[2], center[3] - self.pos[3])
    self.I = normalize(cross(self.K, up or { 0, 1, 0 }))
    self.J = cross(self.I, self.K)
    return self
end

function camera_mt:calc_ray(sx, sy)
    local cx = sx * self.scale_x - 1
    local cy = (1 - sy * self.scale_y) * self.aspect
    local I, J, K, cz = self.I, self.J, self.K, self.cz
    local x = I[1] * cx + J[1] * cy + K[1] * cz
    local y = I[2] * cx + J[2] * cy + K[2] * cz
    local z = I[3] * cx + J[3] * cy + K[3] * cz
    local len = sqrt(x*x + y*y + z*z)
    return x / len, y / len, z / len
end

function camera_mt:trace_ray(dist_fn, sx, sy, max_t)
    local pos = self.pos
    local bx, by, bz = pos[1], pos[2], pos[3]
    local mx, my, mz = self:calc_ray(sx, sy)
    local t, eps = 0, 0.001
    while t < max_t do
        local rx, ry, rz = bx + mx * t, by + my * t, bz + mz * t
        local d = dist_fn(rx, ry, rz)
        if d < eps then
            return rx, ry, rz
        end
        t = t + d
    end
end

local light_mt = {}
light_mt.__index = light_mt

local function light(x, y, z, v)
    local obj = {
        pos = { x or 0 , y or 0, z  or 0 },
        v = v or 1,
    }
    return setmetatable(obj, light_mt)
end

function light_mt:calc_diffuse(x, y, z, norm)
    local pos = self.pos
    local dx, dy, dz = pos[1] - x, pos[2] - y, pos[3] - z
    local dsq = dx*dx + dy*dy + dz*dz
    local il = 1 / sqrt(dsq)
    return self.v * il * max(norm[1] * dx + norm[2] * dy + norm[3] * dz, 0) / dsq
end

function light_mt:trace_shadow(dist_fn, x, y, z, k)
    local pos = self.pos
    local dx, dy, dz = pos[1] - x, pos[2] - y, pos[3] - z
    local dist = sqrt(dx*dx + dy*dy + dz*dz)
    local mx, my, mz = dx / dist, dy / dist, dz / dist
    local t, eps, res = 0.02, 0.001, 1
    while t < dist do
        local rx, ry, rz = x + mx * t, y + my * t, z + mz * t
        local d = dist_fn(rx, ry, rz)
        if d < eps then
            return 0
        end
        res = min(res, k * d / t)
        t = t + d
        --t = t + min(max(d, 0.02), 0.1)
    end
    return res
end

local function render(scene, cam, light_src, ambient)
    local buf, i = {}, 1
    for y = 0, cam.h - 1 do
        for x = 0, cam.w - 1 do
            local rx, ry, rz = cam:trace_ray(scene, x, y, 42)
            local col = 0
            if rx then
                local diffuse = 0
                local shadow = light_src:trace_shadow(scene, rx, ry, rz, 8)
                if shadow > 0 then
                    local normal = gradient(scene, rx, ry, rz)
                    diffuse = light_src:calc_diffuse(rx, ry, rz, normal)
                end
                col = ambient + diffuse * shadow
            end
            buf[i] = string.char(min(col * 255, 255))
            i = i + 1
        end
    end
    return table.concat(buf)
end

---

local function sphere(x, y, z, r)
    return sqrt(x*x + y*y + z*z) - r
end

local function torus(x, y, z, r1, r2)
    local q = sqrt(x*x + z*z) - r1
    return sqrt(q*q + y*y) - r2
end

local function plane(x, y, z, nx, ny, nz, b)
    return x*nx + y*ny + z*nz + b
end

local function cone(x, y, z, ct, st)
    local q = sqrt(x*x + z*z)
    return q*ct + y*st
end

local function box(x, y, z, bx, by, bz)
    local dx, dy, dz = abs(x) - bx, abs(y) - by, abs(z) - bz
    local mx, my, mz = max(dx, 0), max(dy, 0), max(dz, 0)
    return min(max(dx, dy, dz), 0) + sqrt(mx*mx + my*my + mz*mz)
end

local function cylinder(x, y, r)
    return sqrt(x*x + y*y) - r
end

local function mandel_dist(cr, ci, max_it)
    local zr, zi = 0, 0
    local Dzr, Dzi = 1, 0
    for _ = 1, max_it do
        Dzr, Dzi = (zr * Dzr - zi * Dzi) * 2 + 1, (zr * Dzi + zi * Dzr) * 2
        zr, zi = zr * zr - zi * zi + cr, zr * zi * 2 + ci
        local zsq = zr * zr + zi * zi
        if zsq > 1024 then
            local Dzsq = Dzr * Dzr + Dzi * Dzi
            return sqrt(zsq / Dzsq) * log(zsq) * 0.5
        end
    end
    return 0
end

local ct, st = math.cos(math.pi/6), math.sin(math.pi/6)

local function scene(x, y, z)
    return min(
        sphere(x-2, y+.5, z, 0.6),
        cone(x-3.2, y, z-0.5, ct, st),
        torus(x+1, y, z, 1.5, .3),
        max(box(x, y, z, 1, 1, 1), -sphere(x, y, z, 1.3)),
        max(cylinder(x, y+1, .6), cylinder(y+1, z-2.5, .6)),
        max(mandel_dist(x-2.1, z-1.5, 256), plane(x, y, z, 0, 1, 0, 0.6)),
        plane(x, y, z, 1, 0, 0, 2.1), plane(x, y, z, 0, 1, 0, 1), plane(x, y, z, 0, 0, 1, 1.5)
    )
end

local cam = camera(3, 3, 5):set_size(640, 480):set_fov(60):look_at{ 0, -1, 0 }
local light_src = light(3, 2, 2, 7)

local file = arg[1] and assert(io.open(arg[1], "wb")) or io.stdout
file:write(string.format("P5\n%d %d\n%d\n", cam.w, cam.h, 255))
file:write(render(scene, cam, light_src, 0.03))
