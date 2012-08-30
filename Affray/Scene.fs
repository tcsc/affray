module Scene

open Affray.Colour
open Affray.Geometry
open Affray.Math
open Affray.Material
open PointLight

type primitive = Sphere of sphere
               | Plane of plane
               | BoundedPlane of bounded_plane

type obj = 
    {primitive: primitive; material: material}

type light = PointSource of point_light

let light_colour (l:light) = 
    match l with
    | PointSource p -> p.colour

let light_location (l: light) = 
    match l with 
    | PointSource p -> p.location

type camera = {
        location: point; 
        direction: unit_vector; 
        up: unit_vector;
        horizontal_fov: float<radians>;
        vertical_fov: float<radians>
     }

/// the default camera - roughly simulates the angle of view of a 50mm lens on 
/// a 35mm-format camera
let default_camera = { 
        location = {x = 0.0; y = 0.0; z = 0.0}
        direction = {x = 0.0; y = 0.0; z = -1.0}
        up = {x = 0.0; y = 1.0; z = 0.0}
        horizontal_fov = to_radians 39.6<degrees>
        vertical_fov = to_radians 27.0<degrees> 
    }

let set_aspect_ratio (aspect: float) (fov_x: float<radians>) (c: camera) =
    let fov_y = (1.0/aspect) * fov_x
    {c with horizontal_fov = fov_x; vertical_fov = fov_y}

/// <summary>
/// Pans and rotates the supplied camera to look at a given point.
/// </summary>
let look_at (target: point) (c: camera) = 
    let direction' = normalize (target - c.location)
    let right = normalize (cross direction' c.up)
    let up' = normalize (cross right direction')
    {c with up = up'; direction = direction'}

/// Describes a scene. Probably only temporary, as I don't think it will scale 
/// to complex scenes.
type scene = {camera: camera; objects: obj list; lights: light list}

/// The default scene. No objects, no lights and the default camera.
let default_scene = { 
        camera = default_camera;
        objects = [];
        lights = [] 
    }

let add_object s o = 
    {s with objects = o :: s.objects}
    
let add_light s l = 
    {s with lights = l :: s.lights}
    
let intersects (r: ray) (o: obj) = 
    match o.primitive with
    | Sphere s -> ray_sphere_intersection r s
    | Plane p -> ray_plane_intersection r p
    | BoundedPlane p -> ray_bounded_plane_intersection r p
    
let surface_normal (p: point) (o: obj) = 
    match o.primitive with
    | Sphere s -> sphere_normal_at p s
    | Plane p -> p.normal
    | BoundedPlane p -> p.plane.normal
     
type ray_context = {x: int; y: int; r: ray} 
   
let planar_projection (width: int) (height: int) (cam: camera) = 
    // work out the worldspace co-ordinates of the corner points of the viewport
    // and then linearly interpolate. 
    let vx = normalize <| cross cam.direction cam.up
    let vy = normalize <| cross vx cam.direction // assuming that vy == cam.up ???
    let plane_centre = cam.location + cam.direction
    
    let tan_x = tan <| float(cam.horizontal_fov / 2.0)
    let left = (-tan_x * vx)
    let right = (tan_x * vx)
    let next_pixel = (right - left) / width
    
    let tan_y = tan <| float(cam.vertical_fov / 2.0)
    let top = (tan_y * vy)
    let bottom = (-tan_y * vy)
    let next_scan = (bottom - top) / height
    
    let start = plane_centre + left + top

    seq { 
        for y = 0 to height-1 do
            let scan_start = start + (y * next_scan)
            for x = 0 to width-1 do
                let pixel = scan_start + (x * next_pixel) 
                let dir = normalize <| pixel - cam.location
                yield {x = x; y = y; r = {src = cam.location; direction = dir}}
    }
        