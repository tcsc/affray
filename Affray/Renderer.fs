module Renderer

open System
open System.Drawing
open System.Drawing.Imaging
open Microsoft.FSharp.NativeInterop

open Affray.Colour
open Affray.Geometry
open Affray.Math
open Scene

type pixel = 
    {r: float; g: float; b: float; a: float}
    static member fromColour (c: colour) = 
        {r = c.r; g = c.g; b = c.b; a = 1.0}

let to_color p =
    let a = int(255.0 * p.a)
    let r = int(255.0 * p.r)
    let g = int(255.0 * p.g)
    let b = int(255.0 * p.b)
    Color.FromArgb(a,r,g,b)

let in_shadow (s: scene) (r: ray) = 
    let rec scan_objects (objects: obj list) (r: ray) = 
        match objects with
        | [] -> false
        | o :: tail ->
            match intersects r o with 
            | Some _ -> true
            | None -> scan_objects tail r
    scan_objects s.objects r
        
let find_closest_intersecting_object (s: scene) (r: ray) = 
    let rec scan_objects (closest: (obj * float) option) (objects: obj list) =
        match objects with
        | [] -> closest
        | candidate :: tail -> 
            match intersects r candidate with
            | None -> scan_objects closest tail
            | Some t ->
                let min_t = match closest with 
                            | None -> System.Double.PositiveInfinity
                            | Some (_,x) -> x

                let closest' = match t < min_t with
                               | true -> Some (candidate, t)
                               | false -> closest
                scan_objects closest' tail
    scan_objects None s.objects
    
/// sets a pixel in a 32-bit image
let set_pixel (bits: BitmapData) x y p = 
    let p_scan = bits.Scan0 + nativeint(y * bits.Stride)
                 |> NativePtr.ofNativeInt<int32>
    NativePtr.set p_scan x p

/// Converts a device-indepentant pixel into an ARGB32 quad 
let convert p : int32 = 
    let a = int(255.0 * p.a)
    let r = int(255.0 * p.r)
    let g = int(255.0 * p.g)
    let b = int(255.0 * p.b)
    ((a &&& 0xFF) <<< 24) ||| ((r &&& 0xFF) <<< 16) ||| ((g &&& 0xFF) <<< 8) ||| (b &&& 0xFF)

let blinn_phong_highlight (view_ray: ray) (light_ray: ray) (norm: unit_vector) (intensity: colour) (mat: material) : colour = 
    match mat.highlight.intensity with 
    | spec when spec > 0.0 ->
        let half_vector = light_ray.direction - view_ray.direction
        match sqrt (half_vector |> dot <| half_vector) with
        | x when x = 0.0 -> black
        | x -> 
            let blinn_vector = half_vector * (1.0/x)
            let blinn_base = max (blinn_vector |> dot <| norm) 0.0
            let blinn_term = spec * (blinn_base ** mat.highlight.size)
            blinn_term * intensity
    | _ -> black

let lighting_contribution (view_ray: ray) (scene: scene) (l: light) (p: point) (n: unit_vector) (m: material) : colour = 
    let light_pos = light_location l
    let light_beam = light_pos - p
    if dot n light_beam < 0.0
        then black
        else
            // move the starting point a miniscule amount in the direction of the surface 
            // normal to avoid this point shadowing itself.
            let src = p + (1e-12 * n) 
            let light_ray = {src = src; direction = normalize light_beam}
            if in_shadow scene light_ray 
                then black
                else 
                    let lambert = dot light_ray.direction n
                    let diffuse = m.diffuse * m.colour * light_colour l * lambert
                    let specular = blinn_phong_highlight view_ray light_ray n (light_colour l) m
                    diffuse + specular
             
let light_point (view_ray: ray) (s: scene) (pt: point) (norm: unit_vector) (mat: material) = 
    let compute_lighting (c: colour) (l: light) = 
        c + lighting_contribution view_ray s l pt norm mat
    List.fold compute_lighting (mat.colour * mat.ambient) s.lights


let trace_pixel (s: scene) (r: ray) = 
    let rec trace (s: scene) (r: ray) (acc: colour) (weight: float) =
        match find_closest_intersecting_object s r with
        | None -> acc
        | Some (o, t) ->
            let pt = r.src + (t * r.direction)
            let n = surface_normal pt o
            let c = light_point r s pt n o.material
            let acc' = acc + (weight * c)
            match o.material.reflection with
            | ref when ref > 0.0 ->
                // reflect the vector through the surface normal:
                //   http://www.3dkingdoms.com/weekly/weekly.php?a=2
                let src = pt + (1e-12 * n) 
                let dir = (-2.0 * (r.direction |> dot <| n) * n) + r.direction
                let reflected_ray = {src = src; direction = normalize dir}
                trace s reflected_ray acc' (weight * ref)

            | _ -> acc'
    trace s r black 1.0

let render width height s (f: int -> int -> pixel -> unit) = 
    for ray_context in planar_projection width height s.camera do
        trace_pixel s ray_context.r 
        |> clamp
        |> pixel.fromColour
        |> f ray_context.x ray_context.y

let render_to_bitmap (bmp: Bitmap) (s: scene) = 
    let with_bits_do (f : BitmapData -> unit) = 
        let bounds = Rectangle(0,0, bmp.Width, bmp.Height)
        let bits = bmp.LockBits (bounds, ImageLockMode.ReadWrite, bmp.PixelFormat)
        try
            f bits
        finally
            bmp.UnlockBits bits

    let render_to_bits bits = 
        let pixel_catcher = fun x y p -> convert p |> set_pixel bits x y 
        render bmp.Width bmp.Height s pixel_catcher

    with_bits_do render_to_bits
        