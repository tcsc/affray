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


let lighting_contribution (scene: scene) (l: light) (p: point) (n: unit_vector) : colour = 
    match l with 
    | PointSource point_light -> 
        let light_pos = point_light.location
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
                        point_light.colour * lambert

             
let light_point (s: scene) (p: point) (n: unit_vector) = 
    let rec light_val lights luma p n = 
        match lights with 
        | [] -> luma
        | l :: tail -> 
            let luma' = luma + lighting_contribution s l p n
            light_val tail luma' p n
    light_val s.lights black p n


let trace (s: scene) (r: ray) = 
    match find_closest_intersecting_object s r with
    | None -> {r = 1.0; g = 1.0; b = 1.0; a = 1.0 } 
    | Some (o, t) ->
        let pt = r.src + (t * r.direction)
        let n = surface_normal pt o
        let c = o.material.colour * o.material.ambient +
                o.material.colour * light_point s pt n
        clamp c |> pixel.fromColour

let render width height s (f: int -> int -> pixel -> unit) = 
    for ray_context in planar_projection width height s.camera do
        trace s ray_context.r 
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
        