module Renderer

open System.Drawing
open System.Drawing.Imaging
open Microsoft.FSharp.NativeInterop

open Affray.Geometry
open Affray.Math
open Scene

type pixel = {r: float; g: float; b: float; a: float}

let to_color p =
    let a = int(255.0 * p.a)
    let r = int(255.0 * p.r)
    let g = int(255.0 * p.g)
    let b = int(255.0 * p.b)
    Color.FromArgb(a,r,g,b)
        
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

let trace (s: scene) (r: ray) = 
    match find_closest_intersecting_object s r with
    | None -> {r = 1.0; g = 1.0; b = 1.0; a = 1.0 } 
    | Some (o, t) -> {r = 1.0; g = 0.0; b = 0.0; a = 1.0 } 

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
        