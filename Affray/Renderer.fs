namespace Affray

open System
open System.Drawing
open System.Drawing.Imaging

open Microsoft.FSharp.NativeInterop
open Microsoft.FSharp.Collections

open Affray.Colour
open Affray.Geometry
open Affray.Material
open Affray.Math
open Scene

module Renderer = 
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

    let blinn_phong_highlight (view_ray: ray)
                              (light_ray: ray)
                              (norm: unit_vector)
                              (intensity: colour)
                              (finish: finish) : colour =
        match finish.highlight.intensity with 
        | spec when spec > 0.0 ->
            let half_vector = light_ray.direction - view_ray.direction
            match sqrt (half_vector |> dot <| half_vector) with
            | x when x = 0.0 -> black
            | x -> 
                let blinn_vector = half_vector * (1.0/x)
                let blinn_base = max (blinn_vector |> dot <| norm) 0.0
                let blinn_term = spec * (blinn_base ** finish.highlight.size)
                blinn_term * intensity
        | _ -> black

    let lighting_contribution (view_ray: ray)
                              (scene: scene)
                              (l: light)
                              (p: point)
                              (n: unit_vector)
                              (colour: colour)
                              (finish: finish) : colour = 
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
                        let diffuse = finish.diffuse * colour * light_colour l * lambert
                        let specular = blinn_phong_highlight view_ray light_ray n (light_colour l) finish
                        diffuse + specular
             
    let light_point (view_ray: ray)
                    (s: scene)
                    (pt: point)
                    (norm: unit_vector)
                    (colour: colour)
                    (finish: finish) = 
        let compute_lighting (c: colour) (l: light) = 
            c + lighting_contribution view_ray s l pt norm colour finish

        in List.fold compute_lighting (colour * finish.ambient) s.lights

    // reflect the vector through the surface normal:
    //   http://www.3dkingdoms.com/weekly/weekly.php?a=2
    let reflect_ray (r: ray) (n: unit_vector) (pt: point) = 
        let src = pt + (1e-12 * n) 
        let dir = (-2.0 * (r.direction |> dot <| n) * n) + r.direction
        {src = src; direction = normalize dir}
        
    let sky_value (s: scene) (r: ray) =
        // treat the sky as a fantastically large unit sphere with the camera 
        // location at the centre. Not sure if this will work in the wild - 
        // animations with a moving camera could be problematic - but good 
        // enough for now.
        pigment_colour s.sky r.direction

    let trace_pixel (s: scene) (r: ray) = 
        let rec trace (s: scene) (r: ray) (acc: colour) (weight: float) =
            match find_closest_intersecting_object s r with
            | None -> acc + weight * (sky_value s r)
            | Some (o, t) ->
                let pt = r.src + (t * r.direction)
                let colour, finish = texture_point pt o.material 
                let n = surface_normal pt o
                let c = light_point r s pt n colour finish
                let acc' = acc + (weight * c)

                match finish.reflection with
                | ref when ref > 0.0 ->
                    let r' = reflect_ray r n (pt + (1e-12 * n))
                    in trace s r' acc' (weight * ref)

                | _ -> acc'

        trace s r black 1.0

    let render width height s (f: int -> int -> pixel -> unit) = 
        planar_projection width height s.camera
        |> Seq.fold (fun _ ctx -> trace_pixel s ctx.r //PSeq
                                   |> clamp
                                   |> pixel.fromColour
                                   |> f ctx.x ctx.y) ()

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
        