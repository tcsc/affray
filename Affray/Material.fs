namespace Affray 

open Microsoft.FSharp.Core.Operators

open Affray.Math
open Affray.Colour
open Affray.Geometry

module Material = 

    type highlight = {intensity: float; size: float}

    type finish = {
        opacity: float;
        reflection: float; 
        ambient: float;
        diffuse: float;
        highlight: highlight; 
    }

    let default_finish = {
        opacity = 1.0;
        reflection = 0.0; 
        ambient = 0.1;
        diffuse = 0.75;
        highlight = {intensity = 0.9; size = 60.0}
    }

    type pigment = Colour of colour
                 | Gradient of unit_vector * (float * colour) list

    let gradient_pigment_at (pt: point) 
                            (dir: vector) 
                            (colours: (float * colour) list) : colour = 

        let rec find_colour_range cs v prev =  
            match cs with
            | [] ->
                let next = (1.0, snd prev)
                in (prev, next)
            | next :: tail ->
                let v0 = fst prev 
                let v1 = fst next
                match v with
                | n when v0 <= v && v <= v1 -> (prev, next)
                | _ -> find_colour_range tail v next
 
        let u = (dot dir pt)
        let v = (abs(u % 1.0))
        let ((v0, c0), (v1, c1)) = find_colour_range colours v colours.Head
        let delta = c1 - c0
        let v' = (v - v0) / (v1 - v0)
        c0 + (v' * delta)


    let pigment_colour p pt = 
        match p with
        | Colour c -> c
        | Gradient (dir, colours) -> gradient_pigment_at pt dir colours
        
    type material = Solid of pigment * finish
                  | Checkerboard of material * material

    let checkerboard (p: point) (a: material) (b: material) : material = 
        let pick p (m1, m2) = if p then (m1,m2) else (m2,m1)
        (a, b) |> pick ((int(ceil p.x) % 2) = 0)
               |> pick ((int(ceil p.y) % 2) = 1)
               |> pick ((int(ceil p.z) % 2) = 0)
               |> fst

    /// <param name="pt">
    /// The point to texture, in object co-ordinates
    /// </param>
    let texture_point (pt: point) (mat: material) : (colour * finish) =
        let rec point_material  (p: point) (m: material) : (pigment * finish) = 
            match m with 
            | Solid (p, f) -> (p, f)
            | Checkerboard (a, b) -> checkerboard p a b |> point_material p

        let pigment, finish = point_material pt mat
        in (pigment_colour pigment pt, finish)