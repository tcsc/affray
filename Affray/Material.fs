namespace Affray 

open Microsoft.FSharp.Core.Operators

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

    type material = Solid of colour * finish
                  | Checkerboard of material * material

    let checkerboard (p: point) (a: material) (b: material) : material = 
        let pick p (m1, m2) = if p then (m1,m2) else (m2,m1)
        (a, b) |> pick ((int(ceil p.x) % 2) = 0)
               |> pick ((int(ceil p.y) % 2) = 1)
               |> pick ((int(ceil p.z) % 2) = 0)
               |> fst

    let rec texture_point (p: point) (m: material) =
        match m with 
        | Solid (c, f) -> (c, f)
        | Checkerboard (a, b) -> 
            checkerboard p a b |> texture_point p