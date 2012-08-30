namespace Affray

open System
open Microsoft.FSharp.Math
open Affray.Math

module Geometry = 

    /// <summmary>
    /// A 3d point - defined simply as an alias for a vector.
    /// </summary>
    type point = vector
                
    /// Draws a vector from point p1 to point p2 
    let vector_between (p1: point) (p2: point) : vector = 
        let x' = p2.x - p1.x
        let y' = p2.y - p1.y
        let z' = p2.z - p1.z
        {x = x'; y = y'; z = z'}
        
    type sphere = 
        {centre: point; radius: float}
        
    type box = 
        {lower: point; upper: point}
        
    type cylender = 
        {a: point; b: point; radius: float} 

    type plane = 
        {normal: unit_vector; offset: float}

    type bounded_plane = 
        {plane: plane; min: point; max: point}

    /// <summary>
    /// A ray startinng of at a given position and running off to infinity in a
    /// given direction
    /// </summary>
    type ray =
        {src: point; direction: unit_vector}

        override self.ToString () = 
            sprintf "src: (%.12f, %.12f, %.12f); dir: (%.12f, %.12f, %.12f)" 
                self.src.x self.src.y self.src.z 
                self.direction.x self.direction.y self.direction.z

    /// <summary>
    /// Attempts to find the point on a ray where it intersects with the given
    /// sphere.
    /// </summary>
    /// <remarks>
    /// For the maths behind how it works, check out the following links
    ///   http://wiki.cgsociety.org/index.php/Ray_Sphere_Intersection
    ///   http://www.siggraph.org/education/materials/HyperGraph/raytrace/rtinter1.htm
    /// <remarks>
    let ray_sphere_intersection (r : ray) (s: sphere) : double option = 
        let dist = vector_between r.src s.centre
        let b = r.direction |> dot <| dist
        match (sqr b) - (dist |> dot <| dist) + (sqr s.radius) with 
        | n when n < 0.0 -> None
        | d_squared ->
            let d = sqrt d_squared
            let t1, t2 = (b - d, b + d)
            if t2 > 0.0 
                then Some <| if t1 > 0.0 then t1 else t2 
                else None

    let sphere_normal_at (p: point) (s: sphere) = 
        p - s.centre |> normalize

    let ray_plane_intersection (r : ray) (p : plane) = 
        let n = (p.offset - dot r.src p.normal)
        let d = (dot r.direction p.normal)
        match n / d with
        | a when a > 0.0 -> Some a
        | _ -> None

    let ray_bounded_plane_intersection (r: ray) (p: bounded_plane) = 
        match ray_plane_intersection r p.plane with
        | Some t -> 
            let pt = r.src + (t * r.direction)
            let xi = pt |> dot <| positive_x
            let yi = pt |> dot <| positive_y
            let zi = pt |> dot <| positive_z

            let intersects = p.min.x < xi && xi < p.max.x && 
                             p.min.y < yi && yi < p.max.y &&
                             p.min.z < zi && zi < p.max.z
            if intersects 
                then Some t 
                else None
        | None -> None