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

    /// <summary>
    /// A ray startinng of at a given position and running off to infinity in a
    /// given direction
    /// </summary>
    type ray = {src: point; direction: unit_vector}

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
            