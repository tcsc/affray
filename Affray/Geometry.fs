module Geometry

open System
open Microsoft.FSharp.Math

type real = float

let sqr (x: real) : real = x * x

/// Defines a simple 3d vector type and some basic operations.
type vector = 
    { x: real; y: real; z: real }
    
    static member ( - ) (a: vector, b: vector) : vector = 
        {x = a.x - b.x; y = a.y - b.y; z = a.z - b.z}

    static member ( + ) (a: vector, b: vector) = 
        {x = a.x + b.x; y = a.y + b.y; z = a.z + b.z}

    static member ( * ) (a: vector, b: real) = 
        {x = a.x * b; y = a.y * b; z = a.z * b}
 
    static member ( * ) (a: real, b: vector) = b * a

    static member ( .* ) (a: vector, b: vector) : real =     
        (a.x * b.x) + (a.y * b.y) + (a.z * b.z)

    
/// A 3d point - defined simply as an alias for a vector.
type point = vector
        
/// Computes the dot (or scalar) product of two vectors.
let dot a b = 
    (a.x * b.x) + (a.y * b.y) + (a.z * b.z)

/// Computes the cross product of two vectors. 
let cross a b =     
    let x' = (a.y * b.z) - (a.z * b.y)
    let y' = (a.z * b.x) - (a.x * b.z)
    let z' = (a.x * b.y) - (a.y * b.x)
    in {x = x'; y = y'; z = z'}
        
/// Computes the length of a vector
let length (v: vector) : real = 
    (sqr v.x) + (sqr v.y) + (sqr v.z) |> sqrt
    
let unitize v = 
    let inv_length = 1.0 / length v
    {x = v.x * inv_length; y = v.y * inv_length; z = v.z * inv_length}
    
/// Draws a vector from point p1 to point p2 
let vector_between (p1: point) (p2: point) : vector = 
    let x' = p2.x - p1.x
    let y' = p2.y - p1.y
    let z' = p2.z - p1.z
    {x = x'; y = y'; z = z'}
    
type sphere = 
    {centre: point; radius: real}
    
type box = 
    {lower: point; upper: point}
    
type cylender = 
    {a: point; b: point; radius: real} 

/// A ray startinng of at a given position and running off to infinity in a
/// given direction
type ray = {src: point; direction: vector}

/// Attempts to find the point on a ray where it intersects with the given
/// sphere.
let ray_sphere_intersection (r : ray) (s: sphere) : double option = 
    let dist = vector_between r.src s.centre // simplify by assuming a ray starting at (0, 0, 0)
    let b = r.direction .* dist
    match (sqr b) - (dist .* dist) + (sqr s.radius) with 
    | n when n < 0.0 -> None
    | d_squared ->
        let d = sqrt d_squared
        let t1, t2 = (b - d, b + d)
        if t2 > 0.0 
            then Some <| if t1 > 0.0 then t1 else t2 
            else None
            