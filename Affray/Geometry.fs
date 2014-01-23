namespace Affray

open System
open System.Diagnostics
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

    type cylender = 
        {a: point; b: point; radius: float} 

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
