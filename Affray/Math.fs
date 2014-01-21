module Affray.Math

open System
open System.Diagnostics
open Microsoft.FSharp.Collections

[<Measure>] type radian
[<Measure>] type rad = radian
[<Measure>] type radians = radian

[<Measure>] type degree
[<Measure>] type deg = degree
[<Measure>] type degrees = degree

let degrees_per_radian = 180.0<deg> / 3.14159265358979323846<rad>
let radians_per_degree = 3.14159265358979323846<rad> / 180.0<deg>
let to_radians (a: float<deg>) : float<rad> = a * radians_per_degree
let to_degrees (a: float<rad>) : float<deg> = a * degrees_per_radian

let sqr (x: float) : float = x * x

/// <summary>
/// Checks to see if two floats are close enough to be considered equal in real life, even
/// if they're not actually identical. 
/// </summary>
let closeEnough (a: float) (b: float) =  
    if a = b then true
    elif (a * b) = 0.0 then (a - b) < (Double.Epsilon * Double.Epsilon)
    else ((a - b) / (abs a + abs b)) < Double.Epsilon

/// <summary>
/// Defines a simple 3d vector type and some basic operations.
/// </summary>
[<DebuggerDisplay("x = {x}, y = {y}, z = {z}"); CustomEquality; NoComparison>]
type vector =
    { x: float; y: float; z: float }

    static member ( - ) (a: vector, b: vector) : vector = 
        {x = a.x - b.x; y = a.y - b.y; z = a.z - b.z}

    static member ( + ) (a: vector, b: vector) = 
        {x = a.x + b.x; y = a.y + b.y; z = a.z + b.z}

    static member ( * ) (a: vector, b: float) = 
        {x = a.x * b; y = a.y * b; z = a.z * b}
       
    static member ( * ) (a: float, b: vector) = b * a
    
    static member ( * ) (a: int, b: vector) =  float(a) * b
        
    static member ( * ) (a: vector, b: int) = a * float(b)

    static member ( / ) (a: vector, b: float) = 
        {x = a.x / b; y = a.y / b; z = a.z / b}
    
    static member ( / ) (a: vector, b: int) = 
        a / float(b)

    static member ( ~- ) (v: vector) = 
        {x = -v.x; y = -v.y; z = -v.z}

    /// <summary>
    /// Implements the standard CLR equality operator. Compares the individual 
    /// components of the vector for practical equality, rather than binary 
    /// identity.
    /// </summary>              
    static member op_Equality (lhs: vector, rhs: vector) : bool = 
        (closeEnough lhs.x rhs.x) && 
        (closeEnough lhs.y rhs.y) && 
        (closeEnough lhs.z rhs.z)

    /// <summary>
    /// Overrides the standard Object.Equals method. 
    /// </summary>
    /// <remarks>
    /// If the supplied object is another vector, this methoc tests it for practical 
    /// equality with the target object. All other types will compare <c>false</c>
    /// </remarks>              
       override self.Equals (other: obj) : bool = 
        match other with 
        | :? vector as v -> (closeEnough self.x v.x) && 
                            (closeEnough self.y v.y) && 
                            (closeEnough self.z v.z)
        | _ -> false

    override self.ToString () =
        sprintf "{x: %f, y: %f, z: %f}" self.x self.y self.z

    override self.GetHashCode () = 
        (int)(self.x + self.y + self.z)

let positive_x = {x = 1.0;  y = 0.0;  z = 0.0}
let positive_y = {x = 0.0;  y = 1.0;  z = 0.0}
let positive_z = {x = 0.0;  y = 0.0;  z = 1.0}
let negative_x = {x = -1.0; y = 0.0;  z = 0.0}
let negative_y = {x = 0.0;  y = -1.0; z = 0.0}
let negative_z = {x = 0.0;  y = 0.0;  z = -1.0}

/// <summary>
/// Defines a 4x4 matrix for manipulating 3d vectors and points in 
/// homogeneous co-ordinates 
/// </summary>
type matrix (values : float[,]) = 
    static let identity =
        new matrix(Array2D.init 4 4 (fun i j -> if i = j then 1.0 else 0.0))
    
    static let row_column_dot_product (a: matrix) (b: matrix) i j = 
        let mutable result = 0.0
        for n = 0 to 3 do
            result <- result + a.[i, n] * b.[n, j]
        result 
                 
    static member ( * ) (a: matrix, b: matrix) = 
        let result = Array2D.zeroCreate 4 4
        for j = 0 to 3 do
            for i = 0 to 3 do
                result.[i, j] <- row_column_dot_product a b i j
        matrix result
       
    override self.Equals (obj: Object) : bool =
        match obj with
        | :? matrix as m -> matrix.op_Equality (self, m)
        | _ -> false       
            
    static member op_Equality (lhs: matrix, rhs: matrix) =
        let rec check i j (a: matrix) (b: matrix) =
            match a.[i,j] = b.[i,j] with
            | false -> false
            | true -> 
                match (i + 1) % 4 with
                | 0 ->
                    match (j + 1) % 4 with
                    | 0 -> true
                    | j' -> check 0 j' a b
                | i' -> check i' j a b
        check 0 0 lhs rhs

    member self.Item with get(i,j) = values.[i, j]
        
    static member Identity with get() = identity

/// <summary>
/// Computes and returns the transpose of the supplied matrix
/// </summary>
let transpose (m: matrix) = 
    new matrix (Array2D.init 4 4 (fun i j -> m.[j, i]))

/// <summary>
/// A unit vector alias for a vector. More for implicit documentation than
/// anything else.
/// </summary>        
type unit_vector = vector

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
let length (v: vector) : float = 
    (sqr v.x) + (sqr v.y) + (sqr v.z) |> sqrt
    
/// Computes a unit vector for a vector of arbitrary length.
let normalize v : unit_vector = 
    let inv_length = 1.0 / length v
    {x = v.x * inv_length; y = v.y * inv_length; z = v.z * inv_length}
