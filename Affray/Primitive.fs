namespace Affray
open System
open Affray.Geometry
open Affray.Material
open Affray.Math

module Primitive = 

    [<AbstractClass>]
    type Primitive (material: material) = 
        abstract member RayIntersection : ray -> double option
        abstract member NormalAt : point -> unit_vector
        member self.Material 
            with get () = material

    /// <summary>
    /// 
    /// </summary>
    type Sphere (centre: point, radius: float, material: material) = 
        inherit Primitive(material)

        /// <summary>
        /// Attempts to find the point on a ray where it intersects with the 
        /// given sphere.
        /// </summary>
        /// <remarks>
        /// For the maths behind how it works, check out the following links
        ///   http://wiki.cgsociety.org/index.php/Ray_Sphere_Intersection
        ///   http://www.siggraph.org/education/materials/HyperGraph/raytrace/rtinter1.htm
        /// <remarks>
        let intersection (r : ray) : double option = 
            let dist = vector_between r.src centre
            let b = r.direction |> dot <| dist
            match (sqr b) - (dist |> dot <| dist) + (sqr radius) with 
            | n when n < 0.0 -> None
            | d_squared ->
                let d = sqrt d_squared
                let t1, t2 = (b - d, b + d)
                if t2 > 0.0 
                    then Some <| if t1 > 0.0 then t1 else t2 
                    else None

        let normal p = p - centre |> normalize

        override self.RayIntersection r = intersection r
        override self.NormalAt pt = normal pt
        member self.Centre with get() = centre
        member self.Radius with get() = radius

    /// <summary>
    /// An infinite plane
    /// </summary>
    type Plane (normal: unit_vector, offset: float, material) = 
        inherit Primitive(material)

        let intersection r = 
            let n = (offset - dot r.src normal)
            let d = (dot r.direction normal)
            match n / d with
            | a when a > 0.0 -> Some a
            | _ -> None
        
        override self.RayIntersection pt = intersection pt
        override self.NormalAt pt = normal
        member self.Normal with get() = normal
        member self.Offset with get() = offset

    /// <summary>
    /// A plane constrained within a volume.
    /// </summary>
    type BoundedPlane (min: vector, max: vector, normal: unit_vector, offset: float, material) = 
        inherit Plane(normal, offset, material)

        override self.RayIntersection r = 
            match base.RayIntersection r with
            | None -> None
            | Some t ->
                let pt = r.src + (t * r.direction)
                let xi = pt |> dot <| positive_x
                let yi = pt |> dot <| positive_y
                let zi = pt |> dot <| positive_z

                let intersects = min.x < xi && xi < max.x && 
                                 min.y < yi && yi < max.y &&
                                 min.z < zi && zi < max.z

                if intersects then Some t else None


    let inline sort a b = 
        if a < b then (a, b) else (b, a)


    /// <summary>
    /// An axis-aligned box
    /// </summary>
    type Box (lower: point, upper: point, material) = 
        inherit Primitive(material) 

        /// <summary>
        /// Calculates the intersection point of an axis-aligned box 
        /// </summary>
        let intersection (r: ray) = 
            let mutable tmin = Double.NegativeInfinity
            let mutable tmax = Double.PositiveInfinity

            let t_min_x, t_max_x = sort ((lower.x - r.src.x) / r.direction.x) 
                                        ((upper.x - r.src.x) / r.direction.x)
            tmin <- max t_min_x tmin
            tmax <- min t_max_x tmax

            if (tmin > tmax) || (tmax < 0.0) 
                then None // no intersection missed the box, or ray origin in front of the box 
                else 
                    let t_min_y, t_max_y = sort ((lower.y - r.src.y) / r.direction.y)
                                                ((upper.y - r.src.y) / r.direction.y)
                    tmin <- max t_min_y tmin
                    tmax <- min t_max_y tmax
                    if (tmin > tmax) || (tmax < 0.0) 
                        then None
                        else 
                            let t_min_z, t_max_z = sort ((lower.z - r.src.z) / r.direction.z)
                                                        ((upper.z - r.src.z) / r.direction.z)
                            tmin <- max t_min_z tmin
                            tmax <- min t_max_z tmax
                            if (tmin > tmax) || (tmax < 0.0) 
                                then None
                                else Some tmin
                                                      
        let normal_at p = 
            let e = 1e-10
            if (abs (p.x - lower.x)) < e then negative_x
            elif (abs (p.x - upper.x)) < e then positive_x
            elif (abs (p.y - lower.y)) < e then negative_y
            elif (abs (p.y - upper.y)) < e then positive_y
            elif (abs (p.z - lower.y)) < e then negative_z
            elif (abs (p.z - upper.z)) < e then positive_z
            else failwith "Point not on cube"

        override self.RayIntersection r = intersection r
        override self.NormalAt pt = normal_at pt
        member self.Lower with get () = lower
        member self.Upper with get () = upper