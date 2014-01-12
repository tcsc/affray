namespace Affray

open System
open System.IO
open FParsec

open Affray.Colour
open Affray.Material
open Affray.Math
open Affray.Geometry
open Scene

module Scenefile = 
    let expression = spaces >>. pfloat .>> spaces

    let delimited_block start finish p = (pstring start) >>. spaces >>. p .>> spaces .>> (pstring finish)

    let block p = delimited_block "{" "}" p

    let named_block name p = // name { p }
        (pstring name) >>. spaces >>. block p

    let comma = pstring ","

    let named_value name p = 
        spaces >>. (pstring name) >>. (pstring ":") >>. spaces >>. p .>> spaces

    let arg p = p .>> comma .>> spaces

    /// <summary>
    /// A comma-separated list of options, Acts like the FParsec pipe, except each parser in the pipe 
    /// (except the last) is expected to be followed by a comma and whitespace
    /// </summary>
    let arglist2 a1 a2 fn = pipe2 (arg a1) (a2) fn 

    let arglist3 a1 a2 a3 fn = pipe3 (arg a1) (arg a2) a3 fn 

    let arglist4 a1 a2 a3 a4 fn = pipe4 (arg a1) (arg a2) (arg a3) a4 fn

    let arglist5 a1 a2 a3 a4 a5 fn = pipe5 (arg a1) (arg a2) (arg a3) (arg a4) a5 fn


    /// <summary>
    /// Parses a 3D vector like { x, y, z }
    /// </summary>
    let vector =
        block (pipe3 (expression .>> comma) (expression .>> comma) (expression)
                     (fun x y z -> {vector.x = x; y = y; z = z}))

    /// <summary>
    /// Same as a vector, but automatically normalized as part of its construction.
    /// </summary>
    let unit_vector = vector |>> normalize

    /// <summary>
    /// Parses a sphere primitive
    /// </summary>  
    let sphere = // sphere { centre: vector, radius: float }
        named_block "sphere" (arglist2 (named_value "centre" vector)
                                       (named_value "radius" expression)
                                       (fun c r -> Sphere {centre = c; radius = r}))

    /// <summary>
    /// Parses an an infinite plane 
    /// </summary>
    let plane = // plane { normal: vector, offset: float }
        named_block "plane" (arglist2 (named_value "normal" unit_vector)
                                      (named_value "offset" expression)
                                      (fun n offs -> Plane {normal = n; offset = offs}))

    /// <summary>
    /// A plane bounded by some world-space constraints
    /// </summary>
    let bounded_plane =
        named_block "bounded_plane" 
            (arglist4 (named_value "normal" unit_vector)
                      (named_value "offset" expression)
                      (named_value "min" vector)
                      (named_value "max" vector)
                      (fun n o mn mx -> BoundedPlane {plane = {plane.normal = n; offset = o}; 
                                                      min = mn; 
                                                      max = mx}))

    /// <summary>
    /// {r, g, b}
    /// </summary>
    let colour = 
        block (arglist3 expression
                        expression 
                        expression
                        (fun r g b -> {r = r; g = g; b = b}))

    let colour_point = delimited_block "(" ")" (arglist2 pfloat colour (fun p c -> (p, c)))

    let colour_map = 
        delimited_block "[" "]" (sepEndBy1 colour_point (spaces >>. comma .>> spaces))

    let gradient_pigment = // gradient { direction: {x, y, z}, colours: [(n, c), (n, c)] }
        named_block "gradient" 
            (arglist2 (named_value "direction" unit_vector)
                      (named_value "colours" colour_map)
                      (fun dir cols -> Gradient (dir, cols)))

    let colour_pigment = // {r, g, b}
        colour |>> Material.Colour

    let pigment = colour_pigment <|> gradient_pigment

    let highlight = block (arglist2 (named_value "intensity" pfloat) 
                                    (named_value "size" pfloat)
                                    (fun i s -> {intensity = i; size = s}))

    let material_finish = 
        block (arglist5 (named_value "opacity" pfloat)
                        (named_value "reflection" pfloat)
                        (named_value "ambient" pfloat)
                        (named_value "diffuse" pfloat)
                        (named_value "highlight" highlight)
                        (fun o r a d h -> {finish.opacity = o; reflection = r; ambient = a; diffuse = d; highlight = h}))

    let material_solid = 
        named_block "solid"
            (arglist2 (named_value "pigment" pigment)
                      (named_value "finish" material_finish)
                      (fun p f -> Solid (p, f))) 

    let rec material_checkerboard _ =
        named_block "checkerboard"
            (arglist2 (named_value "1" material)
                      (named_value "2" material)
                      (fun a b -> Checkerboard (a, b)))

    and material = material_solid <|> (material_checkerboard ())

    let primitive = sphere <|> plane <|> bounded_plane

    let parse_scene src = run primitive src

    let load_scene (filename: string) = 
        let text = File.ReadAllText filename
        parse_scene text