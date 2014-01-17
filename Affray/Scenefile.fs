namespace Affray

open System
open System.IO
open FParsec
open FParsec.Error

open Affray.Colour
open Affray.Material
open Affray.Math
open Affray.Geometry
open Scene

module Scenefile = 
    /// <summary>
    /// The parser context: stores all of the named objects in the scene dutring parsing.
    /// </summary>
    type scene_state = 
        { colours: Map<string, colour>;
          finishes: Map<string, finish>;
          materials: Map<string, material> }
        
        member self.GetColour name = Map.tryFind name self.colours
        member self.GetFinish name = Map.tryFind name self.finishes
        member self.GetMaterial name = Map.tryFind name self.materials

        static member empty 
            with get() = { colours = Map.empty; 
                           finishes = Map.empty; 
                           materials = Map.empty }


    let expression = spaces >>. pfloat .>> spaces

    let delimited_block start finish p = (pstring start) >>. spaces >>. p .>> spaces .>> (pstring finish)

    let block p = delimited_block "{" "}" p

    let named_block name p = // name { p }
        (pstring name) >>. spaces >>. block p

    let comma = pstring ","

    /// <summary>
    /// Predicate chaining OR, i.e. (f .||. g) x -> f x || g x
    /// </summary>
    let (.||.) f g = fun c -> f c || g c

    /// <summary>
    /// An arbitrary comination of letters and some punctuation.
    /// </summary>
    let symbol = spaces >>. (many1Satisfy (isLetter .||. (isAnyOf "_-"))) .>> spaces


    let named_value name p = 
        spaces >>. (pstring name) >>. (pstring ":") >>. spaces >>. p .>> spaces

    /// <summary>
    /// A paraser follerd by a comma and (optional) spaces
    /// </summary>
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
    /// The structure for a declaration.
    /// </summary>
    let let_binding (typename: string) (p: Parser<'a,scene_state>) storefn : Parser<unit,scene_state> =
        let binding = ((pstring "let") >>. spaces >>. (pstring typename) >>. symbol) 
                      .>>. 
                      ((pstring "=") >>. spaces >>. p)
        fun (stream: CharStream<scene_state>) ->
            match binding stream with
            | r when r.Status = Ok -> storefn r.Result stream
            | r -> Reply(Error, r.Error)

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
    let colour_literal = 
        block (arglist3 expression
                        expression 
                        expression
                        (fun r g b -> {r = r; g = g; b = b}))

    let colour_symbol = 
        fun stream -> 
            match symbol stream with
            | r when r.Status = Ok -> 
                let s = stream.UserState
                match Map.tryFind r.Result s.colours with
                | Some c -> Reply(c)
                | None -> Reply(Error, messageError(sprintf "undefined colour: %s" r.Result))
            | r -> Reply(Error, r.Error)

    let colour = colour_literal <|> colour_symbol 

    let private store_colour (name: string, colour: colour) : Parser<unit,scene_state> = 
        fun stream ->
            let state = stream.UserState
            match Map.containsKey name state.colours with
            | true -> Reply(Error, messageError(sprintf "colour %s already defined" name))
            | false ->
                let colours' = Map.add name colour state.colours
                let state' = {state with colours = colours'}
                (setUserState state') stream       

    let colour_declaration : Parser<unit,scene_state> = 
        let_binding "colour" colour_literal store_colour

    let colour_point = delimited_block "(" ")" (arglist2 pfloat colour (fun p c -> (p, c)))

    let colour_map = 
        delimited_block "[" "]" (sepBy1 colour_point (comma .>> spaces))

    let gradient_pigment = // gradient { direction: {x, y, z}, colours: [(n, c), (n, c)] }
        named_block "gradient" 
            (arglist2 (named_value "direction" unit_vector)
                      (named_value "colours" colour_map)
                      (fun dir cols -> Gradient (dir, cols)))

    let colour_pigment =
        (pstring "colour") >>. spaces >>. colour .>> spaces |>> Colour
 
    let pigment = colour_pigment <|> gradient_pigment

    let highlight = block (arglist2 (named_value "intensity" pfloat) 
                                    (named_value "size" pfloat)
                                    (fun i s -> {intensity = i; size = s}))

    let finish_literal = 
        block (arglist5 (named_value "opacity" pfloat)
                        (named_value "reflection" pfloat)
                        (named_value "ambient" pfloat)
                        (named_value "diffuse" pfloat)
                        (named_value "highlight" highlight)
                        (fun o r a d h -> {opacity = o; reflection = r; ambient = a; diffuse = d; highlight = h}))

    let finish_symbol = 
        fun stream -> 
            match symbol stream with
            | r when r.Status = Ok -> 
                let s = stream.UserState
                match s.GetFinish r.Result with
                | Some f -> Reply(f)
                | None -> Reply(Error, messageError(sprintf "undefined finish: %s" r.Result))
            | r -> Reply(Error, r.Error)

    /// <summary>
    /// 
    /// </summary>
    let finish = finish_literal <|> finish_symbol

    /// <summary>
    /// Returns a parser that stores the suppiled finish in the scene_state, iff a finish 
    /// with the same name hasn't already been defined.
    /// </summary>
    let private store_finish (name: string, finish: finish) : Parser<unit, scene_state> =
        fun stream ->
            let state = stream.UserState
            match Map.containsKey name state.finishes with
            | true -> Reply(Error, messageError(sprintf "finish %s already defined" name))
            | false ->
                let finishes' = Map.add name finish state.finishes
                let state' = {state with finishes = finishes'}
                (setUserState state') stream       

    /// <summary>
    /// Allows the user to define a named finish, using the form "let finish name = { ..."
    /// </summary>
    let finish_declaration = let_binding "finish" finish_literal store_finish 

    let material_solid = 
        named_block "solid"
            (arglist2 (named_value "pigment" pigment)
                      (named_value "finish" finish)
                      (fun p f -> Solid (p, f))) 

    let rec material_checkerboard _ =
        named_block "checkerboard"
            (arglist2 (named_value "1" material)
                      (named_value "2" material)
                      (fun a b -> Checkerboard (a, b)))

    and material = material_solid <|> (material_checkerboard ())

    /// <summary>
    /// Returns a parser that stores the suppiled material in the scene_state, iff a material 
    /// with the same name hasn't already been defined.
    /// </summary>
    let private store_material (name: string, material: material) : Parser<unit, scene_state> =
        fun stream ->
            let state = stream.UserState
            match Map.containsKey name state.finishes with
            | true -> Reply(Error, messageError(sprintf "finish %s already defined" name))
            | false ->
                let materials' = Map.add name material state.materials
                let state' = {state with materials = materials'}
                (setUserState state') stream       


    /// <summary>
    /// Allows the user to define a named material, using the form 
    /// "let material name = material_type { ... }"
    /// </summary>
    let material_declaration = let_binding "material" material store_material

    /// <summary>
    /// Any sort of let ... binding
    /// </summary>
    let declaration = (attempt colour_declaration) <|> 
                      (attempt finish_declaration) <|> 
                      (attempt material_declaration)


    let primitive = sphere <|> plane <|> bounded_plane

//    let parse_scene src = run primitive src

//    let load_scene (filename: string) = 
//        let text = File.ReadAllText filename
//        parse_scene text