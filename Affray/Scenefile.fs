namespace Affray

open System
open System.Text
open System.IO
open FParsec
open FParsec.Error

open Affray.Colour
open Affray.Material
open Affray.Math
open Affray.Geometry
open PointLight
open Scene

module Scenefile = 
    /// <summary>
    /// The parser context: stores all of the named objects in the scene dutring parsing.
    /// </summary>
    type scene_state = 
        { colours: Map<string, colour>;
          finishes: Map<string, finish>;
          materials: Map<string, material>;
          camera_defined: bool;
          scene: scene }
        
        member self.GetColour name = Map.tryFind name self.colours
        member self.GetFinish name = Map.tryFind name self.finishes
        member self.GetMaterial name = Map.tryFind name self.materials

        static member empty 
            with get() = { colours = Map.empty; 
                           finishes = Map.empty; 
                           materials = Map.empty;
                           camera_defined = false;
                           scene = default_scene }


    let expression = spaces >>. pfloat .>> spaces

    /// <summary>
    /// A block delimted by a start and end string, with the content of the block defined 
    /// by a given parser.
    /// </summary>
    let delimited_block start finish p = 
        (pstring start) >>. spaces >>. p .>> spaces .>> (pstring finish) .>> spaces

    /// <summary>
    /// A brace-delimited block, with the content of the blok defined by a supplied 
    /// parser.
    /// </summary>
    let block p = delimited_block "{" "}" p

    /// <summary>
    /// A block with a name that identifies the content. 
    /// </summary>
    let named_block name p = // name { p }
        (pstring name) >>. spaces >>. block p

    let comma = pstring ","

    /// <summary>
    /// C++ style single-line comment
    /// </summary>
    let comment : Parser<unit,scene_state> = 
        (pstring "//") .>> (skipRestOfLine true) |>> ignore

    /// <summary>
    /// Predicate chaining OR, i.e. (f .||. g) x -> f x || g x
    /// </summary>
    let (.||.) f g = fun c -> f c || g c

    /// <summary>
    /// An arbitrary comination of letters and some punctuation.
    /// </summary>
    let symbol = spaces >>. (many1Satisfy (isLetter .||. (isAnyOf "_-"))) .>> spaces

    /// <summary>
    /// A named value fo the form "name: value".
    /// </summary>
    let named_value name p = 
        spaces >>. (pstring name) >>. (pstring ":") >>. spaces >>. p .>> spaces

    /// <summary>
    /// A paraser follerd by a comma and (optional) spaces
    /// </summary>
    let arg p = p .>> comma .>> spaces

    /// <summary>
    /// A comma-separated list of options. Acts like the FParsec pipe, except each parser
    /// in the pipe (except the last) is expected to be followed by a comma and whitespace.
    /// </summary>
    let arglist2 a1 a2 fn = pipe2 (arg a1) (a2) fn 

    let arglist3 a1 a2 a3 fn = pipe3 (arg a1) (arg a2) a3 fn 

    let arglist4 a1 a2 a3 a4 fn = pipe4 (arg a1) (arg a2) (arg a3) a4 fn

    let arglist5 a1 a2 a3 a4 a5 fn = pipe5 (arg a1) (arg a2) (arg a3) (arg a4) a5 fn

    /// <summary>
    /// The structure for a declaration.
    /// </summary>
    let let_binding (typename: string) (p: Parser<'a,scene_state>) storefn : Parser<unit,scene_state> =
        let binding = (((pstring "let") >>. spaces >>. (pstring typename) >>. symbol) 
                       .>>. 
                       ((pstring "=") >>. spaces >>. p)) .>> spaces
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

    // ------------------------------------------------------------------------------
    // Colour parsing
    // ------------------------------------------------------------------------------

    /// <summary>
    /// A literal colour specification of the form {r, g, b}
    /// </summary>
    let colour_literal = 
        block (arglist3 expression
                        expression 
                        expression
                        (fun r g b -> {r = r; g = g; b = b}))

    /// <summary>
    /// A symbolic reference to a predefined colour object.
    /// </summary>
    let colour_symbol = 
        fun stream -> 
            match symbol stream with
            | r when r.Status = Ok -> 
                let s = stream.UserState
                match Map.tryFind r.Result s.colours with
                | Some c -> Reply(c)
                | None -> (failFatally (sprintf "undefined colour: %s" r.Result)) stream
            | r -> Reply(Error, r.Error)

    let colour = colour_literal <|> colour_symbol 

    let private store_colour (name: string, colour: colour) : Parser<unit,scene_state> = 
        fun stream ->
            let state = stream.UserState
            match Map.containsKey name state.colours with
            | true -> (failFatally (sprintf "colour %s already defined" name)) stream
            | false ->
                let colours' = Map.add name colour state.colours
                let state' = {state with colours = colours'}
                (setUserState state') stream       
               
    /// <summary>
    /// A colur declaration of the form "let colour cname = {r, g, b}".
    /// <summary>
    let colour_declaration = let_binding "colour" colour_literal store_colour

    // ------------------------------------------------------------------------------
    // Pigment Parsing
    // ------------------------------------------------------------------------------

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

    // ------------------------------------------------------------------------------
    // Finish (i.e. lighting parameters) Parsing
    // ------------------------------------------------------------------------------

    let private highlight = block (arglist2 (named_value "intensity" pfloat) 
                                            (named_value "size" pfloat)
                                            (fun i s -> {intensity = i; size = s}))

    /// <summary>
    /// Parses a literal finish obje t embedded in the scene file.
    /// </summary>
    let finish_literal = 
        block (arglist5 (named_value "opacity" pfloat)
                        (named_value "reflection" pfloat)
                        (named_value "ambient" pfloat)
                        (named_value "diffuse" pfloat)
                        (named_value "highlight" highlight)
                        (fun o r a d h -> {opacity = o; reflection = r; ambient = a; diffuse = d; highlight = h}))

    /// <summary>
    /// Parses a symbolic reference to a finish object, and locates and returns the 
    /// referenced finish object. The parser will fail if no such named finish exists.
    /// </summary>
    let finish_symbol = 
        fun stream -> 
            match symbol stream with
            | r when r.Status = Ok -> 
                let s = stream.UserState
                match s.GetFinish r.Result with
                | Some f -> Reply(f)
                | None -> stream |> failFatally (sprintf "undefined finish: %s" r.Result)
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
            | true -> stream |> failFatally (sprintf "finish %s already defined" name)
            | false ->
                let finishes' = Map.add name finish state.finishes
                let state' = {state with finishes = finishes'}
                (setUserState state') stream       

    /// <summary>
    /// Allows the user to define a named finish, using the form "let finish name = { ..."
    /// </summary>
    let finish_declaration = let_binding "finish" finish_literal store_finish 

    // ------------------------------------------------------------------------------
    // Material Parsing
    // ------------------------------------------------------------------------------

    let material, private materialImpl = createParserForwardedToRef()

    let material_solid = 
        named_block "solid"
            (arglist2 (named_value "pigment" pigment)
                      (named_value "finish" finish)
                      (fun p f -> Solid (p, f)))

    let material_checkerboard =
        named_block "checkerboard"
            (arglist2 (named_value "1" material)
                      (named_value "2" material)
                      (fun a b -> Checkerboard (a, b)))

    let private material_literal = 
        material_solid <|> material_checkerboard

    /// <summary>
    /// Parses a symbolic reference to a material object, and locates and returns the 
    /// referenced material. The parser will fail if no such named material exists.
    /// </summary>
    let private material_symbol = 
        fun stream -> 
            match symbol stream with
            | r when r.Status = Ok -> 
                let s = stream.UserState
                match s.GetMaterial r.Result with
                | Some m -> Reply(m)
                | None -> stream |> failFatally (sprintf "undefined material: %s" r.Result)
            | r -> Reply(Error, r.Error)

    do materialImpl := material_literal <|> material_symbol

    /// <summary>
    /// Returns a parser that stores the suppiled material in the scene_state, iff a material 
    /// with the same name hasn't already been defined.
    /// </summary>
    let private store_material (name: string, material: material) : Parser<unit, scene_state> =
        fun stream ->
            let state = stream.UserState
            match Map.containsKey name state.finishes with
            | true -> stream |> failFatally (sprintf "finish %s already defined" name)
            | false ->
                let materials' = Map.add name material state.materials
                let state' = {state with materials = materials'}
                (setUserState state') stream       


    /// <summary>
    /// Allows the user to define a named material, using the form 
    /// "let material name = material_type { ... }"
    /// </summary>
    let material_declaration = let_binding "material" material store_material

    // ------------------------------------------------------------------------------
    // Geometry Primitives
    // ------------------------------------------------------------------------------

    /// <summary>
    /// Parses a sphere primitive
    /// </summary>  
    let sphere = // sphere { centre: vector, radius: float }
        named_block "sphere" (arglist3 (named_value "centre" vector)
                                       (named_value "radius" expression)
                                       (named_value "material" material)
                                       (fun c r mat -> 
                                         let p = Sphere {centre = c; radius = r}
                                         in { primitive = p; material = mat} ))

    /// <summary>
    /// Parses an an infinite plane 
    /// </summary>
    let plane = // plane { normal: vector, offset: float }
        named_block "plane" (arglist3 (named_value "normal" unit_vector)
                                      (named_value "offset" expression)
                                      (named_value "material" material)
                                      (fun n offs mat -> 
                                        let p = Plane { normal = n; offset = offs }
                                        in { primitive = p; material = mat }))

    /// <summary>
    /// A plane bounded by some world-space constraints
    /// </summary>
    let bounded_plane =
        named_block "bounded_plane" 
            (arglist5 (named_value "normal" unit_vector)
                      (named_value "offset" expression)
                      (named_value "min" vector)
                      (named_value "max" vector)
                      (named_value "material" material)
                      (fun n o min max mat -> 
                        let plane = {plane.normal = n; offset = o}
                        let p = BoundedPlane {plane = plane; min = min; max = max}
                        in { primitive = p; material = mat }))

    let box = 
        named_block "box"
            (arglist3 (named_value "min" vector)
                      (named_value "max" vector)
                      (named_value "material" material)
                      (fun min max mat ->
                         let b = Box {lower = min; upper = max}
                         in { primitive = b; material = mat })) 

    let primitive = sphere <|> plane <|> bounded_plane <|> box

    let store_object obj = 
        fun (stream: CharStream<scene_state>) -> 
            let state = stream.UserState
            let objs' = obj :: state.scene.objects
            let scene' = {state.scene with objects = objs'}
            (setUserState {state with scene = scene'}) stream

    let obj = primitive >>= store_object

    // ------------------------------------------------------------------------------
    // Lights
    // ------------------------------------------------------------------------------

    let point_light = 
        named_block "point_light" 
            (arglist2 (named_value "location" vector)
                      (named_value "colour" colour)
                      (fun loc col -> PointSource {location = loc; colour = col}))

    let store_light l = 
        fun (stream: CharStream<scene_state>) ->
            let state = stream.UserState 
            let lights' = l :: state.scene.lights 
            let scene' = {stream.UserState.scene with lights = lights'}
            (setUserState {state with scene = scene'}) stream

    let light = (point_light) >>= store_light

    // ------------------------------------------------------------------------------
    // Camera
    // ------------------------------------------------------------------------------

    /// <summary>
    /// Parses a camera declaration of the form "camera { ... }"
    /// </summary>
    let camera_declaration : Parser<camera, scene_state> = 
        named_block "camera"
            (arglist5 (named_value "location" vector)
                      (named_value "look_at" vector)
                      (named_value "up" unit_vector)
                      (named_value "h_fov" expression)
                      (named_value "v_fov" expression)
                      (fun loc lookat up hfov vfov -> 
                        let h = to_radians (hfov * 1.0<degrees>)
                        let v = to_radians (vfov * 1.0<degrees>)
                        let cam  = { default_camera with location = loc; 
                                                         up = up;
                                                         horizontal_fov = h;
                                                         vertical_fov = v }
                        look_at lookat cam))

    /// <summary>
    /// Updates the state object with the new camera. Fails if a non-default camera has
    /// already been defined.
    /// </summary>
    let store_camera cam : Parser<unit,scene_state> = 
        fun (stream: CharStream<scene_state>) ->
            let state = stream.UserState
            match state.camera_defined with 
            | true -> Reply(Error, messageError("Camera already defined"))
            | false -> 
                let scene' = {state.scene with camera = cam}
                let state' = {state with camera_defined = true; scene = scene'}
                (setUserState state') stream

    /// <summary>
    /// Parses a camera declaration and updates the state block. There may only be one
    /// camera definition per scene.
    /// </summary>
    let camera : Parser<unit,scene_state> = camera_declaration >>= store_camera

    // ------------------------------------------------------------------------------
    // Top level constructs
    // ------------------------------------------------------------------------------

    /// <summary>
    /// Any sort of let ... binding
    /// </summary>
    let declaration = (attempt colour_declaration) <|> 
                      (attempt finish_declaration) <|> 
                      (attempt material_declaration)

    let scene = (many (declaration <|> camera <|> light <|> obj <|> comment)) .>> eof

    /// <summary>
    /// Parses a scene object out of a stream
    /// </summary> 
    let parse_scene (stream: Stream) (name: string) : scene = 
        match runParserOnStream scene (scene_state.empty) name stream Encoding.UTF8 with
        | Success (_,state,_) -> state.scene 
        | Failure (err,_,_) -> failwith err

