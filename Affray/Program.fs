module Program

open System
open System.Drawing
open System.Drawing.Imaging
open System.Diagnostics
open System.IO

open CommandLine

open Affray.Colour
open Affray.Material
open Affray.Math
open Affray.Renderer
open Affray.Scenefile

open Scene

type options () = 
    let mutable scene_file = System.String.Empty
    let mutable output_file = System.String.Empty
    let mutable width = 1024
    let mutable height = 768

    [<Option("s", "scene", Required=true, HelpText="The scene to render")>]
    member self.SceneFile
        with get() = scene_file 
        and set(v) = scene_file <- v

    [<Option("o", "output", DefaultValue="image.png", HelpText="Where to write the output image")>]
    member self.OutputFile
        with get() = output_file
        and set(v) = output_file <- v

    [<Option("w", "width", DefaultValue=1024, HelpText="The width of the image to render")>]
    member self.Width
        with get() = width
        and set(v) = width <- v

    [<Option("h", "height", DefaultValue=768, HelpText="The height of the image to render")>]
    member self.Height
        with get() = height
        and set(v) = height <- v
    
let parse_command_line argv = 
    let args = new options()
    let parser = new CommandLineParser()
    match parser.ParseArguments(argv, args) with
    | true -> Some args
    | false ->
        // print usage 
        None

let load_scene filename = 
    printfn "Loading %s" filename
    use f = new FileStream(filename, FileMode.Open, FileAccess.Read)
    parse_scene f filename
      
[<EntryPoint>]
let main argv =
    match parse_command_line argv with
    | None -> -1
    | Some args ->
        let output = new Bitmap(args.Width, args.Height, PixelFormat.Format32bppPArgb)
        let cam = 
            {default_camera with location = {x = 0.0; y = 10.0; z = 15.0}; up = positive_y}
            |> set_aspect_ratio 1.333 default_camera.horizontal_fov
            |> look_at {x = 0.0; y = 2.0; z = 0.0}

        let scene = load_scene args.SceneFile

        let timer = new Stopwatch()
        timer.Start()
        render_to_bitmap output scene
        timer.Stop()

        output.Save(args.OutputFile)

        printfn "Render took %d ms" timer.ElapsedMilliseconds
        0
