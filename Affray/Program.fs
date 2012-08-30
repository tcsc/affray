module Program

open System
open System.Drawing
open System.Drawing.Imaging
open System.Diagnostics

open CommandLine

open Affray.Colour
open Affray.Material
open Affray.Math
open Affray.Renderer

open Scene

let scene =
    let default_finish = {
            reflection = 0.5;
            opacity = 1.0;
            ambient = 0.15; 
            diffuse = 1.0;
            highlight = {intensity = 1.0; size = 60.0}
        }

    let checkerboard_material = 
        Checkerboard (
            Solid (Colour black, default_finish),
            Solid (Colour {r = 0.8; g = 0.8; b = 0.8}, default_finish))

    let objects = [
        { 
            primitive = Sphere {centre = {x = 2.0; y = 2.0; z = 2.0}; radius = 0.5};
            material = Solid (Colour green, {default_finish with reflection = 0.99})
        }; { 
            primitive = Sphere {centre = {x = 3.0; y = 1.0; z = 1.0}; radius = 1.0};
            material = Solid (Colour green, {default_finish with reflection = 0.25})
        }; { 
            primitive = Sphere {centre = {x = -2.0; y = 1.0; z = 4.0}; radius = 1.0};
            material = Solid (Colour blue, default_finish)
        }; { 
            primitive = Sphere {centre = {x = 0.0; y = 2.0; z = 0.0}; radius = 2.0};
            material = Checkerboard (
                            Solid (Colour red, {default_finish with reflection = 0.0}),
                            Solid (Colour white, {default_finish with reflection = 0.75}))
       }; { 
            primitive = Sphere {centre = {x = -2.5; y = 1.0; z = -2.0}; radius = 0.5};
            material = Solid (Colour red, default_finish)
       }; {
            primitive = BoundedPlane {
                            plane = {normal = {x = 0.0; y = 1.0; z = 0.0}; offset = -0.01};
                            min = {x = -5.0; y = -0.1; z = -5.0; }
                            max = {x = 5.0; y = 0.1; z = 5.0; }
                        }
            material = checkerboard_material
       }; { 
            primitive = Sphere {centre = {x = 4.0; y = 3.0; z = -5.0}; radius = 1.0};
            material = Solid (Colour blue, default_finish)
       };]


    let l1 = PointSource {
        location = {x = 100.0; y = 300.0; z = 100.0}; 
        colour = 0.9 * white
    }
    let l2 = PointSource {
        location = {x = -10.0; y = -0.5; z = 10.0}
        colour = 0.25 * white
    }

    {default_scene with 
        objects = objects; 
        lights = [l1] 
        sky = Gradient (positive_y, [
                    (0.0,   {r=0.1; g=0.1; b=0.4});
                    (1.0,   {r=0.0; g=0.0; b=0.0})
                  ])}

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

        let timer = new Stopwatch()
        timer.Start()
        render_to_bitmap output {scene with camera = cam}
        timer.Stop()

        output.Save(args.OutputFile)

        printfn "Render took %d ms" timer.ElapsedMilliseconds
        0