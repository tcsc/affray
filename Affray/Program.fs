module Program

open System
open System.Drawing
open System.Drawing.Imaging
open System.Diagnostics

open CommandLine

open Affray.Colour
open Scene
open Renderer

let scene =
    let default_material = {
            reflection = 0.5;
            colour = white; 
            opacity = 1.0;
            ambient = 0.15; 
            diffuse = 1.0;
            highlight = {intensity = 1.0; size = 60.0}
        }
    let o = { 
        primitive = Sphere {centre = {x = 2.0; y = 1.0; z = 2.0}; radius = 0.5};
        material = {default_material with colour = red}
    }
    let o2 = { 
        primitive = Sphere {centre = {x = 3.0; y = 0.0; z = 1.0}; radius = 1.0};
        material = {default_material with colour = green; reflection = 0.25}
    }
    let o3 = { 
        primitive = Sphere {centre = {x = -2.0; y = 0.0; z = 4.0}; radius = 1.0};
        material = {default_material with colour = blue}
    }
    let o4 = { 
        primitive = Sphere {centre = {x = 0.0; y = 0.0; z = 0.0}; radius = 2.0};
        material = {default_material with reflection = 0.99}
    }
    let o5 = {
        primitive = Plane {normal = {x = 0.0; y = 1.0; z = 0.0}; offset = -5.0};
        material = {default_material with colour = {r = 1.0; g = 1.0; b = 0.0}}
    }
    let o6 = { 
        primitive = Sphere {centre = {x = -2.0; y = 2.0; z = -2.0}; radius = 0.5};
        material = {default_material with colour = red}
    }


    let l1 = PointSource {
        location = {x = 100.0; y = 300.0; z = 100.0}; 
        colour = 0.9 * white
    }
    let l2 = PointSource {
        location = {x = -10.0; y = -0.5; z = 10.0}
        colour = 0.25 * white
    }
    {default_scene with objects = [o; o2; o3; o4; o5; o6]; lights = [l1]}

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

    [<Option("w", "width", DefaultValue="1024", HelpText="The wdth of the image to render")>]
    member self.Width
        with get() = width
        and set(v) = width <- v
      
do
    let output = new Bitmap(1024, 768, PixelFormat.Format32bppPArgb)
    let cam = {default_camera with location = {x = 0.0; y = 10.0; z = 10.0}}
              |> set_aspect_ratio 1.333 default_camera.horizontal_fov
              |> look_at {x = 0.0; y = 0.0; z = 0.0}

    let timer = new Stopwatch()
    timer.Start()
    render_to_bitmap output {scene with camera = cam}
    timer.Stop()

    output.Save("pic.jpg", ImageFormat.Jpeg)

    printfn "Render took %d ms" timer.ElapsedMilliseconds