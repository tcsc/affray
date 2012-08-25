module Program

open System
open System.Drawing
open System.Drawing.Imaging
open System.Diagnostics

open Affray.Colour
open Scene
open Renderer

let scene =
    let default_material = {
            colour = white; 
            opacity = 1.0;
            ambient = 0.15; 
            diffuse = 1.0;
            highlight = {intensity = 1.0; size = 60.0}
        }
    let o = { 
        primitive = Sphere {centre = {x = 0.0; y = 0.0; z = -5.0}; radius = 1.0};
        material = {default_material with colour = red}
    }
    let o2 = { 
        primitive = Sphere {centre = {x = -1.5; y = 0.0; z = -6.0}; radius = 1.0};
        material = {default_material with colour = green}
    }
    let o3 = { 
        primitive = Sphere {centre = {x = 1.5; y = 0.0; z = -4.0}; radius = 1.0};
        material = {default_material with colour = blue}
    }
    let l1 = PointSource {
        location = {x = 100.0; y = 300.0; z = 100.0}; 
        colour = 0.9 * white
    }
    let l2 = PointSource {
        location = {x = -10.0; y = -0.5; z = 10.0}
        colour = 0.25 * white
    }
    {default_scene with objects = [o; o2; o3]; lights = [l1; l2]}
      
do
    let output = new Bitmap(640, 480, PixelFormat.Format32bppPArgb)
    let cam' = set_aspect_ratio 1.333 default_camera.horizontal_fov default_camera
    let cam'' = {cam' with location = {x = 0.0; y = 0.0; z = 1.1}}

    let timer = new Stopwatch()
    timer.Start()
    render_to_bitmap output {scene with camera = cam''}
    timer.Stop()

    output.Save("pic.png", ImageFormat.Png)

    printfn "Render took %d ms" timer.ElapsedMilliseconds