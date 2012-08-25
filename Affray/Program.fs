module Program

open System
open System.Drawing
open System.Drawing.Imaging
open System.Diagnostics

open Affray.Colour
open Scene
open Renderer

let scene =
    let o = { 
        primitive = Sphere {centre = {x = 0.0; y = 0.0; z = -5.0}; radius = 1.0};
        material = {colour = red; opacity = 1.0; ambient = 0.15}
    }
    let o2 = { 
        primitive = Sphere {centre = {x = -1.5; y = 0.0; z = -6.0}; radius = 1.0};
        material = {colour = green; opacity = 1.0; ambient = 0.15}
    }
    let o3 = { 
        primitive = Sphere {centre = {x = 1.5; y = 0.0; z = -4.0}; radius = 1.0};
        material = {colour = blue; opacity = 1.0; ambient = 0.15}
    }

    let l = PointSource {location = {x = 100.0; y = 100.0; z = 100.0}; colour = white}
    {default_scene with objects = [o; o2; o3]; lights = [l]}
      
do
    let output = new Bitmap(640, 480, PixelFormat.Format32bppPArgb)
    let cam' = set_aspect_ratio 1.333 default_camera.horizontal_fov default_camera

    let timer = new Stopwatch()
    timer.Start()
    render_to_bitmap output {scene with camera = cam'}
    timer.Stop()

    output.Save("pic.png", ImageFormat.Png)

    printfn "Render took %d ms" timer.ElapsedMilliseconds