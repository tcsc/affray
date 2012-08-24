module Program

open System
open System.Drawing
open System.Drawing.Imaging
open System.Diagnostics

open Scene
open Colour
open Renderer

let scene =
    let o = Sphere {centre = {x = 0.0; y = 0.0; z = -5.0}; radius = 1.0}
    let l = PointSource {location = {x = 100.0; y = 100.0; z = -100.0}; colour = white}
    {default_scene with objects = [o]; lights = [l]}
      
do
    let output = new Bitmap(640, 480, PixelFormat.Format32bppPArgb)
    let cam' = set_aspect_ratio 1.333 default_camera.horizontal_fov default_camera

    let timer = new Stopwatch()
    timer.Start()
    render_to_bitmap output {scene with camera = cam'}
    timer.Stop()

    output.Save("pic.png", ImageFormat.Png)

    printfn "Render took %d ms" timer.ElapsedMilliseconds