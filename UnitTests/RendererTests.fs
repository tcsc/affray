module Renderer

open NUnit.Framework
open Geometry
open System

[<TestFixture>]
type RaySphereIntersection() = class

    [<Test>]
    member self.Simple() = 
        let s = {centre = {x = 0.0; y = 0.0; z = 0.0}; radius = 1.0}
        let r = {
            src = {x = 5.0; y = 0.0; z = 0.0}; 
            direction = {x = -1.0; y = 0.0; z = 0.0}}
        Assert.AreEqual(Some 4.0, ray_sphere_intersection r s)
    
    [<Test>]    
    member self.OffCentre() = 
        let s = {centre = {x = 0.0; y = 0.0; z = 0.0}; radius = 1.0}
        let r = {
            src = {x = 10.0; y = 10.0; z = 0.0}; 
            direction = unitize {x = -10.0; y = -10.0; z = 0.5}}
        let value = ray_sphere_intersection r s 
        match ray_sphere_intersection r s with 
        | None -> Assert.Fail("Expcted a value")
        | Some n -> 
            Assert.Less(n, 14.14)
            Assert.Greater(n, 13.14)
        
    [<Test>]
    member self.NoIntersection() = 
        let s = {centre = {x = 0.0; y = 5.0; z = 0.0}; radius = 1.0}
        let r = {
            src = {x = 10.0; y = 10.0; z = 0.0}; 
            direction = unitize {x = -10.0; y = -10.0; z = 0.5}}
        match ray_sphere_intersection r s with
        | Some n -> Assert.Fail("Expected no intersection")
        | _ -> ()
end