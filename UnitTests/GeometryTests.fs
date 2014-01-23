module Geometry
open System
open NUnit.Framework

open Affray.Geometry
open Affray.Primitive
open Affray.Math
open Affray.Material

[<TestFixture>]
type RaySphereIntersection() = class

    [<Test>]
    member self.Simple() = 
        let s = Sphere ({x = 0.0; y = 0.0; z = 0.0}, 1.0, default_material)
        let r = {
            src = {x = 5.0; y = 0.0; z = 0.0}; 
            direction = {x = -1.0; y = 0.0; z = 0.0}}
        Assert.AreEqual(Some 4.0, s.RayIntersection r)
    
    [<Test>]    
    member self.OffCentre() = 
        let s = Sphere({x = 0.0; y = 0.0; z = 0.0}, 1.0, default_material)
        let r = {
            src = {x = 10.0; y = 10.0; z = 0.0}; 
            direction = normalize {x = -10.0; y = -10.0; z = 0.5}}
        match s.RayIntersection r with 
        | None -> Assert.Fail("Expcted a value")
        | Some n -> 
            Assert.Less(n, 14.14)
            Assert.Greater(n, 13.14)
        
    [<Test>]
    member self.NoIntersection() = 
        let s = Sphere({x = 0.0; y = 5.0; z = 0.0}, 1.0, default_material)
        let r = {
            src = {x = 10.0; y = 10.0; z = 0.0}; 
            direction = normalize {x = -10.0; y = -10.0; z = 0.5}}
        match s.RayIntersection r with
        | Some n -> Assert.Fail("Expected no intersection")
        | _ -> ()
end

[<TestFixture>]
type RayPlaneIntersection() = class

    [<Test>]
    member self.Simple() = 
        let p = Plane({x = 0.0; y = 0.0; z = 1.0}, -10.0, default_material)
        let r = {
            src = {x = 0.0; y = 0.0; z = 0.0};
            direction = {x = 0.0; y = 0.0; z = -1.0}
        }

        match p.RayIntersection r with
        | Some n -> Assert.True((n - 10.0) < 1e-3, "Expeceted ~10, got {0}", n)
        | None -> Assert.Fail("Should be ");

    [<Test>]
    member self.NoIntersection() = 
        let p = Plane ({x = 0.0; y = 0.0; z = 1.0}, -10.0, default_material)
        let r = {
            src = {x = 0.0; y = 0.0; z = 0.0};
            direction = {x = 0.0; y = 1.0; z = 0.0}
        }

        match p.RayIntersection r with
        | None -> Assert.True(true, "Should not intersect");
        | Some n -> Assert.Fail("Expeceted no inersection, got one at{0}", n)

     
end