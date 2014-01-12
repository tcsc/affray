module Scenefile

open Affray.Scenefile
open Affray.Geometry
open Affray.Material
open Affray.Math
open Scene

open NUnit.Framework
open FParsec

[<TestFixture>]
type Geometry() = class

    [<Test>]
    member self.Vector () = 
        match run vector "{1, 2, 3}" with
        | Failure (err, _, _) -> Assert.Fail err
        | Success (v, _, _) -> 
            Assert.That (v.x, Is.EqualTo(1.0).Within(1e-8))
            Assert.That (v.y, Is.EqualTo(2.0).Within(1e-8))
            Assert.That (v.z, Is.EqualTo(3.0).Within(1e-8))

    [<Test>]
    member self.VectorWithOddSpacing () = 
        match run vector "{ 1,\r\n2 , 3   }" with
        | Failure (err, _, _) -> Assert.Fail err
        | Success (v, _, _) -> 
            Assert.That (v.x, Is.EqualTo(1.0).Within(1e-8))
            Assert.That (v.y, Is.EqualTo(2.0).Within(1e-8))
            Assert.That (v.z, Is.EqualTo(3.0).Within(1e-8))

    [<Test>]
    member self.VectorWithNegatives () = 
        match run vector "{-1, -2, -3}" with
        | Failure (err, _, _) -> Assert.Fail err
        | Success (v, _, _) -> 
            Assert.That (v.x, Is.EqualTo(-1).Within(1e-8))
            Assert.That (v.y, Is.EqualTo(-2).Within(1e-8))
            Assert.That (v.z, Is.EqualTo(-3).Within(1e-8))

    [<Test>]
    member self.VectorWithFractions () = 
        match run vector "{1.3, 2.2, 3.1}" with
        | Failure (err, _, _) -> Assert.Fail err
        | Success (v, _, _) -> 
            Assert.That (v.x, Is.EqualTo(1.3).Within(1e-8))
            Assert.That (v.y, Is.EqualTo(2.2).Within(1e-8))
            Assert.That (v.z, Is.EqualTo(3.1).Within(1e-8))

    [<Test>]
    member self.Sphere () = 
        match run sphere "sphere { centre: {1, 2, 3}, radius: 5 }" with
        | Failure (err,_,_) -> Assert.Fail err
        | Success (p, _, _) -> 
            match p with
            | Sphere s -> 
                let expected = {sphere.centre = {x = 1.0; y = 2.0; z = 3.0}; radius = 5.0}
                Assert.AreEqual (expected, s)
            | _ -> Assert.Fail()
            

    [<Test>]
    member self.Plane () = 
        match run plane "plane { normal: {1, 2, 3}, offset: 42}" with
        | Failure (err, _, _) -> Assert.Fail err
        | Success (p, _, _) -> 
            match p with
            | Plane pl -> 
                let expected = {normal = normalize {x = 1.0; y = 2.0; z = 3.0}; offset = 42.0}
                Assert.That (pl.offset, Is.EqualTo(expected.offset).Within(1e-8))
                Assert.That (pl.normal.x, Is.EqualTo(expected.normal.x).Within(1e-8))
                Assert.That (pl.normal.y, Is.EqualTo(expected.normal.y).Within(1e-8))
                Assert.That (pl.normal.z, Is.EqualTo(expected.normal.z).Within(1e-8))
            | _ -> Assert.Fail ()
end

[<TestFixture>]
type Material() = class
    [<Test>]
    member self.ParseColourMap () = 
        let text = "[(0, {0,0,0}),\r\n" +  
                   " (0.25, {1.0, 0.0, 0.0}),\n" + 
                   " (0.50, {0.0, 1.0, 0.0})," +
                   " (0.75, {0.0, 0.0, 1.0})," + 
                    "(1,{1,1,1})]"
        match run colour_map text with 
        | Failure (err,_,_) -> Assert.Fail err
        | Success (m,_,_) -> ()

    [<Test>]
    member self.Pigment_Gradient () = 
        let text = "gradient { direction: {3, 2, -1}, colours: [(0, {0,0,0}), (1, {1,1,1})] }"
        match run gradient_pigment text with
        | Failure (err,_,_) -> Assert.Fail err
        | Success (p,_,_) -> ()

    [<Test>]
    member self.Material_Checkerboard () = 
        let text = "checkerboard {\n"                                            + 
                   "  1: solid { pigment: {1, 0, 0},\n"                          + 
                   "             finish: { opacity: 1,\n"                        +
                   "                       reflection: 0,\n"                     +
                   "                       ambient: 0.1,\n"                      +
                   "                       diffuse: 1.0,\n"                      + 
                   "                       highlight: { intensity: 1.0,\n"       + 
                   "                                    size: 60.0\n"            +
                   "                       }\n"                                  +
                   "              }\n"                                           + 
                   "  },\n"                                                      + 
                   "  2: solid { pigment: gradient { direction: {3, 2, 1},\n"    + 
                   "                                 colours: [ (0, {0,0,0}),\n" + 
                   "                                            (1, {1,1,1}) ]"  + 
                   "                      },\n"                                  +
                   "             finish: { opacity: 0.5,\n"                      +
                   "                       reflection: 1,\n"                     +
                   "                       ambient: 0.4,\n"                      +
                   "                       diffuse: 1.5,\n"                      + 
                   "                       highlight: { intensity: 2.0,\n"       + 
                   "                                    size: 0.0\n"             +
                   "                       }\n"                                  +
                   "              }\n"                                           + 
                   "  }\n"                                                       +
                   "}"
        match run (material_checkerboard ()) text with
        | Failure (err, _, _) -> Assert.Fail err
        | Success (p, _, _) ->
            match p with 
            | Checkerboard (a, b) -> ()
            | _ -> Assert.Fail "Incorrect material type"

end
               