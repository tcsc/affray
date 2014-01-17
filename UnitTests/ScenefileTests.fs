module Scenefile

open Affray.Colour
open Affray.Scenefile
open Affray.Geometry
open Affray.Material
open Affray.Math
open Scene

open NUnit.Framework
open FParsec

let test p str = runParserOnString p scene_state.empty "text" str
let testWithState p str s = runParserOnString p s "text" str

[<TestFixture>]
type Geometry() = class

    [<Test>]
    member self.Vector () = 
        match test vector "{1, 2, 3}" with
        | Failure (err, _, _) -> Assert.Fail err
        | Success (v, _, _) -> 
            Assert.That (v.x, Is.EqualTo(1.0).Within(1e-8))
            Assert.That (v.y, Is.EqualTo(2.0).Within(1e-8))
            Assert.That (v.z, Is.EqualTo(3.0).Within(1e-8))

    [<Test>]
    member self.VectorWithOddSpacing () = 
        match test vector "{ 1,\r\n2 , 3   }" with
        | Failure (err, _, _) -> Assert.Fail err
        | Success (v, _, _) -> 
            Assert.That (v.x, Is.EqualTo(1.0).Within(1e-8))
            Assert.That (v.y, Is.EqualTo(2.0).Within(1e-8))
            Assert.That (v.z, Is.EqualTo(3.0).Within(1e-8))

    [<Test>]
    member self.VectorWithNegatives () = 
        match test vector "{-1, -2, -3}" with
        | Failure (err, _, _) -> Assert.Fail err
        | Success (v, _, _) -> 
            Assert.That (v.x, Is.EqualTo(-1).Within(1e-8))
            Assert.That (v.y, Is.EqualTo(-2).Within(1e-8))
            Assert.That (v.z, Is.EqualTo(-3).Within(1e-8))

    [<Test>]
    member self.VectorWithFractions () = 
        match test vector "{1.3, 2.2, 3.1}" with
        | Failure (err, _, _) -> Assert.Fail err
        | Success (v, _, _) -> 
            Assert.That (v.x, Is.EqualTo(1.3).Within(1e-8))
            Assert.That (v.y, Is.EqualTo(2.2).Within(1e-8))
            Assert.That (v.z, Is.EqualTo(3.1).Within(1e-8))

    [<Test>]
    member self.Sphere () = 
        match test sphere "sphere { centre: {1, 2, 3}, radius: 5 }" with
        | Failure (err,_,_) -> Assert.Fail err
        | Success (p, _, _) -> 
            match p with
            | Sphere s -> 
                let expected = {sphere.centre = {x = 1.0; y = 2.0; z = 3.0}; radius = 5.0}
                Assert.AreEqual (expected, s)
            | _ -> Assert.Fail()
            

    [<Test>]
    member self.Plane () = 
        match test plane "plane { normal: {1, 2, 3}, offset: 42}" with
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
        match test colour_map text with 
        | Failure (err,_,_) -> Assert.Fail err
        | Success (m,_,_) -> ()

    [<Test>]
    member self.Pigment_Gradient () = 
        let text = "gradient { direction: {3, 2, -1}, colours: [(0, {0,0,0}), (1, {1,1,1})] }"
        match test gradient_pigment text with
        | Failure (err,_,_) -> Assert.Fail err
        | Success (p,_,_) -> ()

    [<Test>]
    member self.Material_Checkerboard () = 
        let text = "checkerboard {\n"                                            + 
                   "  1: solid { pigment: colour {1, 0, 0},\n"                   + 
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
        match test (material_checkerboard ()) text with
        | Failure (err, _, _) -> Assert.Fail err
        | Success (p, _, _) ->
            match p with 
            | Checkerboard (a, b) -> ()
            | _ -> Assert.Fail "Incorrect material type"

    [<Test>]
    member self.Material () = 
        let text = "solid { pigment: colour {0, 0.5, 1}, "                  +  
                   "        finish: { opacity: 1,\n"                        +
                   "                  reflection: 0,\n"                     +
                   "                  ambient: 0.1,\n"                      +
                   "                  diffuse: 1.0,\n"                      + 
                   "                  highlight: { intensity: 1.0,\n"       + 
                   "                               size: 60.0 }\n"          +
                   "        }\n"                                            +
                   "}\n"
        match test material text with
        | Failure (err,_,_) -> Assert.Fail err
        | Success (p,_,_) -> ()
end

[<TestFixture>]
type NamedObjects () = class
    [<Test>]
    member self.DeclareColor () = 
        let text = "let colour red = { 1, 0, 0 }"
        match test declaration text with
        | Failure (err,_,_) -> Assert.Fail err
        | Success (p,s,_) ->
            Assert.That (s.colours.Count, Is.EqualTo 1)
            Assert.That (Map.find "red" s.colours, Is.EqualTo red)

     [<Test>]
     member self.DeclareFinish () = 
        let text = "let finish dull = { " + 
                   "    opacity: 1,\n"                  +
                   "    reflection: 0,\n"               +
                   "    ambient: 0.1,\n"                +
                   "    diffuse: 1.0,\n"                + 
                   "    highlight: { intensity: 1.0,\n" + 
                   "                 size: 1.0 }\n"     +
                   " }\n"   
        match test declaration text with 
        | Failure (err,_,_) -> Assert.Fail err
        | Success (_, s, _) ->
            Assert.That (s.finishes.Count, Is.EqualTo 1)
            let f = Map.find "dull" s.finishes
            Assert.That (f.opacity,    Is.EqualTo 1.0)
            Assert.That (f.reflection, Is.EqualTo 0.0)
            Assert.That (f.ambient,    Is.EqualTo 0.1)
            Assert.That (f.diffuse,    Is.EqualTo 1.0)

    [<Test>]
    member self.DeclareMaterial () = 
        let text = "let material thingy = solid { " + 
                   "    pigment: colour {1, 1, 1},                                      " +
                   "    finish: { opacity: 1.0, reflection: 2.0, ambient: 3, diffuse: 4," +
                   "              highlight: { intensity: 5, size: 6 }                  " +
                   "    }                                                               " +
                   "}"
        match test declaration text with 
        | Failure(err,_,_) -> Assert.Fail err
        | Success(_,s,_) ->
            Assert.That (s.materials.Count, Is.EqualTo 1)
            match s.GetMaterial "thingy" with
            | None -> Assert.Fail "No such material in the returned state"
            | Some m -> 
                match m with
                | Solid (p, f) ->
                    Assert.That (p, Is.EqualTo (Colour {r = 1.0; g = 1.0; b = 1.0}))
                | _ -> Assert.Fail "Unexpected material"
             
    [<Test>]
    member self.UseDeclaredColour () = 
        let colours = Map.ofList [("red", {r = 1.0; g = 0.0; b = 0.0})]
        let s = {scene_state.empty with colours = colours}
        let text = "solid { pigment: colour red, " + 
                   "        finish: { opacity: 1.0, reflection: 2.0, ambient: 3, diffuse: 4," +
                   "                  highlight: { intensity: 5, size: 6 }                  " +
                   "      }                                                                 " +
                   "}"

        match testWithState material text s with
        | Failure (err,_,_) -> Assert.Fail err
        | Success (p,_,_) ->
            match p with
            | Solid (p, _) -> 
                match p with 
                | Colour c -> Assert.That (c, Is.EqualTo {r = 1.0; g = 0.0; b = 0.0})
                | _ -> Assert.Fail "Bad pigment"
            | _ -> Assert.Fail "Unexpected material"

    [<Test>]
    member self.UseDeclaredFinish () = 
        let dull_finish = {opacity = 1.0; reflection = 0.0; ambient = 1.0; diffuse = 6.0;
                           highlight = {intensity = 0.1; size = 1.0}}
        let finishes = Map.ofList [("dull", dull_finish)]
        let s = {scene_state.empty with finishes = finishes}
        let text = "solid { pigment: colour {0, 0, 0}, finish: dull }"

        match testWithState material text s with
        | Failure (err,_,_) -> Assert.Fail err
        | Success (p,_,_) -> 
            match p with
            | Solid (_, f) -> Assert.That (f, Is.SameAs dull_finish)
            | _ -> Assert.Fail "Unexpected material"

end