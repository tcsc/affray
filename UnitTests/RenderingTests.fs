module Rendering

open Microsoft.FSharp.Collections
open NUnit.Framework
open System

open Affray.Renderer
open Affray.Math
open Affray.Geometry


[<TestFixture>]
type Reflection() = class

    [<Test>]
    member self.Angled() =
        let r = {
            src = {x = -1.0; y = 1.0; z = -1.0};
            direction = normalize {x = 1.0; y = -1.0; z = 1.0}
        }

        let r' = reflect_ray r {x = 0.0; y = 0.0; z = -1.0} {x = 0.0; y = 0.0; z = 0.0}

        let expected = {
            src = {x = 0.0; y = 0.0; z = 0.0;}
            direction = normalize {x = 1.0; y = -1.0; z = -1.0}
        }

        Assert.AreEqual(r', expected)

end
                 
