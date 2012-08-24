module Maths

open Microsoft.FSharp.Collections
open NUnit.Framework
open System

open Affray.Math

[<TestFixture>]
type Matrix() = class
    [<Test>]
    member self.IdentitySquared() = 
        let identity = matrix.Identity
        let result = identity * identity
        Assert.IsTrue ((result = identity))
        
    [<Test>]
    member self.MultiplyByIdentity() = 
        let m = new matrix(Array2D.init 4 4 (fun i j -> float(i - j)))
        let result = m * matrix.Identity
        Assert.IsTrue ((m = result))
end