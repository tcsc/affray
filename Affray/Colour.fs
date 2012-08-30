namespace Affray 

open System.Diagnostics

module Colour = 
    [<DebuggerDisplay("r = {r}, g = {g}, b = {b}")>]
    type colour = 
        {r: double; g: double; b: double }
        static member ( * ) (lhs: colour, rhs: colour) = 
            {r = lhs.r * rhs.r; g = lhs.g * rhs.g; b = lhs.b * rhs.b}

        static member ( * ) (lhs: colour, rhs: float) = 
            {r = lhs.r * rhs; g = lhs.g * rhs; b = lhs.b * rhs}

        static member ( * ) (lhs: float, rhs: colour) = 
            {r = lhs * rhs.r; g = lhs * rhs.g; b = lhs * rhs.b}

        static member ( + ) (lhs: colour, rhs: colour) = 
            {r = lhs.r + rhs.r; g = lhs.g + rhs.g; b = lhs.b + rhs.b}

        static member ( - ) (lhs: colour, rhs: colour) = 
            {r = lhs.r - rhs.r; g = lhs.g - rhs.g; b = lhs.b - rhs.b}

    let clamp (c: colour) = 
        let r' = min 1.0 c.r |> max 0.0
        let g' = min 1.0 c.g |> max 0.0
        let b' = min 1.0 c.b |> max 0.0
        {r = r'; g = g'; b = b'}

    let white = {r = 1.0; g = 1.0; b = 1.0}

    let black = {r = 0.0; g = 0.0; b = 0.0}

    let red = {r = 1.0; g = 0.0; b = 0.0}

    let green = {r = 0.0; g = 1.0; b = 0.0}

    let blue = {r = 0.0; g = 0.0; b = 1.0}

    let yellow = {r = 1.0; g = 1.0; b = 0.0}
