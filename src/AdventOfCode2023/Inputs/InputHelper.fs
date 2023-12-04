module AdventOfCode2023.Inputs

open System.IO
open System.Text

type InputHelper =
    static member GetResourceAsString(name: string) =
        let tp = typedefof<InputHelper>
        let result = tp.Assembly.GetManifestResourceStream("AdventOfCode2023.Inputs." + name)
        if isNull result
            then failwith $"resource {name} not found" 
        use reader = new StreamReader(result, Encoding.UTF8)
        reader.ReadToEnd()

type Input(day: int) =
    member this.GetData(suffix: string) =
        InputHelper.GetResourceAsString $"day%02i{day}-%s{suffix}.txt"
        
    member this.TestData with get() = this.GetData("test")
    
    member this.RealData with get() = this.GetData("real")
