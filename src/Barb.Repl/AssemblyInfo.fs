namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Barb.Repl")>]
[<assembly: AssemblyProductAttribute("Barb")>]
[<assembly: AssemblyDescriptionAttribute("A Simple Dynamic Scripting Language for .NET")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
