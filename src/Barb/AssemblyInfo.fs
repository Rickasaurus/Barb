namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyTitleAttribute("Barb")>]
[<assembly: AssemblyProductAttribute("Barb")>]
[<assembly: AssemblyDescriptionAttribute("A Simple Dynamic Scripting Language for .NET")>]
[<assembly: AssemblyVersionAttribute("1.0.1")>]
[<assembly: AssemblyFileVersionAttribute("1.0.1")>]
[<assembly: InternalsVisibleToAttribute("Barb.Tests")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0.1"
