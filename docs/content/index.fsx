(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/Barb"

(**
Barb is a simple dynamic data scripting language for .NET
======================

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The Barb library can be <a href="https://nuget.org/packages/Barb">installed from NuGet</a>:
      <pre>PM> Install-Package Barb</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Example
-------

Getting started with Barb is easy!

*)
#r "Barb.dll"
open Barb.Compiler

let func = buildExpr<unit,string> "Hello World!"

printfn "%s" (func ())

(**

Samples & documentation
-----------------------

 * [Tutorial](tutorial.html) contains a further explanation of how to use Barb.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under the Apache 2.0 license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fsprojects/Barb/tree/master/docs/content
  [gh]: https://github.com/fsprojects/Barb
  [issues]: https://github.com/fsprojects/Barb/issues
  [readme]: https://github.com/fsprojects/Barb/blob/master/README.md
  [license]: https://github.com/fsprojects/Barb/blob/master/LICENSE.txt
*)
