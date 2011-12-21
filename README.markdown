Barb is a small dynamic scripting language for .NET with the following properties:

- Syntax that makes both F# and C# devs feel at home
- Fully expression oriented (Whitespace is completely ignored)
- Paren based scoping rules (Lisp-like)
- Pure functional (No mutation allowed)

This makes Barb an easy to learn language which you can give to your users for safely scripting over data in your application.  Best of all, you'll be able to use it without learning a bunch of new syntax.


Why Barb?
---------

I created Barb because I needed a user language for my applications but yet no existing .NET language hosted nicely inside of my application and met my criteria:

- Simple/Terse Syntax
- Expression Oriented (Any program should be able to be represented in a single line of code)
- No casting necessary and transparent type conversions
- Native .NET type system (No painful conversions!)
- Tries really hard to do what a non-technical user would expect
- Easy to maintain
- Fast Execution (I use it to script over sets of records in the millions range)
- Safe from accidental data corruption by non-technical users

Dynamic Linq, IronPython and IronRuby all failed for one of these reasons or another.

The current implementation is still young and a bit rough around the edges. It's was originally designed for writing one-line expressions along these lines:

"(Name.Contains "John" or Name.Contains "Mary") and (Age > 20 or Weight > 200)"

It can do much more now, but still needs work to be friendly enoungh for a real release. In particular, it needs better error messages and a friendly API for hosting.

However, I thought it might be useful to others in its current form.  If you do play with it I hope you'll let me know what you think.


Current Features <a id="features" />
----------------

- Recursive lambda bindings with F# or C# syntax
- Value name binding with F# or C# syntax
- Order of operations 
- Static or instance calls and indexers via cached reflection
- Numerical sequence expressions (Identical to F#'s)
- Tuples (any IEnumerable is treated as a tuple internally)

 
Things To Do <a id="todo" />
------------

- Lambda interop with .NET
- Extension method resolution
- A resonable API with configuration options
- Generalized Sequence Expressions



Planned Limitations <a id="limitations" />
-------------------

- All numbers are converted internally to Int64 or Double.  This will change but was necessary for now to keep 
  things simple.
- Barb may cause unexpected behavior when making calls that mutate things.  I do plan on adding an API option to 
  fix this, but it will tone down optimization significantly.


Examples of Use <a id="examples" />
---------------

The best way to think about Barb is over some collection of records (data classes to you C# folks).

type CustRecord =
    {
		Name: string
		Age: int
		Weight: int
		Source: obj
		Locations: string array
    }

1. As mentioned above, Barb is great for writing queries.
e.g.  let predicate = buildExpr<CustRecord,bool>("Name.Contains "John" and (Age > 20 or Weight > 200)")
This will will return a predicate which you can then use to filter over large numbers of records.


2. It can also be used to for user specified reflection. One case where I use this is for writing out CSV files. 
Given a set of record field specifying strings, you can easily build functions which will grab the specified data.  

e.g. let fld1Getter = buildExpr<CustRecord,int>("Weight") 
Quite simple right? Barb will also convert to the correct output type if able.

e.g. let fld1Getter = buildExpr<CustRecord,string>("Weight") 
Here the integer will be converted on the fly to string because it's what you statically specified.

e.g. let fld1Getter = buildExpr<CustRecord,string>("Source.Url")
Barb can will also reflect into the real type of a given obj, which can be quite handy for taking the hard work out of accessing untyped data.


3. Barb supports many of the constructs a F# user would expect

e.g. buildExpr<CustRecord,string>("Locations.[0]") // The F# style '.' is optional :)
e.g. buildExpr<CustRecord,string>("if Age > 40 then 'Old' else 'Young'")
e.g. buildExpr<CustRecord,int>("let x = Age + 1 in x") // or..
e.g. buildExpr<CustRecord,int>("var x = Age + 1 in x") // (although, I may use var for mutables in the future)

...and some fancier ones from F#

e.g. buildExpr<unit,seq<int>>("{ 1 .. 5 }") // Returns 1,2,3,4,5
e.g. buildExpr<unit,seq<int>>("{ 1 .. 2 .. 10 }") // Returns 1,3,5,7,9
e.g. buildExpr<unit,seq<int>>("(1, 2, 3)") // Returns 1,2,3

...all looping must currently be handled with lambda recursion

e.g. buildExpr<unit,seq<int>>("let recfun = fun x -> if x > 1 then x else recfun (x + 1) in recfun 0") 

...but it's still growing and more features are being added all the time.


Extra Nerdy Stuff Under Here <a id="nerdy" />
----------------------------

Compilation is done by n-tuple merging of expression nodes.  This is done initially to pre-compute as much as possible and the optimized result is then reduced again with the information given by the passed in data object.  Decisions can be made at runtime based on the contents of these tuples and their neighbors.  This allows for a huge amount of flexibility in deciding what to do.  Function/Value composition also allows for fast code to be built out of existing .NET constructs and eliminates the complexity of a intermediate representation.

It's kind of Scheme-like under the hood, but I've worked hard to make it very C#/F#ish on the surface.