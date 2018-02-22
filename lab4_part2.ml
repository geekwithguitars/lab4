(*
                              CS51 Lab 4
                          Modules & Functors

Objective:

This lab practices concepts of modules, including files as modules,
signatures, polymorphic abstract types, and functors.

There are 5 total parts to this lab. Please refer to the following
files to complete all exercises:

   lab4_part1.ml -- Part 1: Implementing modules
-> lab4_part2.ml -- Part 2: Files as modules (this file)
   lab4_part3.ml -- Part 3: Interfaces as abstraction barriers
   lab4_part4.ml -- Part 4: Polymorphic abstract types
   lab4_part5.ml -- Part 5: Functors
 *)

(*======================================================================
Part 2: Files as modules

A useful feature of OCaml is that it *automatically* wraps functions
and values defined in a single file into a module named after that
file during compilation. The module name is the name of the file with
the first letter capitalized. This functionality is in addition to the
manual definition of modules as you've just used in Part 1, but it is
a convenient way of separating logic into separate namespaces when
writing a large program.

There are several other source files included in this lab, other than
the lab4 ML files. Take a look at fibonacci.ml, inverse.ml, and
studyplan.ml to see what functions and values they contain. You will
*not* need to modify these files to complete any of the exercises
below, and you shouldn't do so.

NB **** The contents of these files aren't intended to be particularly
        useful. They're just a vehicle to explore ideas of
        commonalities among modules and their relation to module
        signatures.

The `ocamlbuild` command should automatically find modules that you've
written in the same directory as your source, compile those additional
files, and link them to your compiled program. You can then access
functions from those files under the module name, which (again) is the
name of the file with the first letter capitalized.

If you're testing with a top-level REPL, like utop, it will not
automatically find those modules and evaluate them for you. However,
you can do so manually yourself with the mod_use directive, like this:

    # #mod_use "fibonacci.ml" ;;
    module Fibonacci :
      sig
        type length = Infinite | Finite of int
        type info = { name : string; length : length; inventor : string; }
        val name : string
        val length : length
        val inventor : string
        val info : info
        val eval : int -> int option
        val exists : int -> bool
      end

allowing you to then refer to elements of the module as, for instance,

    # Fibonacci.length ;;
    - : Fibonacci.length = Fibonacci.Infinite

(Note the capitalized module name.)

......................................................................
Exercise 2A: Find the 7th term of the fibonacci sequence by using the
`eval` function defined in the "fibonacci.ml" file, and store it in
the value `best_fib`.
......................................................................*)

#mod_use "fibonacci.ml" ;;
let best_fib = Fibonacci.eval 7 ;;

(*......................................................................
Exercise 2B: Find the last element of the `Studyplan` sequence, and
store it in the value `last_class`. You'll want to read about what the
various values in the Studyplan module do to understand how to use
them.
......................................................................*)

#mod_use "studyplan.ml" ;;
let last_class = None ;;

(*......................................................................
We hope you'll find the module system easy to use and convenient, once
you get the hang of the conventions.

Let's investigate one way that a signature could be useful. Although
there are some differences between module Fibonacci, module Inverse,
and module Studyplan, there are also some similarities. Several
functions and values have the same *name* and *type*, even if they are
implemented differently. We can leverage this, and write a signature
that can be applied to both modules and will be useful to determine
which functions we can call in an identical fashion between both
modules.

......................................................................
Exercise 2C: Define a signature called Sequence that maximally exposes
as much of all three modules as possible, that is, those values and
functions that have the same name and type.
......................................................................*)


module type Sequence =
module Studyplan :
  sig
    type length = Infinite | Finite of int
    type info = { name : string; length : length; inventor : string; }
    val length : length
    val name : string
    val courses : int array
    val info : info
    val inventor : string
    val exists : int -> bool
    val eval : int -> int option
  end
  module Fibonacci :
  sig
    type length = Infinite | Finite of int
    type info = { name : string; length : length; inventor : string; }
    val name : string
    val length : length
    val inventor : string
    val info : info
    val exists : int -> bool
    val eval : int -> int option
  end
  module Inverse :
  sig
    type length = Infinite | Finite of int
    type info = { name : string; inventor : string option; length : length; }
    val name : string
    val length : length
    val inventor : 'a option
    val info : info
    val eval : int -> float option
    val exists : int -> bool
  end ;;



(*......................................................................
Exercise 2D: Now, create new modules, named SequenceFibonacci,
SequenceInverse, and SequenceStudyplan that correspond to the
Fibonacci, Inverse, and Studyplan modules, respectively, but
restricted to the Sequence signature.

Hint: Creation of each module should only take one line of
code. You'll just want to replace the trivial module definition
"struct end" in the code below.
......................................................................*)

module SequenceFibonacci = struct end ;;
module SequenceInverse = struct end ;;
module SequenceStudyplan = struct end ;;
