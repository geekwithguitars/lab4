(*
                              CS51 Lab 4
                          Modules & Functors

Objective:

This lab practices concepts of modules, including files as modules,
signatures, polymorphic abstract types, and functors.

There are 5 total parts to this lab. Please refer to the following
files to complete all exercises:

-> lab4_part1.ml -- Part 1: Implementing modules (this file)
   lab4_part2.ml -- Part 2: Files as modules
   lab4_part3.ml -- Part 3: Interfaces as abstraction barriers
   lab4_part4.ml -- Part 4: Polymorphic abstract types
   lab4_part5.ml -- Part 5: Functors

 *)

(*======================================================================
Part 1: Implementing Modules

Modules are a way to encapsulate values and functions into separate
components. We may use a module to group related functions together,
for example.

It's also frequently useful to apply signatures to modules. The
signature guarantees that the module implements at least the values
and functions defined within it. The module may also implement more
as well, for internal use, but only those specified in the signature
will be available outside the module definition.

Below is a MATH signature; we'll use it to describe a limited subset of
functions and values that a math module might contain.
......................................................................*)

module type MATH =
  sig
    val pi : float
    val cos : float -> float
    val sin : float -> float
    val sum : float -> float -> float
    val max : float list -> float option
  end ;;

(*......................................................................
Exercise 1A: Complete the implementation of a module called Math that
satisfies the signature above. (The value `nan` stands for "not a
number" and is an actual value of the float type. We're using it here
as a temporary value pending your putting in appropriate ones.)
......................................................................*)

module Math : MATH =
  struct
    let pi = 3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196442881097566593344612847564823378678316527120190914564856692346034861045432664821339360726024914127372458700660631558817488152092096282925409171536436789259036001133053054882046652138414695194151160943305727036575959195309218611738193261179310511854807446237996274956735188575272489122793818301194912983367336244065664308602139494639522473719070217986094370277053921717629317675238467481846766940513200056812714526356082778577134275778960917363717872146844090122495343014654958537105079227968925892354201995611212902196086403441815981362977477130996051870721134999999837297804995105973173281609631859502445945534690830264252230825334468503526193118817101000313783875288658753320838142061717766914730359825349042875546873115956286388235378759375195778185778053217122680661300192787661119590921642019
    let cos = cos
    let sin = sin
    let sum = ( +. )
    let max (lst : float list) =
      match lst with
      | [] -> None
      | hd :: tl -> Some (List.fold_right max tl hd)
  end ;;

(*......................................................................
Exercise 1B: Now that you've implemented the Math module, use it to
compute the maximum of the cosine of pi and the sine of pi, a value of
type float option. Name the resulting value `result`. (Do not use
the `open` command for this exercise.)
......................................................................*)

let result = Math.max([Math.cos(Math.pi); Math.sin(Math.pi)]) ;;

(*......................................................................
Exercise 1C: Redo the computation from above, but use the `local open`
syntax to write your computation in a more succinct manner.
......................................................................*)

let open Math in max([cos(pi); sin(pi)]) ;;
