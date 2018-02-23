(*
                              CS51 Lab 4
                          Modules & Functors

Objective:

This lab practices concepts of modules, including files as modules,
signatures, polymorphic abstract types, and functors.

There are 5 total parts to this lab. Please refer to the following
files to complete all exercises:

   lab4_part1.ml -- Part 1: Implementing modules
   lab4_part2.ml -- Part 2: Files as modules
   lab4_part3.ml -- Part 3: Interfaces as abstraction barriers
   lab4_part4.ml -- Part 4: Polymorphic abstract types
-> lab4_part5.ml -- Part 5: Functors (this file)

 *)

(*======================================================================
Part 5: Functors

Although we made the Queue module's signature abstract, the Stack
module is not (yet) quite as abstract. In this part, you'll rewrite
the Stack module so that its element type is polymorphic, while adding
some functionality at the same time.

Occasionally, it's useful to be able to take a data structure and
"serialize" it (also called "marshalling"), that is, to transform its
internal computational representation into a format, like a string,
that can be saved or transmitted. It can later be deserialized to
recreate the internal representation. You may notice that this
provides an opportunity to break an abstraction barrier: serializing
an object, manipulating its representation, and then deserializing
back to an object could allow for some invariant to be violated. Even
so, serialization has its uses when the circumstances are
warranted. As a result, we'll add to the stack module extremely basic
support for serialization.

In order to do this with an abstract type for an element in a stack,
we also need to have a function `serialize` that can accept an element and
return a string.

We can use a functor to generate a stack that bundles everything
together.

In order to do this, we'll first define a module interface, called
SERIALIZE, that requires that a module expose both a data type and a
function that converts that type into a string representation of its
value. As you'll see, the SERIALIZE module type is an appropriate
signature for the argument of a functor that generates stack modules.
......................................................................*)

module type SERIALIZE =
  sig
    type t
    val serialize : t -> string
  end ;;

(*......................................................................
Now we'll define a STACK interface. Notice that unlike the INT_STACK,
we'll hide the element type as an abstract data type, and add
additional functions for serialization, a couple of higher- order
functions over stacks.
......................................................................*)

module type STACK =
  sig
    exception Empty
    type element
    type stack
    val empty : unit -> stack
    val push : element -> stack -> stack
    val top : stack -> element
    val pop : stack -> stack
    val serialize : stack -> string
    val map : (element -> element) -> stack -> stack
    val filter : (element -> bool) -> stack -> stack
    val fold_left : ('a -> element -> 'a) -> 'a -> stack -> 'a
  end ;;

(*......................................................................
Exercise 5A: Now, re-write your stack implementation to use abstract
data types, and also complete the other functions that the signature
requires. Don't forget that you can implement other functions that are
not specified by the signature if you would like. The module signature
will prevent them from leaking out of the module.

The serialize function should construct a string in the following
form:

    "N0:N1:N2:...:N"

where N0 was the *first* element pushed onto the stack, N1 was the
*second*, and so on, with N being the *most recent* element pushed to
the stack. (This would make deserialization easier, since elements
could be pushed as soon as they were read.)
......................................................................*)

module MakeStack (Element: SERIALIZE) : (STACK with type element = Element.t) =
  struct
    exception Empty

    type element = Element.t

    type stack = element list

    let empty () : stack = []

    let push (el: element) (s: stack) : stack = el :: s

    let pop_helper (s: stack) : (element * stack) =
      match s with
      | [] -> raise Empty
      | h :: t -> (h, t)

    let top (s: stack) : element = fst (pop_helper s)

    let pop (s: stack) : stack = snd (pop_helper s)

    let map (f: element -> element) (s: stack) : stack = List.map f s

    let filter (f: element -> bool) (s: stack) : stack = List.filter f s

    let fold_left (f: 'a -> element -> 'a) (init: 'a) (s: stack) : 'a = 
      List.fold_left f init s

    let serialize (s: stack) : string =
      let f x y = Element.serialize y ^ (if x <> "" then ":" ^ x else "") in
      fold_left f "" s
  end ;;

(*......................................................................
Exercise 5B: Now, make a module "IntStack" by applying the functor
that you just defined to an appropriate module for serializing ints.
......................................................................*)

module IntStack = MakeStack(serialize) ;;

(*......................................................................
Exercise 5C: Make a module "IntStringStack" that creates a stack whose
elements are int * string pairs. Its serialize function should output
values in the form:

    "(N,'S')"

where N is the int, and S is the string, for instance, a stack with
two elements might be serialized as

    "(1, 'pushed first'):(2, 'pushed second')"     .

For this oversimplified serialization function, you may assume that
the string will be made up of alphanumeric characters only.
......................................................................*)

module IntStringStack =   
struct
  type t = (int * string)
  let serialize x = "(" ^ string_of_int (fst x) ^ ",'" ^ (snd x) ^ "')"
end ;;;;
