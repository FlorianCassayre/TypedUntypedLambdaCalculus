The typed untyped lambda calculus
===

This project implement a proof of concept interpreter for the untyped lambda
calculus using nothing more than the Scala type system (thus the funny name).
That means the evaluation is performed by the compiler; no runtime is required.

## Implementation details

The program relies on recent features of Scala 3, namely:

* [Match types](https://docs.scala-lang.org/scala3/reference/new-types/match-types.html)
* Literal types
* [Compile-time operations](https://docs.scala-lang.org/scala3/reference/metaprogramming/compiletime-ops.html)

For the sake of simplicity we chose to use the De Bruijn index representation;
i.e. there are no names but rather indices which indicate to which abstraction a
variable is bound to. The terms are represented by the following concrete types:

* `Term`: the parent of all terms
  * `Var[N <: Int]`: a variable
  * `Abs[T <: Term]`: a lambda abstraction
  * `App[T1 <: Term, T2 <: Term]`: an application

For instance `\x. \y. x y` would be represented as `\. \. 1 0` in the De Bruijn
nameless representation, which would translate to the type
`Abs[Abs[App[Var[1], Var[0]]]]`.

All type-level functions are represented as parametrized type aliases. The
evaluator is defined as `type Reduce[T <: Term] <: Term`; that means one can
then do `Reduce[Abs[Abs[App[Var[1], Var[0]]]]]` to obtain a normal form.

Thanks to the De Bruijn indices, two equivalent normal forms will also be
structurally equal in that representation. Therefore, we can rely on the `=:=`
type operator to compare normal forms. The compile-time method `summon` is
helpful to prove the existence or the absence of a type evidence.

For example the following should compile:

```Scala
summon[Reduce[App[Abs[Var[0]], Abs[Var[0]]]] =:= Abs[Var[0]]]
```

We also defined the following shorthand which yields an evidence only when
the structural equality cannot be proven:

```Scala
import scala.util.NotGiven

type =!=[L, R] = NotGiven[L =:= R]
```

We can then proceed by building our theory:

```Scala
type True = Abs[Abs[Var[1]]]
type False = Abs[Abs[Var[0]]]
type And = Abs[Abs[App2[Var[1], Var[0], Var[1]]]]
// ...

summon[Reduce[App[App[And, False], True]] =:= False]
```

([view the complete file](src/main/scala/TypedUntypedLambdaCalculus.scala))

The untyped lambda calculus is well known to be Turing-complete. Therefore, in
theory, that would automatically grant the type system a Turing-completeness
property.

## Limitations

The time complexity of the evaluator is thought to be exponential. Due to the
way match types are resolved, intermediate results cannot be labelled and must
be recomputed upon each use. It is not known whether the compiler optimizes
these computations using memoization.

Additionally, the compiler has a depth limit which would make it theoretically
impossible to type check all terminating evaluations. In practice I have not
managed to reach it; the breadth seems to be the dominant factor.

_With great computational power comes great responsibility_: if you are using an
IDE, disable the linter as it is very likely to hang. The compiler can crash
if you evaluate a diverging term, or if a term is simply too large for your
computer to handle.
