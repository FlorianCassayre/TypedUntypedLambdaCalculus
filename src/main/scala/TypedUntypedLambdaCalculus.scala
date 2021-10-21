import scala.compiletime.ops.any.*
import scala.compiletime.ops.boolean.*
import scala.compiletime.ops.int.*
import scala.util.NotGiven

object TypedUntypedLambdaCalculus:

  object UntypedLambdaCalculus:

    // Atoms

    sealed transparent trait Term
    sealed transparent trait Var[N <: Int] extends Term
    sealed transparent trait Abs[T1 <: Term] extends Term
    sealed transparent trait App[T1 <: Term, T2 <: Term] extends Term

    // Evaluator

    protected type Leveled[T <: Term, X <: Int, Y <: Int] <: Term = T match
      case Var[n] => n < X match
        case true => T
        case false => Var[n + Y]
      case Abs[t1] => Abs[Leveled[t1, X + 1, Y]]
      case App[t1, t2] => App[Leveled[t1, X, Y], Leveled[t2, X, Y]]

    protected type Subst[T <: Term, X <: Int, R <: Term] <: Term = T match
      case Var[n] => n == X match
        case true => Leveled[R, 0, X]
        case false => n < X match
          case true => T
          case false => Var[n - 1]
      case Abs[t1] => Abs[Subst[t1, X + 1, R]]
      case App[t1, t2] => App[Subst[t1, X, R], Subst[t2, X, R]]

    protected type ReduceStep[T <: Term] <: Term | Unit = T match
      case App[Abs[t1], t2] => Subst[t1, 0, t2]
      case App[t1, t2] => ReduceStep[t1] match
        case Unit => ReduceStep[t2] match
          case Unit => Unit
          case _ => App[t1, ReduceStep[t2]]
        case _ => App[ReduceStep[t1], t2]
      case Abs[t1] => ReduceStep[t1] match
        case Unit => Unit
        case _ => Abs[ReduceStep[t1]]
      case Var[n] => Unit

    type Reduce[T <: Term] <: Term = ReduceStep[T] match
      case Unit => T
      case _ => Reduce[ReduceStep[T]]


  // Shorthands

  import UntypedLambdaCalculus._

  type =!=[L, R] = NotGiven[L =:= R]

  type App2[L <: Term, A <: Term, B <: Term] = App[App[L, A], B]

  // Operations

  type True = Abs[Abs[Var[1]]]
  type False = Abs[Abs[Var[0]]]
  type Not = Abs[Abs[Abs[App2[Var[2], Var[0], Var[1]]]]]
  type And = Abs[Abs[App2[Var[1], Var[0], Var[1]]]]
  type Or = Abs[Abs[App2[Var[1], Var[1], Var[0]]]]

  type Pair = Abs[Abs[Abs[App2[Var[0], Var[2], Var[1]]]]]
  type First = Abs[App[Var[0], True]]
  type Second = Abs[App[Var[0], False]]

  type ChurchHelper[N <: Int] <: Term = N == 0 match
    case true => Var[0]
    case false => App[Var[1], ChurchHelper[N - 1]]
  type Church[N <: Int] = Abs[Abs[ChurchHelper[N]]]

  type Succ = Abs[Abs[Abs[App[Var[1], App[App[Var[2], Var[1]], Var[0]]]]]]
  type Plus = Abs[Abs[Abs[Abs[App[App[Var[3], Var[1]], App[App[Var[2], Var[1]], Var[0]]]]]]]
  type Times = Abs[Abs[App[App[Var[1], App[Plus, Var[0]]], Church[0]]]]

  type IsZero = Abs[App[App[Var[0], Abs[False]], True]]

  type Pred = Abs[App[First, App2[Var[0], Abs[App2[Pair, App[Second, Var[0]], App2[Plus, Church[1], App[Second, Var[0]]]]], App2[Pair, Church[0], Church[0]]]]]

  // Tests

  summon[False =!= True]

  summon[Reduce[App[Not, True]] =:= False]
  summon[Reduce[App[Not, False]] =:= True]

  summon[Reduce[App2[And, True, True]] =:= True]
  summon[Reduce[App2[And, False, True]] =:= False]
  summon[Reduce[App2[And, True, False]] =:= False]
  summon[Reduce[App2[And, False, False]] =:= False]

  summon[Reduce[App2[Or, True, True]] =:= True]
  summon[Reduce[App2[Or, False, True]] =:= True]
  summon[Reduce[App2[Or, True, False]] =:= True]
  summon[Reduce[App2[Or, False, False]] =:= False]

  summon[Reduce[App[First, App2[Pair, True, False]]] =:= True]
  summon[Reduce[App[Second, App2[Pair, True, False]]] =:= False]

  summon[Church[0] =!= Church[1]]
  summon[Church[1] =!= Church[2]]
  summon[Church[0] =!= Church[2]]

  summon[Church[0] =:= Abs[Abs[Var[0]]]]
  summon[Church[1] =:= Abs[Abs[App[Var[1], Var[0]]]]]
  summon[Church[2] =:= Abs[Abs[App[Var[1], App[Var[1], Var[0]]]]]]
  summon[Church[3] =:= Abs[Abs[App[Var[1], App[Var[1], App[Var[1], Var[0]]]]]]]

  summon[Reduce[App[Succ, Church[0]]] =:= Church[1]]
  summon[Reduce[App[Succ, App[Succ, Church[4]]]] =:= Church[6]]

  summon[Reduce[App2[Plus, Church[0], Church[0]]] =:= Church[0]]
  summon[Reduce[App2[Plus, Church[1], Church[1]]] =:= Church[2]]
  summon[Reduce[App2[Plus, Church[3], Church[2]]] =:= Church[5]]

  summon[Reduce[App2[Times, Church[0], Church[0]]] =:= Church[0]]
  summon[Reduce[App2[Times, Church[1], Church[0]]] =:= Church[0]]
  summon[Reduce[App2[Times, Church[1], Church[1]]] =:= Church[1]]
  summon[Reduce[App2[Times, Church[2], Church[3]]] =:= Church[6]]

  summon[Reduce[App[IsZero, Church[0]]] =:= True]
  summon[Reduce[App[IsZero, Church[1]]] =:= False]
  summon[Reduce[App[IsZero, App2[Times, Church[0], Church[4]]]] =:= True]

  summon[Reduce[App[Pred, Church[1]]] =:= Church[0]]
  //summon[Reduce[App[Pred1, Church[2]]] =:= Church[1]] // Uncomment this if you dare
