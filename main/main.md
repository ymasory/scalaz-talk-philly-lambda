!SLIDE
# Thinking Monadically in Scala (:)
# Programming with Scalaz

- Yuvi Masory, Combinatory Solutions Inc
- @ymasory
- http://yuvimasory.com/talks
- May 29, 2012
- Philly Lambda
- A few bits from Mike Pilquist's Scalaz [talk](https://github.com/mpilquist/scalaz-talk)

![pic](main/scary.png "scalaz code")

!SLIDE
# Scalaz 7
- More modular
- _Much_ more discoverable
- Unicode is (mostly) gone
- Still poorly documented

!SLIDE
# I. You don't need to use functional programming to benefit from Scalaz!

!SLIDE
# 1. Scalaz has lots of random utilities (although could use some more ...)

!SLIDE
# Random utilities
```
"1".toInt
"foo".toInt //uh-oh

val iOpt: Option[Int] = "foo".parseInt
iOpt err "not an int!"
println(iOpt.isDefined ? "parsed" | "unparseable")
```

!SLIDE
# 2. Scalaz has _better_ alternatives to Java legacies.

!SLIDE
# `==` Considered Harmful

```
val admin: Option[User] = //...
val curUser: User = //...
if (curUser == admin) { //oops! always false
  //...
}
```

!SLIDE
# `===` Considered Awesome

```
implicit def userEqual = equalA[User]
if (curUser === admin) { //doesn't compile!
  //...
}
```


!SLIDE
# II. The Typeclass Pattern
You may have noticed this strange bit

```
implicit def userEqual = equalA[User]
```

!SLIDE
# What are types?
A simple definition: types are collections of _expressions_.

## Polymophism
Because we want our functions to work on many types.

## Ad-hoc polymorphism
Polymorphic functions may do something different depending on its input type.

!SLIDE
# Ad-hoc polymorphism using inheritance & subtyping

```
trait Equal[A] {
  def ===(a: A): Boolean
}
case class Person(name: String, zip: Int) extends Equal[Person] {
  def ===(that: Person) = //...
}
Person("yuvi", 19104) === Person("colleen", 12345)
```

!SLIDE
# Not bad. But what if ...
- ... you don't control the `Person` source code?
- ... you want more than one equality notion?
- ... you want your domain objects "light" and free of behavior?
- ... you want method implementations to be fully known at compile time?
- ... you want to leave the interface "open" so you can add new implementations later without re-jiggering a type hierarchy

!SLIDE
# Ad-hoc polymorphism using typeclasses
```
trait Equal[A] {
  // A => A => Boolean
  def equals(a1: A, a2: A): Boolean
}
def personEqual: Equal[Person] = new Person {
  def equals(p1: Person, p2: Person): Boolean = //...
}
personEqual equals (Person("yuvi", 19104), Person("colleen", 12345))
```

!SLIDE
# Now with some sugar
```
trait Equal[A] {
  def(lhs: A, rhs: A): Boolean
}
object Equal {
  implicit def addEqualOps[A:Equal](lhs: A) = new EqualOps(lhs)
}
class EqualOps[A](lhs: A)(implicit ev: Equal[A]) {
  def ===(rhs: A) ev.equals(lhs, rhs)
}
implicit object PersonEqual extends Equal[Person] {
  def equals(p1: Person, p2: Person): Boolean = //...
}

import Equal._
p1 === p2
```

!SLIDE
# Note this is a brand new type relationship
```
//"is-a"
def mycompare[A <: Comparable](lhs: A, rhs: A) = lhs compare rhs

//"has-a"
def myequal[A:Equal](lhs: A, rhs: A) = lhs equals rhs

//"has-a", de-sugared, note "ev" is never used
def myequal[A](lhs: A, rhs: A)(ev: Equal[A]) = lhs equals rhs
```

!SLIDE
# Advantages of typeclasses
- No dynamic dispatch, implementations known to compiler.
- Behavior is de-coupled from domain object, AND
- Behavior is de-coupled from inheritance, leaving the trait "open"
- Multiple typeclass instances can exist side-by-side

# Disadvantages of typeclasses
- Boilerplate (for now)
- Run-time overhead of wrapper object creation (for now)

!SLIDE
# More on typeclasses
- Seth Tisue's NE Scala Talk (video): http://bit.ly/He7rgq
- Erik Osheim's PHASE talk (slides): http://bit.ly/pbsukl

!SLIDE
# III. Let's get to some real functional programming ...
![pic](main/dontpanic.jpg "don't panic")

# You already do all these things ...

!SLIDE
# Monoids
Things you can add.

!SLIDE
# Technically, Monoids
```
// A => A => A
// Int => Int => Int
trait Semigroup[A] {
  def plus[A](s1: A, s2: A): A
}

// A
trait Monoid[A] extends Semigroup[A] {
  def zero[A]: A
}
```

!SLIDE
- Name some monoids!
- Semigroup that isn't a monoid?!

!SLIDE
# Functors
Let's keep it simple: if you can map over it, it's a functor.


```
// map: F[A] => (A => B) => F[B]
// e.g., Option[Int] => (Int => String) => Option[String]

trait Functor[F[_]] {
  def map[A, B](r: F[A], f: A => B): F[B]
}
```

!SLIDE
# `Option` has a `Functor`
```
object OptionIsFunctor: Functor[Option] = new Functor[Option] {
  def map[A, B](r: Option[A], f: A => B) = r match {
    case None => None
    case Some(a) => Some(f(a))
  }
}
```

!SLIDE
# `List` has a `Functor`
```
object ListIsFunctor: Functor[List] = new Functor[List] {
  def map[A, B](as: List[A], f: A => B) = as match {
    case Nil => Nil
    case h :: t => f(h) :: map(t, f)
  }
}
```

!SLIDE
# Aside: This is what Scala already does, although oddly without types or a representation of `Functor`

```
for (el <- List(1, 2)) yield el + 10

List(1, 2) map { _ + 10 }
```

!SLIDE
# Applicative typeclass
```
trait Applicative[F[_]] extends Functor[F] {
  def pure(a: A): A
  def apply...
}
```

!SLIDE
# Applicatives (really, applicative functors)
- No time for this ... (read: I don't really get it)
- Um, something about parallel computation? Or reversing the direction of function application? ... Or something?
- Tony Morris will join us at the end to cover applicatives.

!SLIDE
# Trivial example
```
// why aren't we making use of strict order?
for {
  url   <- urlOpt
  pw    <- passwordOpt
  uname <- usernameOpt
} yield DriverManager getConnection (url, pw, uname)

// now we're not
(url |@| pw |@| uname) { Driver.getConnection }
```

!SLIDE
# Monads
Monads are like elephants? Pirates?

!SLIDE
You can't handle an `A`, so I'll give you an `M[A]` for some monad `M`.

![pic](main/monads.jpg "monads")

!SLIDE
# In a world without monads ...
```
def perfectRoot(i: Int): Option[Int] = //...

//shouldn't this be of type Int => Option[Int]?
def doublePerfectRoot = perfectRoot compose perfectRoot

//how do we go about this?
val opt = perfectRoot(10000) //Some(100)
opt map { perfectRoot(_) }   //Some(Some(10)), fail :(
```

!SLIDE
# `Option` has a `Monad`
```
def perfectRoot(i: Int): Option[Int] = //...

val opt = perfectRoot(10000) 
opt map { perfectRoot(_) }     //Some(100)
opt flatMap { perfectRoot(_) } //Some(10), yay!
```

Monads = applicative functors that can `flatMap`

!SLIDE
# Monad
```
trait Monad[M[_]] extends Applicative[M] {

  // A => M[A]
  // e.g., Int => List[Int]
  def pure[A](a: => A): M[A]

  // M[A] => (A => M[B]) => M[B]
  // e.g., List[Int] => (Int => List[Int]) => List[Int]
  def flatMap[A, B](a: M[A], f: A => M[B]): M[B]
}
```

!SLIDE
# Monads you know
- `List[B]` - what's wrong with `A -> B`?
- `Option[B]` - what's wrong with `A -> B`?
- `Function1[B]` - what's wrong with `A -> B`?

!SLIDE
# Aside: Scala already has special support for monads, although oddly without types or a representation of `Monad`

```
for {
  a <- aOpt
  b <- bOpt
  c <- cOpt
} yield a + b + c

aOpt.flatMap { a =>
  bOpt.flatMap { b =>
    cOpt.map { c =>
      a + b + c
    }
  }
}
```

!SLIDE
# A monad you might not know
- `IO[A]` - what's wrong with `() -> A`?

!SLIDE
- Aside: What is functional programming, anyway?
- `def now: Long`
- `def pnow: IO[Long]`

```
val n1 = now
val n2 = now
n2 - n1
```

!SLIDE
# `IO` in action
```
scala> println("hello, world")
hello, world

scala> putStrLn("hello, world")
res1: scalaz.effects.IO[Unit] = scalaz.effects.IO$$anon$2@60419710
```

!SLIDE
# The `IO` typeclass

```
trait IO[A] {
  // A
  // "don't call until the end of the universe"
  def unsafePerformIO: A
}
```

!SLIDE
```
object CatsWithHatsApp {
  def addHat(p: Picture): Picture = ...

  def pmain(args: Vector[String]): IO[Unit] = {
      val Vector(path) = args
      for {
        files <- listDir(path) //List[File] <- IO[List[File]]
        file  <- files         //File <- List[File]
        pic <- readPic(file)   //Picture <- IO[Picture]
        _     <- writeFile(    //() <- IO[Unit]
          file,
          addHat(pic)
        )
      } yield ()
  }

  def main(args: Array[String]) = {
    val program = pmain.(Vector.empty ++ args).except {
      case e => putStrLn("failing with: " e.toString)
    }
    program.unsafePerformIO //end of the universe!
  }
}
```

!SLIDE
# Other typeclasses
`ApplicativePlus`, `MonadPlus`, `Comonad`, `Category`, `Arrow`, `ArrowPlus`, `Foldable`, `Traversable`, `Monad Transformers`, `Reader`, `Writer`, `State`, `Identity`, and more

These are functional "design patterns."

!SLIDE
# Thank you ... questions?
