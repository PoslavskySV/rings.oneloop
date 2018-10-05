package cc.redberry.rings.sym

import cc.redberry.rings.Rational
import cc.redberry.rings.primes.SmallPrimes
import cc.redberry.rings.scaladsl._

/**
  * Created by Stanislav on 04.08.2018.
  */
object commons {
  type Expr = Rational[MultivariatePolynomial[IntZ]]

  def u[E](in: E): E = in

  def U1[E](in: E): E = in

  def U2[E](in: E): E = in

  def U3[E](in: E): E = in

  val cfRing = Z//scaladsl.Zp(SmallPrimes.nextPrime(1<<20))
}
