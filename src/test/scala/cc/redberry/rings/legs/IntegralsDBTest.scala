package cc.redberry.rings.legs

import java.io.File

import cc.redberry.rings.scaladsl.{MultivariateRing, Rational, _}
import cc.redberry.rings.sym.Definitions._
import org.junit.Test

/**
  *
  */
class IntegralsDBTest {
  @Test
  def test1: Unit = {
    val file = File.createTempFile("prefix", "suffix")
    file.deleteOnExit()

    val cfRing = Zp(13)
    implicit val ring = MultivariateRing(cfRing, Array("p12", "p23", "p34", "p45", "p15", "p13", "p14", "p24", "p25", "p35"))
    type Expr = ring.ElementType
    type Rat = Rational[Expr]

    val f1 = IntegralDef("F1", Seq(1, 1), Seq(ring(1), ring(2)))
    val f2 = IntegralDef("F2", Seq(2, 1), Seq(ring(3), ring(4)))
    val f3 = IntegralDef("F3", Seq(1, 2), Seq(ring(5), ring(6)))

    val if1 = IntegralSignature(f1)
    val if2 = IntegralSignature(f2)
    val if3 = IntegralSignature(f3)

    val e1 = CachedIntegralVal(f1, IntegralVal(f1) + IntegralVal(f2))
    val e2 = CachedIntegralVal(f2, IntegralVal(f2) + IntegralVal(f3))
    val e3 = CachedIntegralVal(f3, IntegralVal(f3) + IntegralVal(f1))

    val cache = new IntegralsDB(file)
    cache.putRawIntegral(if1, e1, ring)
    cache.putRawIntegral(if2, e2, ring)
    cache.putRawIntegral(if3, e3, ring)
    cache.close()

    val recache = new IntegralsDB(file)
    assert(recache.getRawIntegral(if1, ring) == e1)
    assert(recache.getRawIntegral(if2, ring) == e2)
    assert(recache.getRawIntegral(if3, ring) == e3)
    recache.close()
  }
}
