package cc.redberry.rings.legs

import java.io.File

import cc.redberry.rings.scaladsl.{MultivariateRing, Rational, _}
import cc.redberry.rings.sym.Definitions._
import org.junit.Test

/**
  *
  */
class CacheTest {

  @Test
  def test1: Unit = {
    val file = File.createTempFile("prefix", "suffix")

    val cfRing = Zp(13)
    implicit val ring = MultivariateRing(cfRing, Array("p12", "p23", "p34", "p45", "p15", "p13", "p14", "p24", "p25", "p35"))
    type Expr = ring.ElementType
    type Rat = Rational[Expr]

    val f1 = IntegralValue("F1", ring(1), ring(2))
    val f2 = IntegralValue("F2", ring(1), ring(2))
    val f3 = f1 + f2

    val cache = new IntegralsDB(file)
    cache.put(f1, f3, ring)
    cache.put(f2, f1, ring)
    cache.put(f3, f2, ring)
    cache.close()

    val recache = new IntegralsDB(file)
    assert(recache.get(f1, ring) == f3)
    assert(recache.get(f2, ring) == f1)
    assert(recache.get(f3, ring) == f2)
    recache.close()
  }
}
