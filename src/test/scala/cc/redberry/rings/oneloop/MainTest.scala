package cc.redberry.rings.oneloop

import org.junit.Test

/**
  *
  */
class MainTest {
  @Test
  def test1(): Unit = {
    println(Main.mainToString(Array("i2", "--n1", "2", "--n2", "2", "--indices", "mn")))
  }
}
