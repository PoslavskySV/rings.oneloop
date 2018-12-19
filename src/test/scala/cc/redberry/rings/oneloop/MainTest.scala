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

  @Test
  def test2(): Unit = {
    val (res, _) = Main.mainToString(Array("i3", "--n1", "1", "--n2", "1", "--n3", "1", "--di", "2", "--indices", "mn"))
    assert(!res.contains("alpha"))
  }
}
