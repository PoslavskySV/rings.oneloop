package cc.redberry.rings.oneloop

import java.nio.file.Files

/**
  *
  */
class TestUtil {

}

object TestUtil {
  def mkTempFile() = Files.createTempFile("xxxxx", "yyyyy").toString
}
