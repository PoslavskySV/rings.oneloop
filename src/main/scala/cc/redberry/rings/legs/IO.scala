package cc.redberry.rings.legs

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.nio.charset.Charset
import java.util.Base64

import cc.redberry.rings.scaladsl.Ring
import cc.redberry.rings.sym.Definitions.IntegralDef


/**
  *
  */
object IO {
  def writeRingElement[E](element: E, output: ObjectOutputStream)(implicit ring: Ring[E]): Unit = {
    val bytes = Base64.getDecoder.decode(ring.stringify(element))
    output.writeInt(bytes.length)
    output.write(bytes)
  }

  def readRingElement[E](input: ObjectInputStream)(implicit ring: Ring[E]): E = {
    val len = input.readInt()
    val bytes = Array.ofDim[Byte](len)
    input.readFully(bytes)
    ring.apply(Base64.getEncoder.encodeToString(bytes))
  }

  def writeSymFunc[E](element: IntegralDef[E], output: ObjectOutputStream)(implicit ring: Ring[E]): Unit = {
    output.writeObject(element.id)
    output.write(element.args.length)
    for (elem <- element.args)
      writeRingElement(elem, output)
  }

  def readSymFunc[E](output: ObjectInputStream)(implicit ring: Ring[E]): IntegralDef[E] = {
    val id: String = output.readObject().asInstanceOf[String]
    val len = output.readInt()
    IntegralDef(id, (0 until len).map(_ => readRingElement(output)))
  }
}
