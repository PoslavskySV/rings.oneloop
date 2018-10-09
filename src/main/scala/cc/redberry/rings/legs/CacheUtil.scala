package cc.redberry.rings.legs

import cc.redberry.rings.scaladsl.Ring
import cc.redberry.rings.sym.Definitions.{IntegralDef, IntegralValue}
import org.mapdb.{DataInput2, DataOutput2, Serializer}

/** Integral signature (indices + dimension) */
case class IntegralSignature[E](indices: Array[Int], dim: E)

/** Integral signature seiralization */
final class SignatureSerializer[E](ring: Ring[E])
  extends Serializer[IntegralSignature[E]] {
  override def serialize(out: DataOutput2, value: IntegralSignature[E]): Unit = {
    out.writeInt(value.indices.length)
    value.indices.foreach(out.writeInt)
    out.writeUTF(ring.stringify(value.dim))
  }

  override def deserialize(in: DataInput2, available: Int): IntegralSignature[E] = {
    val len = in.readInt()
    val indices = (0 until len).map(_ => in.readInt()).toArray
    val dim = ring(in.readUTF())
    IntegralSignature(indices, dim)
  }
}

/** Cached value of integral */
case class CachedIntegralValue[E](integral: IntegralDef[E],
                                  value: IntegralValue[E])

/** Serializer for SymFunc */
final class SymFuncSerializer[E](ring: Ring[E])
  extends Serializer[IntegralDef[E]] {

  override def serialize(out: DataOutput2, value: IntegralDef[E]): Unit = {
    out.writeUTF(value.id)
    out.writeInt(value.args.length)
    value.args.foreach { a => out.writeUTF(ring.stringify(a)) }
  }

  override def deserialize(in: DataInput2, available: Int): IntegralDef[E] = {
    val id = in.readUTF()
    val len = in.readInt()
    val args: Seq[E] = (0 until len).map(_ => ring(in.readUTF()))
    IntegralDef(id, args)
  }
}

/** Serializer for SymFuncSum */
final class SymFuncSumSerializer[E](ring: Ring[E])
  extends Serializer[IntegralValue[E]] {

  private val symFuncSerializer = new SymFuncSerializer[E](ring)

  override def serialize(out: DataOutput2, value: IntegralValue[E]): Unit = {
    out.writeInt(value.size)
    value.terms.foreach { case (k, v) =>
      symFuncSerializer.serialize(out, k)
      out.writeUTF(ring.stringify(v))
    }
  }

  override def deserialize(in: DataInput2, available: Int): IntegralValue[E] = {
    val len = in.readInt()
    IntegralValue((0 until len).map(_ => {
      val k = symFuncSerializer.deserialize(in, available)
      val v = ring(in.readUTF())
      (k, v)
    }).toMap)
  }
}