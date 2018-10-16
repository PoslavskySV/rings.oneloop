package cc.redberry.rings.oneloop

import cc.redberry.rings.oneloop.Definitions._
import cc.redberry.rings.scaladsl.Ring
import org.mapdb.{DataInput2, DataOutput2, Serializer}


/** Integral signature (indices + dimension) */
// todo: same data structure as IntegralDef: generify?
case class IntegralSignature[E](id: String, indices: Seq[Int], args: Seq[E])

object IntegralSignature {
  def apply[E](iDef: IntegralDef[E]): IntegralSignature[E] = IntegralSignature(iDef.id, iDef.indices, iDef.args)
}

/** Integral signature serialization */
final class IntegralSignatureSerializer[E](ring: Ring[E])
  extends Serializer[IntegralSignature[E]] {
  override def serialize(out: DataOutput2, value: IntegralSignature[E]): Unit = {
    out.writeUTF(value.id)
    out.writeInt(value.indices.length)
    value.indices.foreach(out.writeInt)
    out.writeInt(value.args.length)
    value.args.foreach(a => out.writeUTF(ring.stringify(a)))
  }

  override def deserialize(in: DataInput2, available: Int): IntegralSignature[E] = {
    val id = in.readUTF()
    val iLen = in.readInt()
    val indices = (0 until iLen).map(_ => in.readInt())
    val aLen = in.readInt()
    val args = (0 until aLen).map(_ => ring(in.readUTF()))
    IntegralSignature(id, indices, args)
  }
}

/** Cached value of integral */
case class CachedIntegralVal[E](iDef: IntegralDef[E],
                                iVal: IntegralVal[E])

/** Integral signature serialization */
final class CachedIntegralValSerializer[E](ring: Ring[E])
  extends Serializer[CachedIntegralVal[E]] {

  val valSerializer = new IntegralValSerializer[E](ring)
  val defSerializer = valSerializer.defSerializer

  override def serialize(out: DataOutput2, value: CachedIntegralVal[E]): Unit = {
    defSerializer.serialize(out, value.iDef)
    valSerializer.serialize(out, value.iVal)
  }

  override def deserialize(in: DataInput2, available: Int): CachedIntegralVal[E] = {
    val iDef = defSerializer.deserialize(in, available)
    val iVal = valSerializer.deserialize(in, available)
    CachedIntegralVal(iDef, iVal)
  }
}

/** Cached value of integral */
case class CachedFactorizedIntegralVal[E](iDef: IntegralDef[E],
                                          iFactorVals: FactorizedIntegralVal[E])

/** Integral signature serialization */
final class CachedFactorizedIntegralValSerializer[E](ring: Ring[E])
  extends Serializer[CachedFactorizedIntegralVal[E]] {

  val valSerializer = new FactorizedIntegralValSerializer[E](ring)
  val defSerializer = valSerializer.defSerializer

  override def serialize(out: DataOutput2, value: CachedFactorizedIntegralVal[E]): Unit = {
    defSerializer.serialize(out, value.iDef)
    valSerializer.serialize(out, value.iFactorVals)
  }

  override def deserialize(input: DataInput2, available: Int): CachedFactorizedIntegralVal[E] = {
    val iDef = defSerializer.deserialize(input, available)
    val iFactorVals = valSerializer.deserialize(input, available)
    CachedFactorizedIntegralVal(iDef, iFactorVals)
  }
}

/** Serializer for IntegralDef */
final class IntegralDefSerializer[E](ring: Ring[E])
  extends Serializer[IntegralDef[E]] {

  override def serialize(out: DataOutput2, value: IntegralDef[E]): Unit = {
    out.writeUTF(value.id)
    out.writeInt(value.indices.length)
    value.indices.foreach(out.writeInt)
    out.writeInt(value.args.length)
    value.args.foreach { a => out.writeUTF(ring.stringify(a)) }
  }

  override def deserialize(in: DataInput2, available: Int): IntegralDef[E] = {
    val id = in.readUTF()
    val iLen = in.readInt()
    val indices = (0 until iLen).map(_ => in.readInt())
    val aLen = in.readInt()
    val args = (0 until aLen).map(_ => ring(in.readUTF()))
    IntegralDef(id, indices, args)
  }
}

/** Serializer for IntegralValue */
final class IntegralValSerializer[E](ring: Ring[E])
  extends Serializer[IntegralVal[E]] {

  val defSerializer = new IntegralDefSerializer[E](ring)

  override def serialize(out: DataOutput2, value: IntegralVal[E]): Unit = {
    out.writeInt(value.terms.size)
    value.terms.foreach { case (k, v) =>
      defSerializer.serialize(out, k)
      out.writeUTF(ring.stringify(v))
    }
  }

  override def deserialize(in: DataInput2, available: Int): IntegralVal[E] = {
    val len = in.readInt()
    val terms = (0 until len).map(_ => {
      val k = defSerializer.deserialize(in, available)
      val v = ring(in.readUTF())
      (k, v)
    }).toMap
    IntegralVal(terms)
  }
}


/** Serializer for IntegralValue */
final class FactorizedIntegralValSerializer[E](ring: Ring[E])
  extends Serializer[FactorizedIntegralVal[E]] {

  val defSerializer = new IntegralDefSerializer[E](ring)

  override def serialize(out: DataOutput2, value: FactorizedIntegralVal[E]): Unit = {
    out.writeInt(value.terms.size)
    value.terms.foreach { case (k, v) =>
      defSerializer.serialize(out, k)
      out.writeInt(v.size)
      v.foreach { case (f, e) =>
        out.writeInt(e)
        out.writeUTF(ring.stringify(f))
      }
    }
  }

  override def deserialize(in: DataInput2, available: Int): FactorizedIntegralVal[E] = {
    val len = in.readInt()
    val terms = (0 until len).map(_ => {
      val k = defSerializer.deserialize(in, available)
      val iLen = in.readInt()
      val seq = (0 until iLen).map(_ => {
        val e = in.readInt()
        val f = ring(in.readUTF())
        (f, e)
      })
      (k, seq)
    }).toMap
    FactorizedIntegralVal(terms)
  }
}