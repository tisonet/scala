package tisonet.scala.steladb

import tisonet.scala.steladb.OpType.OpType

trait CommitLog {
    def log(op: Op): Long
}

case class Op(opType: OpType, key: String, data: Option[String])

object OpType extends Enumeration {
    type OpType = Value

    val Write, Read, Remove = Value
}
