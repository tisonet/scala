package tisonet.scala.steladb.memtable

trait Memtable {

    def add(key: String, data: String)

    def get(key: String): Option[String]

    def isFull: Boolean
}

