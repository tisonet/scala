package tisonet.scala.steladb.memtable

trait Memtable {

    def add(key: String, data: String)

    def get(key: String): Option[String]

    def getAll: List[(String, String)]

    def isFull: Boolean
}

