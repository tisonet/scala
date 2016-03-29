package tisonet.scala.steladb.sstable

import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets._
import java.nio.file.{Files, Paths}

import org.scalatest.{BeforeAndAfterEach, FunSuite}

class SSTableFileSuite extends FunSuite with BeforeAndAfterEach {

    val TEST_FILE_PATH = Paths.get("test_sstable").toAbsolutePath.toString
    var filePath: String = _

    override def beforeEach() {
        filePath = TEST_FILE_PATH + System.currentTimeMillis
    }

    override def afterEach() {
       // deleteFile(filePath)
    }

    test("Get size should return number of bytes of in UTF-8 encoding") {
        assert(SSTableFile.getSize("Hello world 1") == 13)
    }

    test("Write line should write data with new line delimiter") {
        SSTableFile(filePath).writeLine("Hello world")
        assert(readFile(filePath) == "Hello world\n")
    }

    test("Write should write data into file") {
        SSTableFile(filePath).write("Hello world")
        assert(readFile(filePath) == "Hello world")
    }

    test("Read should read file data of a given size in UTF-8 encoding") {
        writeToFile(filePath, "Hi Johny Walker")
        assert(SSTableFile(filePath).read(8)._1 == "Hi Johny")
    }

    test("Seek should skip to given position and write should rewrite data") {
        writeToFile(filePath, "Hello")
        new SSTableFile(filePath, 1).write("a")

        assert(readFile(filePath) == "Hallo")
    }

    test("Seek should skip to given position and read should start from a given position") {
        writeToFile(filePath, "Hello")
        assert(new SSTableFile(filePath, 2).read(3)._1 == "llo")
    }

    def readFile(filePath: String): String =
        new String(Files.readAllBytes(Paths.get(filePath)), UTF_8)

    def deleteFile(filePath: String) =
        Files.deleteIfExists(Paths.get(filePath))

    def writeToFile(filePath: String, data: String) = {
        val out = new BufferedWriter(new OutputStreamWriter(
            new FileOutputStream(filePath), "UTF-8"))
        try {
            out.write(data)
        } finally {
            out.close()
        }
    }
}
