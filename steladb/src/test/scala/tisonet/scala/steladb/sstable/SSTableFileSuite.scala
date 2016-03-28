package tisonet.scala.steladb.sstable

import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter, Writer}
import java.nio.charset.StandardCharsets._
import java.nio.file.{Files, Paths}

import org.scalatest.{BeforeAndAfterEach, FunSuite}

class SSTableFileSuite extends FunSuite with BeforeAndAfterEach {

    val TEST_FILE_PATH = Paths.get("test_sstable").toAbsolutePath.toString
    var file: SSTableFile = _

    override def beforeEach() {
        file = new SSTableFile(TEST_FILE_PATH + System.currentTimeMillis)
    }

    override def afterEach() {
        file.close()
        deleteFile(file.filePath)
    }

    test("Get size should return number of bytes of in UTF-8 encoding") {
        assert(file.getSize("Hello world 1") == 13)
    }

    test("Write line should write data with new line delimiter") {
        file.writeLine("Hello world")
        assert(readFile(file.filePath) == "Hello world\n")
    }

    test("Write should write data into file") {
        file.write("Hello world")
        assert(readFile(file.filePath) == "Hello world")
    }

    test("Read should read file data of a given size in UTF-8 encoding") {
        writeToFile(file.filePath, "Hi Johny Walker")
        assert(file.read(8) == "Hi Johny")
    }

    test("Seek should skip to given position and write should rewrite data") {
        writeToFile(file.filePath, "Hello")
        file.seek(1)
        file.write("a")

        assert(readFile(file.filePath) == "Hallo")
    }

    test("Seek should skip to given position and read should start read from a given position") {
        writeToFile(file.filePath, "Hello")
        file.seek(2)

        assert(file.read(3) == "llo")
    }

    def readFile(filePath: String): String =
        new String(Files.readAllBytes(Paths.get(filePath)), UTF_8)

    def deleteFile(filePath: String) =
        Files.delete(Paths.get(filePath))

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
