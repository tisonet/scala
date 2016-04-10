package tisonet.scala.steladb.sstable

import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets._
import java.nio.file.{Files, Paths}

trait IOSuite {
    val TEST_FILE_PATH = Paths.get("test_sstable").toAbsolutePath.toString

    def testFilePath = TEST_FILE_PATH + System.currentTimeMillis

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
