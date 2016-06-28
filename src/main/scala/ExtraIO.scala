/** Simple functional interfaces for various IO related tasks. */
object ExtraIO {
  import java.nio.file.{Files, Paths}
  import annotation.tailrec
  import ExtraString._
  import scalaz.effect.IO

  /** Write bytes to a file.
    *
    * @param file_name The name of the file to write to.
    * @param data The bytes to write to the file.
    */
  def write_all(file_name: String, data: Vector[Byte]): IO[Unit] =
    IO(Files.write(Paths get file_name, data.toArray))

  /** Read bytes from a file.
    *
    * @param file_name The name of the file to read from.
    * @return A Vector containing the bytes in the file.
    */
  def read_all(file_name: String): IO[Vector[Byte]] =
    IO(Files.readAllBytes(Paths get file_name).toVector)

  /** Write text to a file, using UTF-8 encoding.
    *
    * @param file_name The name of the file to write to.
    * @param data The text to write to the file.
    */
  def write_all_utf8(file_name: String, data: String): IO[Unit] =
    write_all(file_name, encode_utf8(data))

  /** Read text from a file, using UTF-8 encoding.
    *
    * @param file_name The name of the file to read from.
    * @return A String containing the text in the file.
    */
  def read_all_utf8(file_name: String): IO[String] =
    read_all(file_name) map decode_utf8

  /** Create the path to a file.
    *
    * @param path_inc_file_name The path including the file name.
    * @return Monadic process that ensures all directories in the path exist.
    */
  def make_directories(path_inc_file_name: String): IO[Unit] =
    IO(Files.createDirectories(Paths.get(path_inc_file_name).getParent))

  /** Make a function to execute a shell command.
    *
    * @param command The command to execute.
    * @return A function executing the command taking STDIN to STDOUT.
    */
  def command_to_function(command: String): Vector[Byte] => IO[Vector[Byte]] =
    (input: Vector[Byte]) => IO {
      import sys.process.{Process, ProcessIO}

      var v = Vector.newBuilder[Byte]

      @tailrec
      def reader(input: java.io.InputStream): Unit = {
        val c = input.read()
        if (c != -1) {
          v += c.toByte
          reader(input)
        }
      }

      def writer(output: java.io.OutputStream): Unit = {
        input.foreach(output write _.toInt)
        output.close()
      }

      val io = new ProcessIO(writer, reader, _.close())
      Process(command).run(io).exitValue()
      v.result() // TODO: Is this safe, or is this a race condition?
    }
}
