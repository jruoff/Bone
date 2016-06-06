/** Simple interfaces for various String related tasks. */
object ExtraString {
  import java.nio.charset.StandardCharsets.UTF_8

  /** Convert text to bytes using UTF-8 encoding.
    *
    * @param str The String containing the text to be encoded.
    * @return The Vector containing the bytes representing the text.
    */
  def encode_utf8(str: String): Vector[Byte] = str.getBytes(UTF_8).toVector

  /** Convert bytes to text using UTF-8 encoding.
    *
    * @param dat The Vector containing the bytes to be decoded.
    * @return The String containing the text represented by the bytes.
    */
  def decode_utf8(dat: Vector[Byte]): String = new String(dat.toArray, UTF_8)

  /** Drop a given prefix from a string, if the string contains the prefix.
    *
    * @param str The string
    * @param prefix The prefix to be dropped form the string.
    * @return The string without the prefix.
    */
  def drop_prefix(str: String, prefix: String): String =
    if (str startsWith prefix) str drop prefix.length else str

  /** Drop a given suffix from a string, if the string contains the suffix.
    *
    * @param str The string
    * @param suffix The suffix to be dropped form the string.
    * @return The string without the suffix.
    */
  def drop_suffix(str: String, suffix: String): String =
    if (str endsWith suffix) str dropRight suffix.length else str

  /** Perform some basic string escaping.
    *
    * @param s The string to be escaped.
    * @return The escaped string.
    * @note This function does NOT provide sufficient escaping to be safely used in XML,
    *       HTML, JavaScript, JSON, SQL, etc. applications. Please use more appropriate
    *       escaping functions for these and other situations.
    */
  def escape(s: String): String = s
    .replace("\\", "\\\\")
    .replace("\t", "\\t")
    .replace("\r", "\\r")
    .replace("\n", "\\n")
    .replace("\"", "\\\"")

  /** Escape a string and surround by double-quotes.
    *
    * @param s A string to be escaped and surrounded by double-quotes.
    * @return The escaped and double-quote surrounded string.
    * @note See the note from [[escape]].
    */
  def quote(s: String): String = "\"" + escape(s) + "\""
}
