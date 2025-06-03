package bridge.exceptions

final class FormatException(
  private val message: String = "",
  private val cause: Throwable = None.orNull
) extends Exception(message, cause) {}

final class VersionException(maxSupported: Int, actual: Int)
    extends Exception(s"Unsupported version: $actual (max supported: $maxSupported)") {}
