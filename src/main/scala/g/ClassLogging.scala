package g

trait ClassLogging:
  private val logger: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(getClass.getName.stripSuffix("$"))
  def trace(msg: => String): Unit = if (logger.isTraceEnabled) logger.trace(msg)
  def debug(msg: => String): Unit = if (logger.isDebugEnabled) logger.debug(msg)
  def info (msg: => String): Unit = if (logger.isInfoEnabled)  logger.info (msg)
  def warn (msg: => String): Unit = if (logger.isWarnEnabled)  logger.warn (s"***: $msg")
  def error(msg: => String): Unit = logger.error(s"!!!: $msg")
  def trace(msg: => String, e: => Throwable): Unit = logger.trace(msg, e)
  def debug(msg: => String, e: => Throwable): Unit = logger.debug(msg, e)
  def info (msg: => String, e: => Throwable): Unit = logger.info(msg, e)
  def warn (msg: => String, e: => Throwable): Unit = logger.warn(s"***: $msg", e)
  def error(msg: => String, e: => Throwable): Unit = logger.error(s"!!!: $msg", e)
