package g3

trait ClassLogging:
  protected val log: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(getClass.getName.stripSuffix("$"))
