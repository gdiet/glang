package g4

case class Settings(
                     DIGITS: Int = sys.env.getOrElse("G_DIGITS", "4").toInt,
                     MEMORY: Int = sys.env.getOrElse("G_MEMORY", "4").toInt,
                     STACK: Int = sys.env.getOrElse("G_STACK", "4").toInt
                   )
