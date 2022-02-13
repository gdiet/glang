package g2

val DIGITS: Int = sys.env.getOrElse("G_DIGITS", "4").toInt
val MEMORY: Int = sys.env.getOrElse("G_MEMORY", "4").toInt
val STACK: Int = sys.env.getOrElse("G_STACK", "4").toInt
