package g

val REGISTER_WIDTH: Int = sys.env.getOrElse("G_REGISTER_WIDTH", "16").toInt
val MEMORY_SIZE: Int = sys.env.getOrElse("G_MEMORY_SIZE", "16").toInt
