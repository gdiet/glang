package g4

class ActorSpec extends org.scalatest.freespec.AnyFreeSpec:
  val settings1: Settings = Settings(1, 1, 1)
  "The Actor should process single commands" - {

    "COPY #5 TO §A" in {
      val codeLines = Vector(Vector("COPY #5 TO §A"))
      val actor = Actors(settings1, codeLines).actors(0)
      actor.executeStep()
      assert(actor.registers('A') == 5)
    }

    "COPY $0 TO §A" in {
      val codeLines = Vector(Vector("COPY $0 TO §A"))
      val actor = Actors(settings1, codeLines).actors(0)
      actor.memory(0) = 5
      actor.executeStep()
      assert(actor.registers('A') == 5)
    }

  }
