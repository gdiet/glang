package g4

class ActorSpec extends org.scalatest.freespec.AnyFreeSpec:
  val settings1: Settings = Settings(1, 5, 5)
  "The Actor should process single commands" - {

    "COPY #5 TO 3" in {
      val codeLines = Vector(Vector("COPY #5 TO 3"))
      val actor = Actors(settings1, codeLines).actors(0)
      actor.executeStep()
      assert(actor.memory(3) == 5)
    }

    "COPY 0 TO 1" in {
      val codeLines = Vector(Vector("COPY 0 TO 1"))
      val actor = Actors(settings1, codeLines).actors(0)
      actor.memory(0) = 5
      actor.executeStep()
      assert(actor.memory(1) == 5)
    }

    "ADD #5 TO 0,1" in {
      val codeLines = Vector(Vector("ADD #5 TO 0 OVERFLOW 1"))
      val actor = Actors(settings1, codeLines).actors(0)
      actor.memory(0) = 7
      actor.executeStep()
      assert(actor.memory(0) == 2)
      assert(actor.memory(1) == 1)
    }

  }
