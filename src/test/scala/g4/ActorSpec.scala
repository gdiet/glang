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

    "COPY #x TO §A" in {
      val codeLines = Vector(Vector("COPY #x TO §A"))
      val actor = Actors(settings1, codeLines).actors(0)
      actor.context = List(INVALID -> Map("x" -> "123"))
      actor.executeStep()
      assert(actor.registers('A') == 3)
    }

    "ADD #5 TO §A,§B" in {
      val codeLines = Vector(Vector("ADD #5 TO §A,§B"))
      val actor = Actors(settings1, codeLines).actors(0)
      actor.registers += 'A' -> 7
      actor.executeStep()
      assert(actor.registers('A') == 2)
      assert(actor.registers('B') == 1)
    }

  }
