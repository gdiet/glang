package g3

class ManagerPass1Spec extends org.scalatest.freespec.AnyFreeSpec:
  "In pass 1, the manager should recognize function definitions" - {
    "without parameters" in {
      assert(
        Manager(Seq("", "FUNCTION add_unsigned:")).functionDefinitions == Map("add_unsigned" -> 1)
      )
    }
    "with parameters" in {
      assert(
        Manager(Seq("FUNCTION add_unsigned: $x $y $z")).functionDefinitions == Map("add_unsigned" -> 0)
      )
    }
  }

  "In pass 1, the manager should recognize goto targets" - {
    "followed by no command" in {
      assert(
        Manager(Seq("", "target:")).gotoTargets == Map("target" -> 1)
      )
    }
    "followed immediately by a command" in {
      assert(
        Manager(Seq("target:COMMAND", "")).gotoTargets == Map("target" -> 0)
      )
    }
    "followed by spaces and a command" in {
      assert(
        Manager(Seq("", "loop:  COMMAND", "")).gotoTargets == Map("loop" -> 1)
      )
    }
  }
