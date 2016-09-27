import collection.mutable.Stack
import org.scalatest._
import util.Random
import collection.mutable.ListBuffer
import collection.mutable.TreeSet

class UnitSpec extends FlatSpec with Factory with Logic {

  "class Obj. owner method" should "set and return owner" in {
    val o = makeObjects(1)(0)
    val s = makeSubjects(List(1), List())(0)
    o.owner = s
    assert(o.owner == s)
  }

  "class Subj. public methods" should "add , return, remove object in field actualObjs" in {
    val o = makeObjects(1)(0)
    val s = makeSubjects(List(1), List())(0)
    assert(s.getObjs == 0)
    s.addObj(o)
    assert(s.getObjs == 1)
    s.removeObj(o)
    assert(s.getObjs == 0)
  }

  "func distribute" should "own all objects in subset" in {
    val o = makeObjects(10)
    val s = makeSubjects(List(1), o.dropRight(3))(0)
    assert(s.myObjs.size == 7)
  }
}

class IntegrationSpec extends FlatSpec with Factory with Logic {

  /*
     * #ER# = Equal Rights on objects
     * #UR# = Unequal Rights on objects
     */

  "3x10 Subj x Obj case" should "be resolved as 3x3x4 in any sequence #ER#" in {
    val o = makeObjects(10)
    val subjects = makeSubjects(List(1, 2, 3), o)
    solveProblem(o, subjects, List(), "add")
    // subjects.map(s => println("Name: " + s.myName + ", myObjs: " + (for (o <- s.myObjs.toList) yield o.myName) + ", actualObjs: " + s.actualObjs))
    assert((for (s <- subjects) yield s.actualObjs.size).toSet == TreeSet(3, 4, 3))
  }

  "4x10 Subj x Obj case" should "be resolved as 2x3x2x3 in any sequence #ER#" in {
    val o = makeObjects(10)
    val subjects = makeSubjects(List(1, 2, 3, 4), o)
    solveProblem(o, subjects, List(), "add")
    // subjects.map(s => println("Name: " + s.myName + ", myObjs: " + (for (o <- s.myObjs.toList) yield o.myName) + ", actualObjs: " + s.actualObjs))
    assert((for (s <- subjects) yield s.actualObjs.size).toSet == TreeSet(2, 2, 3, 3))
  }

  "3x21 Subj x Obj case" should "be resolved as 2x3x2x3 in any sequence #ER#" in {
    val o = makeObjects(21)
    val subjects = makeSubjects(List(1, 2, 3), o)
    solveProblem(o, subjects, List(), "add")
    // subjects.map(s => println("Name: " + s.myName + ", myObjs: " + (for (o <- s.myObjs.toList) yield o.myName) + ", actualObjs: " + s.actualObjs))
    assert((for (s <- subjects) yield s.actualObjs.size).toSet == TreeSet(7, 7, 7))
  }

  "100x100 Subj x Obj case" should "be resolved as 1 for each #ER#" in {
    val o = makeObjects(100)
    val subjects = makeSubjects((1 to 100).toList, o)
    solveProblem(o, subjects, List(), "add")
    assert(subjects.map(s => s.actualObjs.size === 1).foldLeft(true)(_ && _))
  }

  "9x1111 Subj x Obj case" should "be resolved as 1 for each #ER#" in {
    val o = makeObjects(1111)
    val subjects = makeSubjects((1 to 9).toList, o)
    solveProblem(o, subjects, List(), "add")
    assert((for (s <- subjects) yield s.actualObjs.size).toSet == TreeSet(123, 123, 123, 123, 124, 123, 124, 124, 124))
  }

  "9x87 Subj x Obj case" should "be resolved as 1 for each #ER#" in {
    val o = makeObjects(100)
    val subjects = makeSubjects(List(1, 2, 3, 4), o)
    solveProblem(o, subjects, List(), "add")
    assert(subjects.map(s => s.actualObjs.size === 25).foldLeft(true)(_ && _))
  }
}
class BitworksSpec extends FlatSpec with Factory with Logic {

  /*
     *
     * Bitworks assignment test reflection
     */

  "Test Bitworks" should "indicates algorithm workflof step by step " in {
    val o = makeObjects(10)

    println("\n\t--step 1--")
    val s1 = makeSubjects(List(1), List(o(2), o(3)))
    println(" +  " + s1(0).myName + " | " + (s1(0).myObjs.map(o => o.myName) mkString (" | ")) + "\n")
    solveProblem(o, s1, List(), "add")

    println((for (el <- o) yield el.myName) mkString (" | "))
    println((for (el <- o) yield if (el.owner != null) { el.owner.myName } else { " " }) mkString (" | "))

    println("\n\t--step 2--")
    val s2 = makeSubjects(List(2), List(o(2), o(4), o(5)))
    println(" +  " + s2(0).myName + " | " + (s2(0).myObjs.map(o => o.myName) mkString (" | ")) + "\n")
    solveProblem(o, s2, List(), "add")

    println((for (el <- o) yield el.myName) mkString (" | "))
    println((for (el <- o) yield if (el.owner != null) { el.owner.myName } else { " " }) mkString (" | "))

    println("\n\t--step 3--")
    val s3 = makeSubjects(List(3), List(o(2)))
    println(" +  " + s3(0).myName + " | " + (s3(0).myObjs.map(o => o.myName) mkString (" | ")) + "\n")
    solveProblem(o, s3, List(), "add")

    println((for (el <- o) yield el.myName) mkString (" | "))
    println((for (el <- o) yield if (el.owner != null) { el.owner.myName } else { " " }) mkString (" | "))

    println("\n\t--step 4--")
    println(" -  " + s2(0).myName + " | " + (s2(0).myObjs.map(o => o.myName) mkString (" | ")) + "\n")
    solveProblem(o, s1 ++ s3, s2, "del") // reflects lack of s2 subject

    println((for (el <- o) yield el.myName) mkString (" | "))
    println((for (el <- o) yield if (el.owner != null) { el.owner.myName } else { " " }) mkString (" | "))

    println("\n\t--step 5--")
    val s5 = makeSubjects(List(4), List(o(2), o(3)))
    println(" +  " + s5(0).myName + " | " + (s5(0).myObjs.map(o => o.myName) mkString (" | ")) + "\n")

    solveProblem(o, s5, List(), "add")
    println((for (el <- o) yield el.myName) mkString (" | "))
    println((for (el <- o) yield if (el.owner != null) { el.owner.myName } else { " " }) mkString (" | "))
  }
}
