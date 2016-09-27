/*
 *
 * Bitworks 
 * Shared object ownership problem
 * author Dmitry V. Frank
 *
 */
import util.Random
import collection.mutable.ListBuffer

trait Logic {

  def checkOwnership(o: Obj, s: Subj, objs: List[Obj]): Unit = {
    if (o.owner.getObjs > (s.getObjs + 1)) {
      val oldOwner = o.owner
      o.owner.removeObj(o)
      o.owner = s
      s.addObj(o)
      share(oldOwner, objs)
    }
  }

  final def freebie(objs: List[Obj], s: Subj): Unit = {
    for (o <- objs) s.myObjs.contains(o) match {
      case true =>
        (o.owner == null) match {
          case true =>
            o.owner = s
            s.addObj(o)
          case false =>
        }
      case _ =>
    }
  }

  final def share(s: Subj, objs: List[Obj]): Unit = {
    (s.amILazy) match {
        case true =>
        case false =>
          for (o <- objs) s.myObjs.contains(o) match {
            case true =>
              o.owner.equals(s) match {
                case true =>
                case false => {
                  checkOwnership(o, s, objs)
                }
              }
            case false =>
          }
      }
  }

  final def solveProblem(objs: List[Obj], subjs: List[Subj], rsubj: List[Subj], action: String): Unit = {
    action match {
      case "add" => 
        subjs.foreach(s => { 
          freebie(objs, s)
          share(s, objs)
        })
      case "del" =>
        rsubj.foreach(rs => rs.actualObjs.foreach(o => o.owner = null))
        subjs.foreach(s => freebie(objs, s))
    }
  }
}

trait Factory {
  def makeObjects(n: Int): List[Obj] = {
    val id = 107
    val objs = (for (i <- id until id + n) yield new Obj(i.asInstanceOf[Char].toString)).toList
    objs
  }

  def makeSubjects(n: List[Int], o: List[Obj]): List[Subj] = {
    val s = (for (i <- n) yield new Subj((i + 96).asInstanceOf[Char].toString.toUpperCase, o, false)).toList
    s
  }
}

class Obj(
  val myName: String,
  private var ownerRef: Subj = null) {

  def owner = ownerRef
  def owner_=(o: Subj) = {
    ownerRef = o
  }
}

class Subj(
  val myName: String,
  val myObjs: List[Obj],
  val amILazy: Boolean) {
  var actualObjs = new ListBuffer[Obj]

  def getObjs = actualObjs.size

  def addObj(o: Obj): ListBuffer[Obj] = {
    actualObjs += o
    actualObjs
  }

  def removeObj(o: Obj): ListBuffer[Obj] = {
    actualObjs -= o
    actualObjs
  }
}