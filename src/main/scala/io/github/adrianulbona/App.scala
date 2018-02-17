package io.github.adrianulbona

/**
  * @author ${user.name}
  */
object App {

  trait Event

  case class grant(task: String, resource: String) extends Event

  case class release(task: String, resource: String) extends Event

  class R1R2 extends Monitor[Event] {
    whenever {
      case grant(t, r) ⇒ Granted(t, r)
      case release(t, r) if !Granted(t, r) ⇒ error
    }

    case class Granted(t: String, r: String) extends hot {
      when {
        case release(`t`, `r`) ⇒ ok
        case grant(_, `r`) ⇒ error
      }
    }

  }

  class R1 extends Monitor[Event] {
    whenever {
      case grant(t, r) ⇒ hot {
        case release(`t`, `r`) ⇒ ok
        case grant(_, `r`) ⇒ error
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val obs = new R1R2
    obs.verify(grant("t1", "A"))
    obs.verify(release("t1", "A"))
    obs.verify(release("t1", "A"))
    obs.end()
  }

}
