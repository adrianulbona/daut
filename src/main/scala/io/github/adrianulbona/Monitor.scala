package io.github.adrianulbona

/**
  * Created by adrianulbona on 25/12/2016.
  */
class Monitor[E <: AnyRef] {
  val monitorName = this.getClass.getSimpleName
  var monitors: List[Monitor[E]] = List()
  var states: Set[state] = Set()
  var statesToAdd: Set[state] = Set()
  var statesToRemove: Set[state] = Set()

  def monitor(monitors: Monitor[E]*) {
    this.monitors ++= monitors
  }

  type Transitions = PartialFunction[E, Set[state]]

  def noTransitions: Transitions = {
    case _ if false => null
  }

  class state {
    var transitions: Transitions = noTransitions

    def when(ts: Transitions) {
      this.transitions = ts
    }

    def apply(event: E): Option[Set[state]] = {
      if (transitions.isDefinedAt(event)) Some(transitions(event)) else None
    }
  }

  class always extends state

  class hot extends state

  case object error extends state

  case object ok extends state

  def stateExists(pred: PartialFunction[state, Boolean]): Boolean = {
    states exists (pred orElse {
      case _ => false
    })
  }

  def state(ts: Transitions): state = {
    val e = new state
    e.when(ts)
    e
  }

  def always(ts: Transitions): state = {
    val e = new always
    e.when(ts)
    e
  }

  def hot(ts: Transitions): state = {
    val e = new hot
    e.when(ts)
    e
  }

  def error(msg: String): state = {
    println("\n*** " + msg + "\n")
    error
  }

  def whenever(ts: Transitions): Unit = {
    states += always(ts)
  }

  implicit def stateToBoolean(s: state): Boolean = states contains s

  implicit def unitToSet(u: Unit): Set[state] = Set(ok)

  implicit def stateToSet(s: state): Set[state] = Set(s)

  implicit def statePairToSet(ss: (state, state)): Set[state] = Set(ss._1, ss._2)

  implicit def stateTripleToSet(ss: (state, state, state)): Set[state] = Set(ss._1, ss._2, ss._3)

  def verify(event: E) {
    states foreach (s => {
      s(event) match {
        case None =>
        case Some(stateSet) => {
          if (stateSet contains error) {
            println("\n*** error!\n")
          } else {
            statesToAdd ++= stateSet.filter(_ != ok)
          }
          if (!s.isInstanceOf[always]) {
            statesToRemove += s
          }
        }
      }
    })
    states --= statesToRemove
    states ++= statesToAdd
    statesToAdd = Set()
    statesToRemove = Set()

    monitors foreach (_.verify(event))
  }

  def end(): Unit = {
    val hotStates = states filter (_.isInstanceOf[hot])
    if (hotStates.nonEmpty) {
      println("*** hot states in " + monitorName)
      hotStates foreach println
    }

    monitors foreach (_.end())
  }
}