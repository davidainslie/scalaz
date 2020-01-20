package com.backwards.fp.chap3

import scala.concurrent.duration._
import scalaz.Scalaz._
import scalaz.{Monad, NonEmptyList, Order, Show}
import com.backwards.fp.time.Epoch

/**
 * 1. algebras define the interface between systems.
 * 2. modules are implementations of an algebra in terms of other algebras.
 * 3. interpreters are concrete implementations of an algebra for a fixed F[_].
 * 4. Test interpreters can replace the side-effecting parts of the system, giving a high amount of test coverage.
 */
object Chap3 extends App

// In FP, an algebra takes the place of an interface in Java, or the set of valid messages for an Actor in Akka.

/*
The following is now under time package
final case class Epoch(millis: Long) extends AnyVal {
  def +(d: FiniteDuration): Epoch = Epoch(millis + d.toMillis)
  def -(e: Epoch): FiniteDuration = (millis - e.millis).millis
}*/

trait Drone[F[_]] {
  def getBacklog: F[Int]
  def getAgents: F[Int]
}

final case class MachineNode(id: String)

object MachineNode {
  implicit val machineNodeShow: Show[MachineNode] =
    Show.showFromToString[MachineNode]

  implicit val machineNodeOrder: Order[MachineNode] =
    (x: MachineNode, y: MachineNode) => scalaz.Ordering.fromLessThan(x, y)((x, y) => x.id <= y.id)
}

trait Machines[F[_]] {
  def getTime: F[Epoch]
  def getManaged: F[NonEmptyList[MachineNode]]
  def getAlive: F[Map[MachineNode, Epoch]]
  def start(node: MachineNode): F[MachineNode]
  def stop(node: MachineNode): F[MachineNode]
}

// We need a WorldView class to hold a snapshot of our knowledge of the world.
// If we were designing this application in Akka, WorldView would probably be a var in a stateful Actor.

// WorldView aggregates the return values of all the methods in the algebras,
// and adds a pending field to track unfulfilled requests:
final case class WorldView(
  backlog: Int,
  agents: Int,
  managed: NonEmptyList[MachineNode],
  alive: Map[MachineNode, Epoch],
  pending: Map[MachineNode, Epoch],
  time: Epoch
)

trait DynAgents[F[_]] {
  def initial: F[WorldView]
  def update(old: WorldView): F[WorldView]
  def act(world: WorldView): F[WorldView]
}

// and now implement this with a module.
// A module depends only on other modules, algebras and pure functions, and can be abstracted over F.
// If an implementation of an algebraic interface is tied to a specific type, e.g. IO, it is called an interpreter.

// The Monad context bound means that F is monadic, allowing us to use map, pure and, of course, flatMap via for comprehensions.
final class DynAgentsModule[F[_]: Monad](D: Drone[F], M: Machines[F]) extends DynAgents[F] {
  import D._
  import M._

  // Our business logic will run in an infinite loop (pseudocode)

  // state = initial()
  // while true:
  //    state = update(state)
  //    state = act(state)

  // In initial we call all external services and aggregate their results into a WorldView.
  // We default the pending field to an empty Map:
  def initial: F[WorldView] =
    (getBacklog |@| getAgents |@| getManaged |@| getAlive |@| getTime) {
      case (db, da, mm, ma, mt) => WorldView(db, da, mm, ma, Map.empty, mt)
    }

  // update should call initial to refresh our world view, preserving known pending actions.
  // If a node has changed state, we remove it from pending,
  // and if a pending action is taking longer than 10 minutes to do anything,
  // we assume that it failed and forget that we asked to do it:
  def update(old: WorldView): F[WorldView] = for {
    snap <- initial
    changed = symdiff(old.alive.keySet, snap.alive.keySet)
    pending = (old.pending -- changed).filterNot {
      case (_, started) => (snap.time - started) >= 10.minutes
    }
    update = snap.copy(pending = pending)
  } yield update

  private def symdiff[T](a: Set[T], b: Set[T]): Set[T] =
    (a union b) -- (a intersect b)

  def act(world: WorldView): F[WorldView] = world match {
    case NeedsAgent(node) =>
      start(node) >| world.copy(pending = Map(node -> world.time))

    case Stale(nodes) =>
      nodes.foldLeftM(world) { (world, n) =>
        for {
          _ <- stop(n)
          update = world.copy(pending = world.pending + (n -> world.time))
        } yield update
      }

    case _ => world.pure[F]
  }

  // We need to add agents to the farm if there is a backlog of work, we have no agents, we have no nodes alive,
  // and there are no pending actions. We return a candidate node that we would like to start:
  private object NeedsAgent {
    def unapply(world: WorldView): Option[MachineNode] = world match {
      case WorldView(backlog, 0, managed, alive, pending, _) if backlog > 0 && alive.isEmpty && pending.isEmpty =>
        Option(managed.head)
      case _ =>
        None
    }
  }

  // If there is no backlog, we should stop all nodes that have become stale (they are not doing any work).
  // However, since Google charge per hour we only shut down machines in their 58th minute to get the most out of our money.
  // We return the non-empty list of nodes to stop.
  // As a financial safety net, all nodes should have a maximum lifetime of 5 hours.
  private object Stale {
    def unapply(world: WorldView): Option[NonEmptyList[MachineNode]] = world match {
      case WorldView(backlog, _, _, alive, pending, time) if alive.nonEmpty =>
        (alive -- pending.keys).collect {
          case (n, started) if backlog == 0 && (time - started).toMinutes % 60 >= 58 => n
          case (n, started) if (time - started) >= 5.hours => n
        }.toList.toNel

      case _ =>
        None
    }
  }
}