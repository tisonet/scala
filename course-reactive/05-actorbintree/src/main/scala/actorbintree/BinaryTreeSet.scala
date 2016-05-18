/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import actorbintree.BinaryTreeNode.{CopyFinished, CopyTo}
import akka.actor._

import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot
  var pendingOperations = Queue.empty[Operation]

  def receive = normal

  val normal: Receive = {
    case op: Operation => {
      root ! op
    }
    case GC => {
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
    }
  }

  def garbageCollecting(newRoot: ActorRef): Receive = {
    case op: Operation => {
      pendingOperations = pendingOperations.enqueue(op)
    }
    case CopyFinished => {
      root = newRoot
      processOperations(pendingOperations)
      context.become(normal)
    }
  }


  def processOperations(p:Queue[Operation]) {
    // Recursively sends all pending operations to new root node
    if(p.nonEmpty) {
          p.dequeue match {
            case (op, xs) =>
              root ! op
              processOperations(xs)
            }
        }
    else {
        pendingOperations = Queue.empty[Operation]
      }
    }
}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  def receive = receiveOperations orElse receiveCopy

  val receiveCopy: Receive = {
    case copyTo@CopyTo(treeNode) => {
      // Add itself into new tree only when not deleted
      if(!removed){
        treeNode ! Insert(self, 100000, elem)
      }

      delegateCopy(Right, copyTo)
      delegateCopy(Left, copyTo)

      context.become(copying(subtrees.values.toSet, removed))
    }
  }

  val receiveOperations: Receive = {
    case Contains(r, id, e) if e == elem => r ! ContainsResult(id, result = !removed)
    case op@Contains(r, id, e) if e > elem => delegateOrNotContains(Right, op)
    case op@Contains(r, id, e) if e < elem => delegateOrNotContains(Left, op)

    case Insert(r, id, e) if e == elem => {
      removed = false
      r ! OperationFinished(id)
    }
    case op@Insert(_, id, e) if e > elem => delegateOrCreate(Right, op)
    case op@Insert(_, id, e) if e < elem => delegateOrCreate(Left, op)

    case Remove(r, id, e) if e == elem => {
      removed = true
      r ! OperationFinished(id)
    }
    case op@Remove(_, _, e) if e > elem => delegateOrFinished(Right, op)
    case op@Remove(_, _, e) if e < elem => delegateOrFinished(Left, op)
  }

  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    if(expected.isEmpty && insertConfirmed) {
      context.parent ! CopyFinished
      context.stop(self)
    }

    {
      case OperationFinished(id) => {
        context.become(copying(expected, insertConfirmed = true))
      }
      case CopyFinished => {
        context.become(copying(expected.-(sender), insertConfirmed))
      }
    }
  }

  def delegateOrFinished(position: Position, operation: Operation) = {
    subtrees.get(position) match {
      case Some(a) => a ! operation
      case None => {
        operation.requester ! OperationFinished(operation.id)
      }
    }
  }

  def delegateOrCreate(position: Position, operation: Operation) = {
    subtrees.get(position) match {
      case Some(a) => a ! operation
      case None => {
        subtrees = subtrees.updated(position, context.actorOf(props(operation.elem, initiallyRemoved = false)))
        operation.requester ! OperationFinished(operation.id)
      }
    }
  }

  def delegateOrNotContains(position: Position, operation: Operation) = {
    subtrees.get(position) match {
      case Some(a) => a ! operation
      case None => {
        operation.requester ! ContainsResult(operation.id, result = false)
      }
    }
  }

  def delegateCopy(position: Position, copyTo: CopyTo) = {
    subtrees.get(position) match {
      case Some(a) => a ! copyTo
      case None =>
    }
  }
}
