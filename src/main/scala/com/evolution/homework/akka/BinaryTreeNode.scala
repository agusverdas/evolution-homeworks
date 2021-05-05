package com.evolution.homework.akka

import akka.actor.{Actor, ActorRef, Props}

object BinaryTreeNode {
  private sealed trait Position

  private case object Left extends Position

  private case object Right extends Position

  def props(elem: Int, initiallyRemoved: Boolean): Props = Props(new BinaryTreeNode(elem, initiallyRemoved))
}

final class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet.Operation._
  import BinaryTreeSet.OperationReply._

  private var subtrees = Map[Position, ActorRef]()
  private var removed = initiallyRemoved

  override def receive: Receive = {
    case i: Insert => doInsert(i)
    case c: Contains => doContains(c)
    case r: Remove => doRemove(r)
  }

  private def doInsert(m: Insert): Unit = {
    if (m.elem == elem) {
      removed = false
      m.requester ! OperationFinished(m.id)
    }
    else if (m.elem < elem && subtrees.contains(Left)) subtrees(Left) ! m
    else if (m.elem < elem && !subtrees.contains(Left)) {
      val actor = context.actorOf(BinaryTreeNode.props(m.elem, initiallyRemoved = false))
      subtrees = subtrees.updated(Left, actor)
      m.requester ! OperationFinished(m.id)
    }
    else if (m.elem > elem && subtrees.contains(Right)) subtrees(Right) ! m
    else if (m.elem > elem && !subtrees.contains(Right)) {
      val actor = context.actorOf(BinaryTreeNode.props(m.elem, initiallyRemoved = false))
      subtrees = subtrees.updated(Right, actor)
      m.requester ! OperationFinished(m.id)
    }
  }

  private def doContains(m: Contains): Unit = {
    if (!removed && m.elem == elem) m.requester ! ContainsResult(m.id, result = true)
    else if (removed && m.elem == elem) m.requester ! ContainsResult(m.id, result = false)
    else if (m.elem < elem && subtrees.contains(Left)) subtrees(Left) ! m
    else if (m.elem < elem && !subtrees.contains(Left)) m.requester ! ContainsResult(m.id, result = false)
    else if (m.elem > elem && subtrees.contains(Right)) subtrees(Right) ! m
    else if (m.elem > elem && !subtrees.contains(Right)) m.requester ! ContainsResult(m.id, result = false)
  }

  private def doRemove(m: Remove): Unit = {
    if (m.elem == elem) {
      removed = true
      m.requester ! OperationFinished(m.id)
    }
    else if (m.elem < elem && subtrees.contains(Left)) subtrees(Left) ! m
    else if (m.elem < elem && !subtrees.contains(Left)) m.requester ! OperationFinished(m.id)
    else if (m.elem > elem && subtrees.contains(Right)) subtrees(Right) ! m
    else if (m.elem > elem && !subtrees.contains(Right)) m.requester ! OperationFinished(m.id)
  }

}

