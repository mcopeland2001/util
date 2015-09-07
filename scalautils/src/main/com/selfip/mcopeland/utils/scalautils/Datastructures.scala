package com.selfip.mcopeland.utils.scalautils

/**
 * @author Michael Copeland
 */
trait Datastructures {

  trait DescendingTree[A] {
    def nodeValue: A
    def children: Seq[DescendingTree[A]]
  }


  implicit class BidirectionalTree[A](val treeNode: DescendingTree[A]) extends DescendingTree[A] {

    def nodeValue: A = treeNode.nodeValue

    def parent: Option[BidirectionalTree[A]] = None

    lazy val children: Seq[BidirectionalTree[A]] = treeNode.children map { (node) => {
      new BidirectionalTree[A](node) {
        override def parent: Option[BidirectionalTree[A]] = Option(BidirectionalTree.this)
      }
    }
    }

    def depth: Int = 1 + (parent map {_.depth} getOrElse(-1))

    def fold[B](z: B)(combineOp: (B, B) => B)(fNode: (B, BidirectionalTree[A]) => B)
               (fAfterChildren: (B, BidirectionalTree[A]) => B = {(x:B, y:BidirectionalTree[A]) => x})
               (filter: BidirectionalTree[A] => Boolean = {(x) => true}): B = {

      val outChildren: Seq[B] = for(child <- children) yield child.fold(z)(combineOp)(fNode)(fAfterChildren)(filter)
      val outNode: B = if(filter(this)) fNode(z, this) else z
      val nodePlusChildren = combineOp(outNode, outChildren.foldLeft(z)(combineOp))
      if(filter(this)) fAfterChildren(nodePlusChildren, this) else nodePlusChildren
    }

    def path(f: A => String): String = (parent.map { p => p.path(f) + "/" } getOrElse "") + f(nodeValue)
  }

  trait NodeValueBase {
    def value: Any
    def render: String
  }

}
