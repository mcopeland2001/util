package com.selfip.mcopeland.utils.scalautils

import org.scalatest.FunSuite
import scala.xml.{Elem, XML}

/**
 * @author Michael Copeland
 *
 *         Note: this test makes no assertions. It was originally developed as a spike (a throw-away test) that primarily
 *         tested that the required code compiled and ran without exceptions. I decided to let it remain for documentary
 *         purposes as it demonstrates how I create a BidirectionalTree with heterogenous node value types that automatically
 *         render themselves with an appropriate renderer for the node value type by searching in the implicit scope.
 */
class BidirectionalTreeTest extends FunSuite with Datastructures {

  abstract class NodeValueRenderer[A <: NodeValueBase] {
    def render(nodeValue: A): String
  }

  case class TreeNodeType1(override val nodeValue: NodeValueBase)(override val children: DescendingTree[NodeValueBase]*) extends DescendingTree[NodeValueBase]

  case class StringNodeValue(value: String) extends NodeValueBase {
    def render: String = renderNodeValue(this)
  }

  case class IntNodeValue(value: Int) extends NodeValueBase {
    def render: String = renderNodeValue(this)
  }

  implicit object StringNodeValueRenderer extends NodeValueRenderer[StringNodeValue] {
    def render(nodeValue: StringNodeValue): String = nodeValue.value
  }

  implicit object IntNodeValueRenderer extends NodeValueRenderer[IntNodeValue] {
    def render(nodeValue: IntNodeValue): String = "node" + nodeValue.value
  }



  def renderNodeValue[A <: NodeValueBase : NodeValueRenderer](nodeValue: A) = implicitly[NodeValueRenderer[A]].render(nodeValue)

  def addTreeNodeTag(b: String, node: BidirectionalTree[NodeValueBase], closing: Boolean): String =
    b + treeNodeXmlTag(node, closing)

  def treeNodeXmlTag(node: BidirectionalTree[NodeValueBase], closing: Boolean): String =
    ("\t" * node.depth) + "<" + (if(closing) "/" else "") + node.nodeValue.render + (if(!closing) " path=\"" + node.path(_.value.toString) + "\"" else "") + ">\n"



  test("construct a tree and render it by folding with implicit renderers") {
    val leafNode1 = TreeNodeType1(StringNodeValue("leafNode1"))()
    val leafNode2 = TreeNodeType1(IntNodeValue(2))()
    val leafNode3 = TreeNodeType1(StringNodeValue("leafNode3"))()
    val leafNode4 = TreeNodeType1(IntNodeValue(4))()
    val internalNode1 = TreeNodeType1(StringNodeValue("internalNode1"))(leafNode1, leafNode2)
    val internalNode2 = TreeNodeType1(StringNodeValue("internalNode2"))(leafNode3, leafNode4)
    val rootNode = TreeNodeType1(StringNodeValue("root"))(internalNode1, internalNode2)

    val tree: BidirectionalTree[NodeValueBase] = new BidirectionalTree[NodeValueBase](rootNode)

    val xml = tree.fold("")(_ + _){addTreeNodeTag(_, _, closing = false)}{addTreeNodeTag(_, _, closing = true)}{(x) => x.nodeValue.value != 4}
    println("\n" + xml)
  }

}

