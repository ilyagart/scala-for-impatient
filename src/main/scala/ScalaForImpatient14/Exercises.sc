import com.sun.org.apache.xpath.internal.operations.{Minus, Plus}
// 1
/*
Your Java Development Kit distribution has the source code for much of the JDK in the
src.zip file. Unzip and search for case labels (regular expression case [^:]+:). Then
look for comments starting with // and containing [Ff]alls? thr to catch comments such
as // Falls through or // just fall thru. Assuming the JDK programmers
follow the Java code convention, which requires such a comment, what percentage of cases
falls through?
   */

// 2
def swap(pair: (Int, Int)): (Int, Int) = pair match {
  case (a, b) => (b, a)
  case _      => throw new Error("Not a pair")
}

swap(1, 2)

// 3
def swapFirstTwoElements(ar: Array[Int]): Array[Int] = ar match {
  case Array(a, b)            => Array(b, a)
  case Array(a, b, rest @ _*) => Array(b, a) ++ rest
}

swapFirstTwoElements(Array(1, 2, 3, 4, 5))
swapFirstTwoElements(Array(1, 2))

// 4
abstract class Item

case class Article(description: String, price: Double) extends Item

case class Bundle(description: String, discount: Double, items: Item*)
    extends Item

case class Multiple(numberOfItems: Int, item: Item) extends Item {
  def price(it: Item): Double = it match {
    case Article(_, p)             => p
    case Bundle(_, disc, its @ _*) => its.map(price).sum - disc
    case Multiple(n, i)            => n * price(i)
  }
  def price: Double = this match {
    case Multiple(n, i) => n * price(i)
  }
}

val bundle = Bundle(
  "Father's day special",
  20.0,
  Article("Scala for the Impatient", 39.95),
  Bundle(
    "Anchor Distillery Sampler",
    10.0,
    Article("Old Potrero Straight Rye Whiskey", 79.95),
    Article("Junípero Gin", 32.95)
  )
)
val article = Article("Some random stuff", 100)
val anotherBundle = Bundle("Big bundle", 10, article, article, article)

val m = Multiple(0, article)
m.price(Multiple(3, article)) // 300
m.price(Multiple(4, bundle)) // 491
Multiple(2, anotherBundle).price // 580

// 5
def leafSum(list: List[Any]): Double = {
  list.foldLeft(0.0) { (acc: Double, leaf) =>
    leaf match {
      case n: Number     => acc + n.doubleValue()
      case list: List[_] => acc + leafSum(list)
      case _             => acc
    }
  }
}

val list: List[Any] = List(List(3, 8), 2, List(5))
leafSum(list) // 18

// 6
sealed abstract class BinaryTree

case class Leaf(value: Int) extends BinaryTree

case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree

def sumLeafs(tree: BinaryTree): Double = {
  tree match {
    case Leaf(value)       => value
    case Node(left, right) => sumLeafs(left) + sumLeafs(right)
  }
}

val tree: BinaryTree = Node(Node(Leaf(2), Leaf(4)), Node(Leaf(1), Leaf(6)))
sumLeafs(tree) // 13

// 7
case class ExtendedNode(children: BinaryTree*) extends BinaryTree

def sumLeafs2(tree: BinaryTree): Double = {
  tree match {
    case Leaf(value)                 => value
    case ExtendedNode(children @ _*) => children.map(sumLeafs2).sum
  }

}
val tree2: BinaryTree =
  ExtendedNode(ExtendedNode(Leaf(3), Leaf(8)), Leaf(2), ExtendedNode(Leaf(5)))
sumLeafs2(tree2) // 18

// 8
case class NodeWithOperation(op: Char, children: BinaryTree*)
    extends BinaryTree

def eval(tree: BinaryTree): Int = {
  tree match {
    case Leaf(value) => value
    case NodeWithOperation(op, children @ _*) =>
      children
        .map(eval)
        .reduce((left, right) => {
          op match {
            case '+' => left + right
            case '-' => left - right
            case '*' => left * right
            case '/' => left / right
            case _   => throw new Error("No such operation =)")
          }
        })
  }
}
val bt: BinaryTree =
  NodeWithOperation(
    '+',
    NodeWithOperation('*', Leaf(3), Leaf(8)),
    Leaf(2),
    NodeWithOperation('-', Leaf(5))
  )
//TODO make this work
eval(bt) // should be 21

class Op private (val identity: Int, op: (Int, Int) => Int) {
  def apply(a: Int, b: Int): Int = op(a, b)
}

object Op {
  val Plus = new Op(0, _ + _)
  val Minus = new Op(0, _ - _)
  val Product = new Op(1, _ * _)
}

case class CustomNode(op: Op, children: BinaryTree*) extends BinaryTree

def eval2(bt: BinaryTree): Int = bt match {
  case leaf: Leaf => leaf.value
  case node: CustomNode =>
    node.children.foldLeft(node.op.identity) { (acc, item) =>
      node.op(acc, eval2(item))
    }
}
val bt2: BinaryTree =
  CustomNode(
    Op.Plus,
    CustomNode(Op.Product, Leaf(3), Leaf(8)),
    Leaf(2),
    CustomNode(Op.Minus, Leaf(5))
  )

eval2(bt2)

// 9
def sumOfNonNone(values: List[Option[Int]]): Int = {
  values.map(_.getOrElse(0)).sum
//  values.foldLeft(0){(acc,item) => acc + item.getOrElse(0)}
}
sumOfNonNone(List(Some(1), Some(3), None, Some(4), None))

// 10
def compose(f1: Double => Option[Double],
            f2: Double => Option[Double]): Double => Option[Double] = {
  a: Double =>
    f2(a) match {
      case Some(value) => f1(value)
      case None        => None
    }
}

def f(x: Double) = if (x != 1) Some(1 / (x - 1)) else None
def g(x: Double) = if (x >= 0) Some(scala.math.sqrt(x)) else None
val h = compose(g, f) // h(x) should be g(f(x))
h(2)
h(1)
h(0)