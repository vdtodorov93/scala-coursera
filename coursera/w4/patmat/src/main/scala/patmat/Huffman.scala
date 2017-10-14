package patmat

import common._

/**
  * Assignment 4: Huffman coding
  *
  */
object Huffman {


  def main(args: Array[String]): Unit = {
    val orderedLeafList = makeOrderedLeafList(Huffman.times(List('a', 'b', 'c', 'b', 'a')))
//    println(orderedLeafList)
//    println(orderedLeafList)
//    println(singleton (combine(combine(orderedLeafList))))
//    println(until(singleton, combine)(orderedLeafList))
//    println(createCodeTree(List('a', 'b', 'c', 'b', 'a')))
    val ct = createCodeTree(List('a', 'b', 'c', 'b', 'a', 'a'))
//    println(ct)
    println(decode(ct, List(0, 1)))
    println(decode(ct, List(0, 0)))
    println(decode(ct, List(1)))
    println(decode(ct, List(0, 1, 0, 0, 1)))
    println(encode(ct)(List('b', 'c', 'a')))
  }

  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  def weight(tree: CodeTree): Int = {
    tree match {
      case Leaf(_, w) => w
      case Fork(left, right, _, _) => weight(left) + weight(right)
    }
  }

  def chars(tree: CodeTree): List[Char] = {
    tree match {
      case Leaf(c, _) => List(c)
      case Fork(left, right, _, _) => chars(left) ++ chars(right)
    }
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))


  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = {
    chars.groupBy(c => c).map(touple => {
      touple match {
        case (c, l) => (c, l.size)
      }
    }).toList
  }

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    freqs.map(t => {
      t match {
        case (ch, weight) => Leaf(ch, weight)
      }
    }).sortBy(_.weight)
  }

  def singleton(trees: List[CodeTree]): Boolean = {
    trees match {
      case _ :: Nil => true
      case _ => false
    }
  }

  def combine(trees: List[CodeTree]): List[CodeTree] = {
    trees match {
      case Nil => trees
      case _ :: Nil => trees
//      case _ :: _ :: Nil =>
//      case l :: r :: xs => (makeCodeTree(l, r) :: xs).sortBy(t => -weight(t))
      case l :: r :: xs => (makeCodeTree(l, r) :: xs).sortBy(weight)
    }
  }

  def until(pred: (List[CodeTree]) => Boolean, func: (List[CodeTree]) => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if (pred(trees)) {
      return trees
    }

    until(pred, func)(func(trees))
  }

  def createCodeTree(chars: List[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head
  }


  // Part 3: Decoding

  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
//    bits match {
//      case Nil => Nil
//      case 0 :: xs => {
//        tree match {
//          case Leaf(c, _) => c :: Nil
//          case Fork(left, _, _, _) => decode(left, xs)
//        }
//      }
//
//      case 1 :: xs => {
//        tree match {
//          case Leaf(c, _) => c :: Nil
//          case Fork(_, right, _, _) => decode(right, xs)
//        }
//      }
//
//      case _ => throw new IllegalArgumentException("bits can be only 1 and 0")
//    }
    decodeHelper(tree, tree, bits)
  }

  def decodeHelper(tree: CodeTree, original: CodeTree, bits: List[Bit]): List[Char] = {
    bits match {
      case Nil => {
        tree match {
          case Leaf(c, _) => c :: Nil
          case _ => Nil
        }
      }
      case 0 :: xs => {
        tree match {
          case Leaf(c, _) => c :: decodeHelper(original, original, bits)
          case Fork(left, _, _, _) => decodeHelper(left, original, xs)
        }
      }

      case 1 :: xs => {
        tree match {
          case Leaf(c, _) => c :: decodeHelper(original, original, bits)
          case Fork(_, right, _, _) => decodeHelper(right, original, xs)
        }
      }

      case _ => throw new IllegalArgumentException("bits can be only 1 and 0")
    }
  }

  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
//    text match {
//      case Nil => Nil
//      case c :: cs => {
//        tree match {
//          case Leaf(_, _) => Nil
//          case Fork(l, r, ch, w) => {
//            if (chars(l).contains(c)) 0 :: encode(l)(c :: cs)
//            else 1 :: encode(r)(c :: cs)
//          }
//        }
//      }
//    }
    encodeHelper(tree, tree)(text)
  }

  def encodeHelper(tree: CodeTree, origin: CodeTree)(text: List[Char]): List[Bit] = {
    text match {
      case Nil => Nil
      case t :: ts => {
        tree match {
          case Leaf(_, _) => encodeHelper(origin, origin)(ts)
          case Fork(left, right, _, _) => {
            if(chars(left).contains(t)) {
              return 0 :: encodeHelper(left, origin)(text)
            } else return 1 :: encodeHelper(right, origin)(text)
          }
        }
      }
    }
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
    table.find(c => c._1 == char).get._2
  }

  def convert(tree: CodeTree): CodeTable = {
    tree match {
      case Leaf(ch, w) => (ch, Nil) :: Nil
      case Fork(l, r, w, c) => mergeCodeTables(
        convert(l).map(x => (x._1, 0 :: x._2)),
        convert(r).map(x => (x._1, 1 :: x._2))
      )
    }
  }

  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    a ++ b
  }

  /**
    * This function encodes `text` according to the code tree `tree`.
    *
    * To speed up the encoding process, it first converts the code tree to a code table
    * and then uses it to perform the actual encoding.
    */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val ct: CodeTable = convert(tree)
    val res = text.flatMap(t => codeBits(ct)(t))
    return res
  }
}
