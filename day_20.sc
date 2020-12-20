import scala.annotation.tailrec
import scala.io.Source

def readInputFile(filename: String): List[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret
}

type Img = List[String]

def isolateTiles(puzzleInput: List[String]): Map[Int, List[String]] = {
  @tailrec
  def isolateTilesLoop(inputLeft: List[String], curId: Int,
                       curTile: List[String], tileMap: Map[Int, List[String]]): Map[Int, List[String]] = {
    inputLeft match {
      case hd::tl => {
        if (hd.startsWith("Tile"))
          {
            val id = hd.split(" ")(1).dropRight(1).toInt
            isolateTilesLoop(tl, id, curTile, tileMap)
          }
        else
          if (hd == "") isolateTilesLoop(tl, curId, List(), tileMap + (curId -> curTile))
          else isolateTilesLoop(tl, curId, curTile :+ hd, tileMap)
      }
      case _ => tileMap
    }
    }
  isolateTilesLoop(puzzleInput, 0, List(), Map())
}

def isolateEdges(id: Int, image: List[String]): List[(Int, String, Int)] = {
  val imageT = image.transpose.map(_.mkString)
  List((id, imageT.head.reverse, 3), (id, image.head, 0),
    (id, imageT.takeRight(1).head, 1), (id, image.takeRight(1).head.reverse, 2))
}

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_20.txt"

val mapTiles = isolateTiles(readInputFile(filepath))

val edgeList = mapTiles.flatMap{
  case (k, v) => isolateEdges(k, v)
}

val neighbors = (for ((id, str, _) <- edgeList)
  yield (id, edgeList.
    filter(x => (x._2 == str | x._2 == str.reverse) && x._1 != id).
    map(_._1))).groupBy(_._1).map{
  case (k, v) => (k, v.flatMap(_._2).toList)
}

def rotateImage(img: List[String], nRot: Int, flip: Boolean): List[String] = {
  @tailrec
  def rotateRightLoop(img: List[String], nRot: Int): List[String] = {
    if (nRot == 0) img
    else rotateRightLoop(img.reverse.transpose.map(_.mkString), nRot - 1)
  }
  val rotated = rotateRightLoop(img, nRot)
  if (flip) rotated.reverse else rotated
}

def takeEdge(img: List[String], edgeNum: Int): String = {
  if (edgeNum == 0) img.head
  else if (edgeNum == 1) img.transpose.takeRight(1).head.mkString
  else if (edgeNum == 2) img.takeRight(1).head
  else img.transpose.head.mkString
}

def rotateToMatch(img: Img, edge: Int, str: String): Img = {
  @tailrec
  def rotateToMatchLoop(imgLeft: List[Img], edge: Int, str: String): Img = {
    imgLeft match {
      case hd::tl => {
        if (takeEdge(hd, edge) == str) hd
        else rotateToMatchLoop(tl, edge, str)
      }
      case _ => List("")
    }
  }
  val rotations = List(0, 1, 2, 3)
  val flips = List(true, false)
  val possibilities = for (rot <- rotations; f <- flips) yield rotateImage(img, rot, f)
  rotateToMatchLoop(possibilities, edge, str)
}

def buildFullImage(neigh: Map[Int, List[Int]]): List[List[(Int, Img)]] = {
  val edges = neigh.filter(_._2.size == 2)
  def buildUpperRow(neigh: Map[Int, List[Int]], uppLeft: (Int, Img)): List[(Int, Img)] = {
    @tailrec
    def buildUpperRowLoop(curNode: (Int, Img), curEdges: List[(Int, Img)], seen: Set[Int]): List[(Int, Img)] = {
      val (id, text) = curNode
      val edgeMatch = takeEdge(text, 1)
      val nextId = neigh(id).find(x => neigh(x).size < 4 && !seen.contains(x) && rotateToMatch(mapTiles(x),
        3, edgeMatch).size > 2).getOrElse(-1)
      if (nextId == -1) List((-1, List("")))
      else {
      val nextImg = rotateToMatch(mapTiles(nextId), 3, edgeMatch)
      if (edges.contains(nextId)) curEdges :+ (nextId, nextImg)
      else buildUpperRowLoop((nextId, nextImg), curEdges :+ (nextId, nextImg), seen + nextId)
      }
    }
    val (upperLeftId, upperLeftText) = uppLeft
    buildUpperRowLoop((upperLeftId, upperLeftText), List((upperLeftId, upperLeftText)), Set(upperLeftId))
  }

  @tailrec
  def buildNextRow(prevRowLeft: List[(Int, Img)], newRow: List[(Int, Img)], seen: Set[Int]): List[(Int, Img)] = {
    prevRowLeft match {
      case hd::tl => {
        val (hdId, hdImg) = hd
        val edgeText = takeEdge(hdImg, 2)
          val elId = neigh(hdId).filter(x => !seen.contains(x)).head
          val elImg = rotateToMatch(mapTiles(elId), 0, edgeText)
          buildNextRow(tl, newRow :+ (elId, elImg), seen + elId)
        }
      case _ => newRow
    }
  }
  @tailrec
  def buildFullImageLoop(upperRow: List[(Int, Img)], fullImage: List[List[(Int, Img)]],
                         seen: Set[Int]): List[List[(Int, Img)]] = {
    if (seen.size == neigh.size || upperRow.map(_._1).contains(-1)) fullImage
    else {
      val nextRow = buildNextRow(upperRow, List(), seen)
      buildFullImageLoop(nextRow, fullImage :+ nextRow, seen ++ nextRow.map(_._1).toSet)
    }
  }
  val rotations = List(0, 1, 2, 3)
  val flips = List(true, false)
  val upperLeftId = edges.keys.head
  val upperLeftTexts = for (rot <- rotations; flip <- flips) yield rotateImage(mapTiles(upperLeftId), rot, flip)
  val firstRows = for (upperLeftText <- upperLeftTexts) yield buildUpperRow(neigh, (upperLeftId, upperLeftText))
  val rets = for (firstRow <- firstRows if !firstRow.map(_._1).contains(-1)) yield buildFullImageLoop(firstRow, List(firstRow), firstRow.map(_._1).toSet)
  rets.filter(image => image.takeRight(1).head.count(x => x._2.head.isEmpty) == 0).head
}

val image = buildFullImage(neighbors)

def trimEdges(img: List[String]): List[String] = {
  img.drop(1).dropRight(1).transpose.drop(1).dropRight(1).transpose.map(_.mkString)
}

def renderFullImage(img: List[List[(Int, Img)]]): Array[String] = {
  def hConcatenate(imgs: List[List[String]]): List[String] = {
    @tailrec
    def hConcatenateLoop(finalOutput: List[String], imgsLeft: List[List[String]]): List[String] = {
      if (finalOutput.size == imgs.head.size) finalOutput
      else {
        hConcatenateLoop(finalOutput :+ imgsLeft.map(x => x.head).reduce(_ + _),
          imgsLeft.map(x => x.tail))
      }
    }
    hConcatenateLoop(List(), imgs)
  }
  val decodedImage = img.map(x => x.map(ix => trimEdges(ix._2)))
  decodedImage.map(hConcatenate).reduce(_ ++ _).toArray
}

val stringImage = image.map(ls => ls.map(_._2))

val bitImage = renderFullImage(image).map(x => x.map(ch => if (ch == '#') 1 else 0).toArray)

val seaMonster = Array(
  "                  # ",
  "#    ##    ##    ###",
  " #  #  #  #  #  #    "
).map(x => x.map(ch => if (ch == '#') 1 else 0).toArray)


def findSeaMonster(image: Array[Array[Int]], monster: Array[Array[Int]]): Int = {
  val (mWidth, mHeight) = (monster(0).length, monster.length)
  val (width, height) = (image(0).length, image.length)

  def matchMonster(image: Array[Array[Int]], monster: Array[Array[Int]], offset: (Int, Int)): Boolean = {
    val (offX, offY) = offset
    if (offX+mWidth > width || offY+mHeight > height) false
    else {
      val imageSlice = image.slice(offY, offY+mHeight).map(x => x.slice(offX, offX+mWidth))
      val (imageFlat, monsterFlat) = (imageSlice.flatten, monster.flatten)
      imageFlat.zip(monsterFlat).map(x => x._1 * x._2).sum == monsterFlat.sum
    }
  }

  (for (x <- 0 until width; y <- 0 until height)
    yield matchMonster(image, monster, (x, y))).count(x => x) * seaMonster.flatten.sum
}

val imagePossibilities = List(
  bitImage,
  bitImage.reverse.transpose,
  bitImage.reverse.transpose.reverse.transpose,
  bitImage.reverse.transpose.reverse.transpose.reverse.transpose,
  bitImage.reverse,
  bitImage.reverse.transpose.reverse,
  bitImage.reverse.transpose.reverse.transpose.reverse,
  bitImage.reverse.transpose.reverse.transpose.reverse.transpose.reverse
)


val totalSum = bitImage.map(x => x.sum).sum
val occupied = (for (im <- imagePossibilities) yield findSeaMonster(im, seaMonster)).filter(x => x > 0).head

neighbors.filter(_._2.size == 2).keys.map(_.toLong).product
totalSum - occupied

