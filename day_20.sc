import scala.annotation.tailrec
import scala.io.Source

type Img = List[String]
type BitImage = List[List[Int]]

def readInputFile(filename: String): List[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret
}

def isolateTiles(puzzleInput: List[String]): Map[Int, Img] = {
  @tailrec
  def isolateTilesLoop(inputLeft: List[String], curId: Int,
                       curTile: List[String], tileMap: Map[Int, Img]): Map[Int, Img] = {
    inputLeft match {
      case hd::tl =>
        if (hd.startsWith("Tile"))
          {
            val id = hd.split(" ")(1).dropRight(1).toInt
            isolateTilesLoop(tl, id, curTile, tileMap)
          }
        else
          if (hd == "") isolateTilesLoop(tl, curId, List(), tileMap + (curId -> curTile))
          else isolateTilesLoop(tl, curId, curTile :+ hd, tileMap)
      case _ => tileMap + (curId -> curTile)
    }
    }
  isolateTilesLoop(puzzleInput, 0, List(), Map())
}

def isolateEdges(id: Int, image: Img): List[(Int, String)] = {
  val imageT = image.transpose.map(_.mkString)
  List((id, imageT.head.reverse), (id, image.head),
    (id, imageT.takeRight(1).head), (id, image.takeRight(1).head.reverse))
}

def rotateImage(img: Img, nRot: Int, flip: Boolean): Img = {
  @tailrec
  def rotateRightLoop(img: Img, nRot: Int): Img = {
    if (nRot == 0) img
    else rotateRightLoop(img.reverse.transpose.map(_.mkString), nRot - 1)
  }
  val rotated = rotateRightLoop(img, nRot)
  if (flip) rotated.reverse else rotated
}

def takeEdge(img: Img, edgeNum: Int): String = {
  if (edgeNum == 0) img.head
  else if (edgeNum == 1) img.transpose.takeRight(1).head.mkString
  else if (edgeNum == 2) img.takeRight(1).head
  else img.transpose.head.mkString
}

def generateRotations(img: Img): List[Img] =
  for (rot <- List(0, 1, 2, 3); f <- List(true, false)) yield rotateImage(img, rot, f)

def rotateToMatch(img: Img, edge: Int, str: String): Img = {
  @tailrec
  def rotateToMatchLoop(imgsLeft: List[Img], edge: Int, str: String): Img = {
    imgsLeft match {
      case hd::tl => {
        if (takeEdge(hd, edge) == str) hd
        else rotateToMatchLoop(tl, edge, str)
      }
      case _ => List("")
    }
  }
  val possibilities = generateRotations(img)
  rotateToMatchLoop(possibilities, edge, str)
}

def trimEdges(img: List[String]): List[String] = {
  img.drop(1).dropRight(1).transpose.drop(1).dropRight(1).transpose.map(_.mkString)
}

def renderFullImage(img: List[List[(Int, Img)]]): List[String] = {
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
  decodedImage.map(hConcatenate).reduce(_ ++ _)
}

def buildFullImage(neigh: Map[Int, List[Int]], tilesMap: Map[Int, Img]): Img = {
  val edges = neigh.filter {case (_, v) => v.size == 2}
  def buildUpperRow(neigh: Map[Int, List[Int]], uppLeft: (Int, Img)): List[(Int, Img)] = {
    @tailrec
    def buildUpperRowLoop(curNode: (Int, Img), curEdges: List[(Int, Img)], seen: Set[Int]): List[(Int, Img)] = {
      val (id, text) = curNode
      val edgeMatch = takeEdge(text, 1)
      val nextId = neigh(id).find(x => neigh(x).size < 4 && !seen.contains(x) && rotateToMatch(tilesMap(x),
        3, edgeMatch).size > 2).getOrElse(-1)
      if (nextId == -1) List((-1, List("")))
      else {
        val nextImg = rotateToMatch(tilesMap(nextId), 3, edgeMatch)
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
        val elImg = rotateToMatch(tilesMap(elId), 0, edgeText)
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
  val upperLeftId = edges.keys.head
  val upperLeftTexts = generateRotations(tilesMap(upperLeftId))
  val firstRows = for (upperLeftText <- upperLeftTexts) yield buildUpperRow(neigh, (upperLeftId, upperLeftText))
  val rets = for (firstRow <- firstRows) yield buildFullImageLoop(firstRow, List(firstRow), firstRow.map(_._1).toSet)
  renderFullImage(rets.filter(image => image.takeRight(1).head.count(x => x._2.head.isEmpty) == 0).head)
}

def matchPattern(image: BitImage, pattern: BitImage): Int = {
  val (pWidth, pHeight) = (pattern.head.length, pattern.length)
  val (width, height) = (image.head.length, image.length)

  def matchPatternPos(image: BitImage, patt: BitImage, offset: (Int, Int)): Boolean = {
    val (offX, offY) = offset
    if (offX + pWidth > width || offY + pHeight > height) false
    else {
      val imageSlice = image.slice(offY, offY + pHeight).map(x => x.slice(offX, offX + pWidth))
      val (imageFlat, pattFlat) = (imageSlice.flatten, patt.flatten)
      imageFlat.zip(pattFlat).map(x => x._1 * x._2).sum == pattFlat.sum
    }
  }

  (for (x <- 0 until width; y <- 0 until height)
    yield matchPatternPos(image, pattern, (x, y))).count(x => x)
}

def convertToBitImage(arr: Img): BitImage =
  arr.map(x => x.map(ch => if (ch == '#') 1 else 0).toList)

def solvePart1(neighbors: Map[Int, List[Int]]): Long = {
  neighbors.filter(_._2.size == 2).keys.map(_.toLong).product
}

def solvePart2(img: Img): Int = {
  val bitImages = generateRotations(img).map(convertToBitImage)
  val seaMonster = convertToBitImage(
    List("                  # ",
          "#    ##    ##    ###",
          " #  #  #  #  #  #    "))

  val occupied = (for (im <- bitImages)
    yield matchPattern(im, seaMonster)).filter(x => x > 0).head * seaMonster.flatten.sum

  bitImages.head.flatten.sum - occupied
}

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_20.txt"

val mapTiles = isolateTiles(readInputFile(filepath))

val edgeList = mapTiles.toList.flatMap{
  case (k, v) => isolateEdges(k, v)
}

val neighbors = (
  for ((id, str) <- edgeList)
    yield (id, edgeList.filter(x => (x._2 == str | x._2 == str.reverse) && x._1 != id))).
  groupBy(_._1).map {case (k, v) => (k, v.flatMap(_._2).map(_._1))}

val image = buildFullImage(neighbors, mapTiles)

solvePart1(neighbors)
solvePart2(image)