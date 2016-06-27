package com.timeout

import java.nio.file.{Files, Paths}

import scala.annotation.tailrec
import scala.io.{Source, StdIn}
import scala.language.reflectiveCalls

/**
 * Created by Abim on 27/06/2016.
 */
object Boot extends App {

  val taxonomy = Taxonomy.read("src/main/resources/taxonomy.csv")

  val instruction =
    """
      | Please select an option:
      |
      | 1. Retrieve a node by id
      | 2. Retrieve all the children of a node
      | 3. Retrieve all the nodes with a particular tag
      |
    """.stripMargin

  val options = Map("1" -> searchById _, "2" -> searchForChildrenById _, "3" -> searchForNodesByTag _)

  Console println options(StdIn readLine instruction)()

  def searchById() = {
    val queryId = (StdIn readLine "Please enter the node Id to find: \n").toInt
    taxonomy getNodeById queryId
  }

  def searchForChildrenById = {
    val queryId = (StdIn readLine "Please enter the node Id to find children of: \n").toInt
    taxonomy getChildrenById queryId
  }

  def searchForNodesByTag = {
    val queryTag = StdIn readLine "Please enter the tag to find: \n"
    taxonomy getNodesByTag queryTag
  }
}

case class Taxonomy(root: Node) {

  @tailrec
  final def getNodeById(id: Int, unsearchedNodes: List[Node] = root :: Nil): Option[Node] = {

    val current = unsearchedNodes.headOption

    if (unsearchedNodes.isEmpty) None
    else if (current exists (_.id == id)) current
    else getNodeById(id, unsearchedNodes.tail ++ (current map (_.children) getOrElse Nil))
  }

  def getChildrenById(id: Int) = {

    @tailrec
    def findChildren(unsearchedNodes: List[Node], children: List[Node] = Nil): List[Node] = {

      if (unsearchedNodes.isEmpty) children
      else {

        val currentChildren = unsearchedNodes.head.children
        findChildren(unsearchedNodes.tail ++ currentChildren, children ++ currentChildren)
      }
    }

    val parent = getNodeById(id)

    if (parent.isDefined) findChildren(parent.get :: Nil)
    else Nil
  }

  def getNodesByTag(queryTag: String) = getChildrenById(root.id) filter (_.tag.value == queryTag)
}

object Taxonomy {

  def persist(taxonomy: Taxonomy, filepath: String): Unit = {

    def extractIds[T <: {def id : Int}](elems: List[T]) = elems.foldLeft(",")((acc, cur) => acc + ";" + cur.id)

    val nodes = taxonomy.root :: (taxonomy getChildrenById taxonomy.root.id)
    val tags = nodes map (_.tag)
    val translations = tags flatMap (_.translations)

    val nodesAsString = nodes.map(n => s"Node,${n.id},${n.tag.id}${extractIds(n.children)}")
    val tagsAsString = tags.map(t => s"Tag,${t.id},${t.value}${extractIds(t.translations)}")
    val translationsAsString = translations.map(t => s"Translation,${t.id},${t.lang},${t.value}")

    val taxonomyAsString = (nodesAsString ++ tagsAsString ++ translationsAsString).reverse.fold("")((acc, cur) => acc + cur + "\n")

    Files.write(Paths.get(filepath), taxonomyAsString.getBytes)
  }

  def read(filepath: String): Taxonomy = {

    def createTranslation(fields: Array[String]) = Translation(fields(0).toInt, fields(1), fields(2))

    def createTag(fields: Array[String], translations: Array[Translation]) = {

      val tagTranslations = if (fields.length == 2) Nil
      else {

        val translationIds = fields(2).split(";").tail.map(_.toInt)
        translations.filter(t => translationIds.contains(t.id)).toList
      }

      Tag(fields(0).toInt, fields(1), tagTranslations)
    }

    @tailrec
    def createNodes(lines: Array[String], tags: Array[Tag], nodes: List[Node]): List[Node] = {

      def createNode(fields: Array[String], tags: Array[Tag], nodes: List[Node]) = {

        val children = if (fields.length == 2) Nil
        else {

          val nodeIds = fields(2).split(";").tail.map(_.toInt)
          nodes.filter(n => nodeIds.contains(n.id))
        }
        Node(fields(0).toInt, tags.find(_.id == fields(1).toInt).get, children)
      }

      if (lines.isEmpty) nodes
      else {
        val fields = lines.head.split(",").tail
        createNodes(lines.tail, tags, createNode(fields, tags, nodes) :: nodes)
      }
    }

    val taxonomyAsString = Source.fromFile(filepath).mkString
    val taxonomyLines = taxonomyAsString.split("\n")

    val translationLines = taxonomyLines filter (_.startsWith("Translation"))
    val translations = translationLines map (l => createTranslation(l.split(",").tail))

    val tagLines = taxonomyLines filter (_.startsWith("Tag"))
    val tags = tagLines map (l => createTag(l.split(",").tail, translations.reverse))

    val nodeLines = taxonomyLines filter (_.startsWith("Node"))
    val nodes = createNodes(nodeLines, tags, Nil)

    Taxonomy(nodes.head)
  }
}

case class Node(id: Int, tag: Tag, children: List[Node]) {
  override def toString: String = s"Node($id, ${tag.value})"
}

case class Tag(id: Int, value: String, translations: List[Translation])

case class Translation(id: Int, lang: String, value: String)