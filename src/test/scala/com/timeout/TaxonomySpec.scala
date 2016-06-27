package com.timeout

import java.nio.file.{Paths, Files}

import org.specs2.Specification

import scala.io.Source

/**
 * Created by Abim on 27/06/2016.
 */
class TaxonomySpec extends Specification {

  val theatre = Node(5, Tag(5, "theatre", Nil), Nil)
  val films = Node(6, Tag(6, "films", Nil), Nil)
  val shows = Node(2, Tag(2, "shows", Nil), theatre :: films :: Nil)
  val music = Node(3, Tag(3, "music", Translation(1, "en_GB", "music") :: Translation(2, "fr_FR", "musique") :: Nil), Nil)
  val restaurant = Node(4, Tag(4, "restaurant", Nil), Nil)
  val categories = Node(1, Tag(1, "categories", Nil), shows :: music :: restaurant :: Nil)
  val taxonomy = Taxonomy(categories)

  def is = s2"""

    A node should be searchable by id: 1 ${ searchById(id = 1, expected = Some(categories)) }
    A node should be searchable by id: 4 ${ searchById(id = 4, expected = Some(restaurant)) }

    A node's children should be searchable by id: 2 ${ searchForChilrenById(id = 2, expected = List(theatre, films)) }
    A node's children should be searchable by id: 5 ${ searchForChilrenById(id = 5, expected = Nil) }
    A node's children should be searchable by id: 99 ${ searchForChilrenById(id = 99, expected = Nil) }

    Nodes should be searchable by tag: music ${ searchByTag(tag = "music", expected = List(music))}
    Nodes should be searchable by tag: xxx ${ searchByTag(tag = "xxx", expected = Nil)}

    A taxonomy should be readable from CSV format ${ readTaxonomy() }
    A taxonomy should be persistable to CSV format ${ persistTaxonomy() }
  """

  def searchById(id: Int, expected: Option[Node]) = taxonomy.getNodeById(id) === expected

  def searchForChilrenById(id: Int, expected: List[Node]) = taxonomy.getChildrenById(id) === expected

  def searchByTag(tag: String, expected: List[Node]) = taxonomy.getNodesByTag(tag) === expected

  def readTaxonomy() = Taxonomy.read("src/test/resources/expectedTaxonomy.csv") === taxonomy

  def persistTaxonomy() = {

    Files.deleteIfExists(Paths.get("src/test/resources/taxonomy.csv"))

    val expected = Source.fromFile("src/test/resources/expectedTaxonomy.csv").mkString

    Taxonomy.persist(taxonomy, "src/test/resources/taxonomy.csv")

    val actual = Source.fromFile("src/test/resources/taxonomy.csv").mkString

    expected === actual
  }
}
