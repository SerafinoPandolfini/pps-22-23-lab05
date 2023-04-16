package u05lab.ex2

import java.util.EnumMap
import scala.collection.immutable.*

case class Pair[A, B](x: A, y: B)

val averagePassingScore = 5.0
val relevancePassingScore = 8
val weightedPassingScore = 10.0

enum Question:
  case RELEVANCE
  case SIGNIFICANCE
  case CONFIDENCE
  case FINAL

trait ConferenceReviewing:

  def loadReview(article: Int, scores: Map[Question, Int]): Unit

  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit

  def orderedScores(article: Int, question: Question): List[Int]

  def averageFinalScore(article: Int): Double

  def accepted(article: Int): Boolean

  def acceptedArticles: Set[Int]

  def sortedAcceptedArticles: List[Pair[Int, Double]]

  def averageWeightedFinalScore(article: Int): Double

  def averageWeightedFinalScoreMap(): Map[Int, Double]

object ConferenceReviewing:
  def apply(): ConferenceReviewing = ConferenceReviewingImpl()
  private class ConferenceReviewingImpl() extends ConferenceReviewing:
    private var reviews: List[Pair[Int, Map[Question, Int]]] = List()

    override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
      if scores.size < Question.values.length then throw new IllegalArgumentException()
      reviews = reviews.appended(Pair(article, scores))

    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
      val map = Map(Question.RELEVANCE -> relevance,
        Question.SIGNIFICANCE -> significance,
        Question.CONFIDENCE -> confidence,
        Question.FINAL -> fin)
      reviews = reviews.appended(Pair(article, map))

    override def orderedScores(article: Int, question: Question): List[Int] =
      reviews.filter(review => review.x == article)
        .map(review => review.y(question))
        .sorted

    override def averageFinalScore(article: Int): Double =
      val finalScores = reviews.filter(review => review.x == article)
        .map(review => review.y(Question.FINAL))
      finalScores.sum.toDouble / finalScores.size

    override def accepted(article: Int): Boolean =
      averageFinalScore(article) >= averagePassingScore
        && reviews.filter(review => review.x == article)
        .map(review => review.y)
        .filter(review => review.keySet.contains(Question.RELEVANCE))
        .exists(review => review(Question.RELEVANCE) >= relevancePassingScore)


    override def acceptedArticles: Set[Int] =
      reviews.map(review => review.x)
        .distinct
        .filter(this.accepted)
        .toSet

    override def sortedAcceptedArticles: List[Pair[Int, Double]] =
      this.acceptedArticles.map(article =>
        Pair(article, this.averageFinalScore(article)))
        .toList
        .sorted((a1, a2) => a1.y.compareTo(a2.y))

    override def averageWeightedFinalScore(article: Int): Double =
      val filteredElements = reviews.filter(review => review.x == article)
        .map(review => review.y(Question.FINAL) * review.y(Question.CONFIDENCE) / weightedPassingScore)
      filteredElements.sum / filteredElements.length

    override def averageWeightedFinalScoreMap(): Map[Int, Double] =
      reviews.map(review => review.x)
        .distinct
        .map(review => review -> averageWeightedFinalScore(review))
        .toMap



@main
def test(): Unit =
  val cr = ConferenceReviewing()
  cr.loadReview(1, 9, 9, 6, 9); // 5.4
  cr.loadReview(2, 9, 9, 10, 9); // 9.0
  cr.loadReview(2, 4, 6, 10, 6); // 6.0
  cr.loadReview(3, 3, 3, 3, 3); // 0.9
  cr.loadReview(3, 4, 4, 4, 4); // 1.6
  cr.loadReview(4, 6, 6, 6, 6); // 3.6
  cr.loadReview(4, 7, 7, 8, 7); // 5.6
  println(cr.averageFinalScore(1))
  println(cr.averageFinalScore(2))