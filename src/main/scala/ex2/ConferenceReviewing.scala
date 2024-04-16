package ex2

import ex2.Question.{Final, Relevance}

import scala.collection.immutable.HashMap

enum Question:
  case Relevance()
  case Significance()
  case Confidence()
  case Final()

trait ConferenceReviewing:
  def loadReview(article: Int, scores: Map[Question, Int]): Unit
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles(): Set[Int]
  def sortedAcceptedArticles(): List[(Int, Double)]
  def averageWeightedFinalScoreMap(): Map[Int, Double]

object ConferenceReviewing:

  def apply(): ConferenceReviewing = ConferenceReviewingImpl(List[(Int, Map[Question, Int])]())

  private case class ConferenceReviewingImpl(private var articleScores: List[(Int, Map[Question, Int])]) extends ConferenceReviewing:

    override def loadReview(article: Int, scores: Map[Question, Int]): Unit = articleScores = articleScores.::((article, scores))

    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
      val questions = new HashMap[Question, Int]().fillMap(relevance: Int, significance: Int, confidence: Int, fin: Int)
      articleScores = articleScores.::((article, questions))

    override def orderedScores(article: Int, question: Question): List[Int] = articleScores.collect( { case (k, v) if k == article => v(question)} ).sortWith(_ < _)

    override def averageFinalScore(article: Int): Double =
      val finalScores = orderedScores(article, Final())
      finalScores.sum.toDouble/finalScores.length.toDouble

    override def acceptedArticles(): Set[Int] = articleScores.collect( { case (k, v) if averageFinalScore(k) >= 5 & v(Relevance()) >= 8 => k } ).toSet

    override def sortedAcceptedArticles(): List[(Int, Double)] = articleScores.collect({ case (k, v) if averageFinalScore(k) >= 5 & v(Relevance()) >= 8 => (k, averageFinalScore(k)) }).distinct.sortWith(_._2 < _._2)

    override def averageWeightedFinalScoreMap(): Map[Int, Double] = ???

    extension(map: HashMap[Question, Int])
      private def fillMap(relevance: Int, significance: Int, confidence: Int, fin: Int): HashMap[Question, Int] =
        map.+((Question.Relevance(), relevance)).+((Question.Significance(), significance)).+((Question.Confidence(), confidence)).+((Question.Relevance(), relevance)).+((Question.Final(), fin))
