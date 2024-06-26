package ex2

import ex2.Question.{Confidence, Final, Relevance}

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

  private case class ConferenceReviewingImpl(private var articlesScores: List[(Int, Map[Question, Int])]) extends ConferenceReviewing:

    override def loadReview(article: Int, scores: Map[Question, Int]): Unit = articlesScores = articlesScores.::((article, scores))

    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit = articlesScores = articlesScores.::((article, HashMap[Question, Int]().fillMap(relevance: Int, significance: Int, confidence: Int, fin: Int)))

    override def orderedScores(article: Int, question: Question): List[Int] = scoresOfQuestion(article, question).sortWith(_ < _)

    override def averageFinalScore(article: Int): Double = scoresOfQuestion(article, Final()).sum.toDouble/scoresOfQuestion(article, Final()).length.toDouble

    override def acceptedArticles(): Set[Int] = articlesScores.collect( { case (a, m) if averageFinalScore(a) >= 5 & m(Relevance()) >= 8 => a } ).toSet

    override def sortedAcceptedArticles(): List[(Int, Double)] = articlesScores.collect({ case (a, m) if averageFinalScore(a) >= 5 & m(Relevance()) >= 8 => (a, averageFinalScore(a)) }).distinct.sortWith(_._2 < _._2)

    override def averageWeightedFinalScoreMap(): Map[Int, Double] = articlesScores.collect( { case (a, _) => (a, scoresOfQuestion(a, Confidence()).zip(scoresOfQuestion(a, Final())).map((x, y) => (x.toDouble * y.toDouble) / 10).sum/articlesScores.count(_._1 == a)) } ).toMap

    private def scoresOfQuestion(article: Int, question: Question): List[Int] = articlesScores.collect( { case (`article`, v) => v(question)} )

    extension(map: HashMap[Question, Int])
      private def fillMap(relevance: Int, significance: Int, confidence: Int, fin: Int): HashMap[Question, Int] =
        map.+((Question.Relevance(), relevance)).+((Question.Significance(), significance)).+((Question.Confidence(), confidence)).+((Question.Final(), fin))
