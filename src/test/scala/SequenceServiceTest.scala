import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfter, FlatSpec}

class SequenceServiceTest extends  FlatSpec with BeforeAndAfter with MockFactory {

  val seq = new SequenceService

  "isCharInSequence" should "return true when char 'A' in a string sequence 'BACBAD'" in {
    val char = "A"
    val stringSequence = "BACBAD"
    val result = seq.isCharInSequence(char, stringSequence)
    result shouldEqual true
  }
  it should "return false when char 'Q' in a string sequence 'BACBAD'" in {
    val char = "Q"
    val stringSequence = "BACBAD"
    val result = seq.isCharInSequence(char, stringSequence)
    result shouldEqual false
  }

  "indexOfCharInSequence" should "return an index of 1 when char 'A' in a string sequence 'BACBAD'" in {
    val char = "A"
    val stringSequence = "BACBAD"
    val result = seq.indexOfCharInSequence(char, stringSequence)
    result shouldEqual 1
  }
  it should "return an index of -1 when char 'A' in a string sequence 'BACBAD'" in {
    val char = "Q"
    val stringSequence = "BACBAD"
    val result = seq.indexOfCharInSequence(char, stringSequence)
    result shouldEqual -1
  }

  "commonSequence" should "return a populated Sequence for s1 = 'AB', s2 = 'BACBAD'" in {
    val char = "AB"
    val stringSequence = "BACBAD"
    val expected = new SequenceMap(Seq("A"), "B", "CBAD")
    val result = seq.commonSequence(char, stringSequence)
    result shouldEqual expected
  }
  it should "return a empty Sequence for s1 = 'QAB', s2 = 'BACBAD'" in {
    val char = "QAB"
    val stringSequence = "BACBAD"
    val expected = new SequenceMap(Seq(), "AB", "BACBAD")
    val result = seq.commonSequence(char, stringSequence)
    result shouldEqual expected
  }

}
