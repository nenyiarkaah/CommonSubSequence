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
  it should "return an index of -1 when char 'Q' in a string sequence 'BACBAD'" in {
    val char = "Q"
    val stringSequence = "BACBAD"
    val result = seq.indexOfCharInSequence(char, stringSequence)
    result shouldEqual -1
  }

  "commonCharSequence" should "return a populated Sequence for s1 = 'AB', s2 = 'BACBAD'" in {
    val char = "AB"
    val stringSequence = "BACBAD"
    val expected = new SequenceMap(Seq("A"), "B", "CBAD")
    val result = seq.commonCharSequence(char, stringSequence)
    result shouldEqual expected
  }
  it should "return a empty Sequence for s1 = 'QAB', s2 = 'BACBAD'" in {
    val char = "QAB"
    val stringSequence = "BACBAD"
    val expected = new SequenceMap(Seq(), "AB", "BACBAD")
    val result = seq.commonCharSequence(char, stringSequence)
    result shouldEqual expected
  }

  "commonStringSequence" should "return '' for s1 = '', s2 = 'BACBAD'" in {
    val char = ""
    val stringSequence = "BACBAD"
    val expected = ""
    val result = seq.commonStringSequence(new SequenceMap(s1 = char, s2 = stringSequence))
    result.isEmpty shouldEqual true
    result shouldEqual expected
  }
  it should "return a 'AB' for s1 = 'QAB', s2 = 'BACBAD'" in {
    val s1 = "QAB"
    val s2 = "BACBAD"
    val expected = "AB"
    val result = seq.commonStringSequence(new SequenceMap(s1 = s1, s2 = s2))
    result shouldBe expected
  }
  it should "return a 'ABAD' for s1 = 'ABAZDC', s2 = 'BACBAD'" in {
    val s1 = "ABAZDC"
    val s2 = "BACBAD"
    val expected = "ABAD"
    val result = seq.commonStringSequence(new SequenceMap(s1 = s1, s2 = s2))
    result shouldBe expected
  }
  it should "return a 'AB' for s1 = 'AGGTAB', s2 = 'GXTXAYB'" in {
    val s1 = "AGGTAB"
    val s2 = "GXTXAYB"
    val expected = "AB"
    val result = seq.commonStringSequence(new SequenceMap(s1 = s1, s2 = s2))
    result shouldBe expected
  }
  it should "return a 'GTAB' for s1 = 'GGTAB', s2 = 'GXTXAYB'" in {
    val s1 = "GGTAB"
    val s2 = "GXTXAYB"
    val expected = "GTAB"
    val result = seq.commonStringSequence(new SequenceMap(s1 = s1, s2 = s2))
    result shouldBe expected
  }
  it should "return a 'GTAB' for s1 = 'GTAB', s2 = 'GXTXAYB'" in {
    val s1 = "GTAB"
    val s2 = "GXTXAYB"
    val expected = "GTAB"
    val result = seq.commonStringSequence(new SequenceMap(s1 = s1, s2 = s2))
    result shouldBe expected
  }
  it should "return a 'TAB' for s1 = 'TAB', s2 = 'GXTXAYB'" in {
    val s1 = "TAB"
    val s2 = "GXTXAYB"
    val expected = "TAB"
    val result = seq.commonStringSequence(new SequenceMap(s1 = s1, s2 = s2))
    result shouldBe expected
  }
  it should "return a 'AB' for s1 = 'AB', s2 = 'GXTXAYB'" in {
    val s1 = "AB"
    val s2 = "GXTXAYB"
    val expected = "AB"
    val result = seq.commonStringSequence(new SequenceMap(s1 = s1, s2 = s2))
    result shouldBe expected
  }
  it should "return a 'B' for s1 = 'B', s2 = 'GXTXAYB'" in {
    val s1 = "B"
    val s2 = "GXTXAYB"
    val expected = "B"
    val result = seq.commonStringSequence(new SequenceMap(s1 = s1, s2 = s2))
    result shouldBe expected
  }

  "commonStringSequenceForAllSubsets" should "return a 'ABAD' for s1 = 'ABAZDC', s2 = 'BACBAD'" in {
    val s1 = "ABAZDC"
    val s2 = "BACBAD"
    val expected = "ABAD"
    val result = seq.commonStringSequence(new SequenceMap(s1 = s1, s2 = s2))
    result shouldBe expected
  }
  it should "return a 'GTAB' for s1 = 'AGGTAB', s2 = 'GXTXAYB'" in {
    val s1 = "AGGTAB"
    val s2 = "GXTXAYB"
    val expected = "GTAB"
    val result = seq.commonStringSequenceForAllSubsets(new SequenceMap(s1 = s1, s2 = s2))
    result shouldBe expected
  }
}
