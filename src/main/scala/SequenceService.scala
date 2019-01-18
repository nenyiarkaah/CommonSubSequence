class SequenceService {


  //next extend functionality to handle two characters in s1 string
  def commonStringSequence(sequenceMap: SequenceMap): String = {
      sequenceMap.s1.isEmpty match {
      case true => sequenceMap.seq.mkString("")
      case false => val seqMap = commonCharSequence(sequenceMap.s1, sequenceMap.s2, sequenceMap.seq)
        commonStringSequence(seqMap)
    }
  }


  def commonCharSequence(s1: String, s2: String, seq: Seq[String] = Seq()) = {
    val head = s1.head.toString
    val tail = s1.tail

    isCharInSequence(head, s2) match {
      case false => val index = 0
        new SequenceMap(seq, tail, s2.substring(index))
      case true => val index = indexOfCharInSequence(head, s2) + 1
        val newSeq: Seq[String] = seq :+ head
        new SequenceMap(newSeq, tail, s2.substring(index))
    }
  }


  def isCharInSequence(char: String, s2: String) = {
    s2.contains(char)
  }

  def indexOfCharInSequence(char: String, s2: String) = {
    s2.indexOfSlice(char)
  }

}
