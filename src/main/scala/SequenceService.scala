class SequenceService {

  def commonSequence(s1: String, s2: String) = {
    val head = s1.head.toString
    isCharInSequence(head, s2) match {
      case false => val index = 0
        new SequenceMap(Seq(), s1.tail, s2.substring(index))
      case true => val index = indexOfCharInSequence(head, s2) + 1
        new SequenceMap(Seq(head), s1.tail, s2.substring(index))
    }

  }


  def isCharInSequence(char: String, s2: String) = {
    s2.contains(char)
  }

  def indexOfCharInSequence(char: String, s2: String) = {
    s2.indexOfSlice(char)
  }

}
