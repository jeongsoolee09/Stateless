package kr.ac.korea.prl.stateless.Utils

object Utils {
  def catMaybes[A](optionList: List[Option[A]]): List[A] = for {
    some <- optionList.filter(opt => !opt.isEmpty)
    value <- some
  } yield value
}
