package util

/**
  * Created by sam_coope on 24/10/2016.
  */
object CodeFormat {

  def formatCCode(code: String): String = {

    val lines = code.split("\n").toList

    def addTabs(tabchar: String, code: List[String], numberOfTabs: Int): String = {
      if (code.isEmpty) return ""

      val line = code.head
      val numberLeftBraces = line.count(_ == '{')
      val numberRightBraces = line.count(_ == '}')

      val changeInBraces = numberLeftBraces - numberRightBraces

      val tabsToAdd = if(changeInBraces > 0) numberOfTabs else numberOfTabs + changeInBraces
      val formattedLine = (tabchar * tabsToAdd) + line

      return formattedLine + "\n" + addTabs(tabchar, code.tail, numberOfTabs + changeInBraces)
    }

    return addTabs("    ", lines, 0)

  }

}
