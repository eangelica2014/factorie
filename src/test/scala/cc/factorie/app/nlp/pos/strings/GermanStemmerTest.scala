package cc.factorie.app.nlp.pos.strings

import org.junit.Test

/**
 * Created by Esther on 7/21/15.
 */
class GermanStemmerTest {
  val list = List(
    ("ager","ager"),
    ("arme", "arm"),
    ("armee","arme"),
    ("angst","angst"),
    ("amtsgeheimnisse", "amtsgeheimnis"),
    ("ansteigende", "ansteig"),
    ("artigkeit","artig"),
    ("aufeinander", "aufeinand"),
    ("aufeinanderbiss", "aufeinanderbiss"),
    ("aufeinanderfolge", "aufeinanderfolg"),
    ("aufeinanderfolgen", "aufeinanderfolg"),
    ("aufeinanderfolgend", "aufeinanderfolg"),
    ("aufeinanderfolgenden", "aufeinanderfolg"),
    ("aufeinanderschlügen", "aufeinanderschlug"),
    ("aufenthalt", "aufenthalt"),
    ("aufenthalten", "aufenthalt"),
    ("aufenthaltes", "aufenthalt"),
    ("auferlegen", "auferleg"),
    ("auferstehen", "aufersteh"),
    ("aufersteht", "aufersteht"),
    ("auferstehung", "aufersteh"),
    ("auferstünde", "auferstund"),
    ("auffallender", "auffall"),
    ("auffälligen", "auffall"),
    ("auffälliges", "auffall"),
    ("auffasst", "auffasst"),
    ("auffaßt", "auffasst"),
    ("aufrichtigkeit","aufricht"),
    ("befriedigung", "befried"),
    ("bosheit", "bosheit"),
    ("der","der"),
    ("dunkelheit","dunkel"),
    ("fadenscheinigen", "fadenschein"),
    ("idee","ide"),
    ("ideen", "ide"),
    ("kategorische", "kategor"),
    ("kategorischer", "kategor"),
    ("kauen", "kau"),
    ("kriegsdienstverweigerer", "kriegsdienstverweig"),
    ("kriegsdienstverweigerung", "kriegsdienstverweiger"),
    ("kriegsdienst", "kriegsdien"),
    ("kriegslist", "kriegslist"),
    ("stammtischfeierlichkeit","stammtischfeier"),
    ("schwerlich","schwerlich"),
    ("tapferkeit", "tapfer"),
    ("zweckdienliche","zweckdi")
  )

  @Test def testStemmer: Unit ={
    list.foreach { e =>
      val (word, stem) = e
      assert(cc.factorie.app.strings.germanPorterStem(word) == stem)
      //println(word + "\t" + stem)
    }
  }

  testStemmer
}
