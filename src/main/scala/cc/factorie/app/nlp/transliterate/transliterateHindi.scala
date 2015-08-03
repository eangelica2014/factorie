package cc.factorie.app.nlp.transliterate


/**
 * Created by Esther on 7/28/15.
 */
object transliterateHindi {

  val charMap = Map(
  //vowels
    'अ' -> 'a',
    'आ' -> 'A',
    'ा'-> 'A',
    'इ' -> 'i',
    'ि' -> 'i',
    'ई' -> 'I',
    'ी'-> 'I',
    'उ' -> 'u',
    '\u0941' -> 'u',
    'ऊ' -> 'U',
    '\u0942' -> 'U',
    'ए' -> 'e',
    '\u0947' -> 'e',
    'ऐ' -> 'E',
    '\u0948' -> 'E',
    'ओ' -> 'o',
    '\u094B' -> 'o',
    'औ' -> 'O',
    '\u094C' -> 'O',
    'ऋ' -> 'f',
    '\u0943' -> 'f',
    'ॠ' -> 'F',
    'ऌ' -> 'x',
    'ॡ' -> 'X',

  //consonants
    'क' -> "ka",
    'ख' -> "Ka",
    'ग' -> "ga",
    'घ' -> "Ga",
    'ङ' -> "Na",
    'च' -> "ca",
    'छ' -> "Ca",
    'ज' -> "ja",
    'झ' -> "Ja",
    'ञ' -> "Ya",
    'ट' -> "wa",
    'ठ' -> "Wa",
    'ड' -> "qa",
    'ढ' -> "Qa",
    'ण' -> "Ra",
    'त' -> "ta",
    'थ' -> "Ta",
    'द' -> "da",
    'ध' -> "Da",
    'न' -> "na",
    'प' -> "pa",
    'फ' -> "Pa",
    'ब' -> "ba",
    'भ' -> "Ba",
    'म' -> "ma",
    'य' -> "ya",
    'र' -> "ra",
    'ल' -> "la",
    'व' -> "va",
    'श' -> "Sa",
    'ष' -> "za",
    'स' -> "sa",
    'ह' -> "ha",
    '\u0901' -> 'n',
    '\u0902' -> 'n',

  //numbers

    '०' -> '0',
    '१' -> '1',
    '२' -> '2',
    '३' -> '3',
    '४' -> '4',
    '५' -> '5',
    '६' -> '6',
    '७' -> '7',
    '८' -> '8',
    '९' -> '9',

  //special
    '\u094D' -> '\0', //indicates half and combined characters
    '\u0964' -> '\0' //delete devanagri end of sentence marker

    )

  def handleMultiChar (s: String): String ={
    val multiChar = List(
    //vowels
      ("ऑ", "au"),
      ("ॉ", "au"),
      ("अं", "M"),
      ("अः", "H"),
      ("अँ", "Om"),


    //consonants
      ("क्ष","kz."),
      ("त्र","tr."),
      ("ज्ञ","jY."),
      ("श्र","Sr."),
      ("क़", "q."),
      ("ख़", "K."),
      ("ग़", "G."),
      ("ज़", "z."),
      ("फ़", "p."),
      ("ड़", "r."),
      ("ढ़", "R.")
    )
    var word = s
    multiChar.foreach{e =>
    val (orig, replace) = e
      word = word.replaceAll(orig,replace)
    }
    word
  }


  def apply(s: String) = handleMultiChar(s.concat("\0")).map(x => charMap.get(x).getOrElse(x)).mkString("").replaceAll("a([aAeEiIoOuU\0])","$1").stripSuffix("\0*")


}
