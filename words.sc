import scala.io.Source
import scala.language.postfixOps

val in = Source.fromURL("https://raw.githubusercontent.com/dwyl/english-words/master/words_alpha.txt")

def wordCombos(source: scala.io.BufferedSource, letters:String) = {
  val words = in.getLines.toStream filter (word => word forall (chr => chr.isLetter))
  val shortWords = words filter (word => word.length() <= letters.size && word.length >= 3)

  type Occurrences = List[(Char, Int)]
  type Word = String

  def wordOccurrences(w: Word): Occurrences = {
        val unsorted = (w.toLowerCase groupBy identity) map { case (c,cs) => (c, cs.length) }
    
        (SortedMap[Char,Int]() ++ unsorted) toList
      }

  def combinations(occurrences: Occurrences): List[Occurrences] = 
        (occurrences foldRight List[Occurrences](Nil)) { case ((ch,tm), acc) => {
          acc ++ ( for { comb <- acc; n <- 1 to tm } yield (ch, n) :: comb )
        } 
      }

  val wordFingerprints = shortWords.groupBy(wordOccurrences(_))

  val combs = combinations(wordOccurrences(letters)).filter(l => l.map(_._2).sum >= 3)  //letters of interest

  val validWords = for {vc <- combs if wordFingerprints.contains(vc)} yield wordFingerprints.get(vc)

  validWords.flatten.flatten.map( k => (k.length, k)).groupBy(_._1).mapValues(_.map(_._2))
}

// val words = in.getLines.toStream filter (word => word forall (chr => chr.isLetter))
// val shortWords = words filter (word => word.length() <= 6 && word.length >= 3)

// type Occurrences = List[(Char, Int)]
// type Word = String

// def wordOccurrences(w: Word): Occurrences = {
//       val unsorted = (w.toLowerCase groupBy identity) map { case (c,cs) => (c, cs.length) }
  
//       (SortedMap[Char,Int]() ++ unsorted) toList
//     }

// def combinations(occurrences: Occurrences): List[Occurrences] = 
//       (occurrences foldRight List[Occurrences](Nil)) { case ((ch,tm), acc) => {
//         acc ++ ( for { comb <- acc; n <- 1 to tm } yield (ch, n) :: comb )
//       } 
//     }

// val setInfo = shortWords.groupBy(wordOccurrences(_))

// val combs = combinations(wordOccurrences("fallow"))  //letters of interest

// val validCombs = combs.filter(l => l.map(_._2).sum >= 3)

// val validWords = for {vc <- validCombs if setInfo.contains(vc)} yield setInfo.get(vc)

// val vws = validWords.flatten.flatten

// vws.map( k => (k.length, k)).groupBy(_._1).mapValues(_.map(_._2))
// show("")