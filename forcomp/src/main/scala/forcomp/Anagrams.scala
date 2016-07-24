package forcomp


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(word: Word): Occurrences = {
    word
      .toLowerCase
      .toList
      .groupBy(c => c)
      .toList
      .sortBy[Char]((a) => a._1)
      .map(occurence => (occurence._1, occurence._2.size))
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    wordOccurrences(
      s.foldLeft("")( (b, a) => b.concat(a) )
    )
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    dictionary.groupBy((word) => wordOccurrences(word))
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    dictionaryByOccurrences.getOrElse(wordOccurrences(word), List())
  }

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
   // Start with empty list
   // Given elem `a`, add List('a', 1)
   // If that is the entire occurrences, then return List( List(('a', 1)), List() )
   // If not, combine ('a', 1) with a recurse call to the rest of the occurences less `a`
   // do the same through n for ('a', n).
   def combinations(occurrences: Occurrences): List[Occurrences] =
     occurrences.foldRight(List[Occurrences](Nil)) { case ((char, times), accum) => {
       accum ++ (for {
         i <- 1 to times
         combination <- accum
       } yield (char, i) :: combination )
     } }

  /** Subtracts occurrence list `y` from occurrence list `from`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `from` -- any character appearing in `y` must
   *  appear in `from`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `from`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(from: Occurrences, y: Occurrences): Occurrences = {
    y.foldLeft(from)((remaining, toSubtract) =>
      for {
        occ <- remaining
        if (occ._1 != toSubtract._1  || occ._2 > toSubtract._2)
      } yield {
        if (occ._1 == toSubtract._1)
          (occ._1, occ._2 - toSubtract._2)
        else
          occ
      }
    )
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */

   /*
   An anagram of a sentence is a list of words that has the same Occurrences as the list.

   We want to compute all such lists.

   A list of words maps to a list of Occurrences for each word.

   The sentence S has an Occurrences list (call it occ). We also have subOcc, the set of all Occurrences
   that are a subset of occ.  The subsets map to new words that are NOT in the sentence, but only container
   letters from the sentence.  Note that not all occurrences in subOcc map to a word.

   Approach:
    - Create method `recurse` which iterate over the subOcc list.  The for loop has a conditional
      which checks if the occurrence 'occ' is real word.  It does not yield anything if it is not a real word.
      If occ is a real word, it will yield a call to recurse which operates on the subOcc after subtracting occ.
      The value that is yielded will be concated to the running acc.  So, it will likely look like a fold
      with a for loop.

   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {

    def recurse(occurrences: Occurrences): List[Sentence] = {
      if (occurrences.isEmpty) {
        List(Nil)
      } else {
        val combos = combinations(occurrences)
        for {
          occ <- combos if ( dictionaryByOccurrences.get(occ).isDefined )
          word <- dictionaryByOccurrences(occ)
          sentences <- recurse(subtract(occurrences, occ))
        } yield {
          word :: sentences
        }
      }
    }

    recurse(sentenceOccurrences(sentence))
  }
}
