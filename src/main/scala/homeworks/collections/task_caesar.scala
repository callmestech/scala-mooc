package homeworks.collections

object task_caesar {

  /** from 65 to 90 */
  val AlphabetLength: Int = ('A' to 'Z').size

  /**
   * В данном задании Вам предлагается реализовать функции,
   * реализующие кодирование/декодирование строки шифром Цезаря.
   * https://ru.wikipedia.org/wiki/Шифр_Цезаря
   * Алфавит - прописные латинские буквы от A до Z.
   * Сдвиг   - неотрицательное целое число.
   * Пример: при сдвиге 2 слово "SCALA" шифруется как "UECNC".
   */
  /**
   * @param word   входное слово, которое необходимо зашифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return зашифрованное слово
   */
  def encrypt(word: String, offset: Int): String =
    shift(word, offset % AlphabetLength)

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String =
    shift(cipher, AlphabetLength - offset % AlphabetLength)

  private def shift(s: String, offset: Int): String =
    s.map { c =>
      val x = c + offset
      (if (x > 'Z') x - AlphabetLength else x).asInstanceOf[Char]
    }
}
