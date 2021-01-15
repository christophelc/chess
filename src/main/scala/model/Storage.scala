package model

trait Storage[P, M] {
  val store: Map[P, Seq[M]] = Map()

  /**
   * Erase all
   * @return an empty Storage object
   */
  def clear: Storage[P, M]

  /**
   * Erase the entry for the key p
   * @param p
   * @return
   */
  def del(p: P): Storage[P, M]

  /**
   * Replace if exist p -> m
   * @param p the key
   * @param m the sequence of values
   * @return the update storage object
   */
  def put(p: P)(m: Seq[M]): Storage[P, M]

  /**
   * Add the entry p with the values m. If there are already existing values,
   * we add them.
   * @param p
   * @param m
   * @return
   */
  def add(p: P)(m: Seq[M]): Storage[P, M]

  def add(storage: Storage[P, M]): Storage[P, M]

  /**
   * Filter other all the values
   * @param condM
   * @return the new object
   */
  def filterM(condM: M => Boolean): Storage[P, M]

  /**
   * Filter other all the keys
   * @param condP
   * @return the new object
   */
  def filterP(condP: P => Boolean): Storage[P, M]

  def findM(condM: M => Boolean): Option[M]

  def existsM(condM: M => Boolean): Boolean

  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

  def toSeq: Seq[M]

  def map(f: M => M): Storage[P, M]

  /**
   * Sum the size of all the values
   * @return the sum
   */
  def countM: Int
}