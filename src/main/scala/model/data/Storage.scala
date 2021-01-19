package model.data

// TODO: extends Iterable[(K, V)]
trait Storage[K, V] {
  type Store
  val store: Store

  /**
   * Erase all
   *
   * @return an empty Storage object
   */
  def clear: Storage[K, V]

  /**
   * Erase the entry for the key p
   *
   * @param k
   * @return
   */
  def del(k: K): Storage[K, V]

  def get(k: K): Seq[V]
  /**
   * Replace if exist p -> m
   *
   * @param k the key
   * @param v the sequence of values
   * @return the update storage object
   */
  def put(k: K)(v: Seq[V]): Storage[K, V]

  /**
   * Add the entry p with the values m. If there are already existing values,
   * we add them.
   *
   * @param k key
   * @param v value
   * @return
   */
  def add(k: K)(v: Seq[V]): Storage[K, V]

  def add(storage: Storage[K, V]): Storage[K, V]

  /**
   * Filter other all the values
   *
   * @param condV
   * @return the new object
   */
  def filterV(condV: V => Boolean): Storage[K, V]

  /**
   * Filter other all the keys
   *
   * @param condK condition over key
   * @return the new object
   */
  def filterK(condK: K => Boolean): Storage[K, V]

  def findV(condV: V => Boolean): Option[V]

  def existsV(condV: V => Boolean): Boolean

  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

  def toSeq: Seq[V]

  def map(f: V => V): Storage[K, V]

  /**
   * Sum the size of all the values
   *
   * @return the sum
   */
  def countV: Int
}
