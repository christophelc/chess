package model

trait TagValue[M]

/**
 * Tag an object M
 * @tparam M the object to be tagged
 */
trait Tagable[M] {
  /**
   * Return a copy of the object M with a add a tag.
   * @param tag the tag to be added
   * @return this object updated
   */
  def enableTag(tag: TagValue[M]): M

  /**
   * Return a copy of the object M and remove the tag if it exsits.
   * @param tag the tag to be removed
   * @return this object updated
   */
  def disableTag(tag: TagValue[M]): M
  def isTagged(tag: TagValue[M]): Boolean
}
