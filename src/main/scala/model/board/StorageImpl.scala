package model.board

import model.Chessboard.MovesStorage
import model.Storage

object StorageImpl {
  val emptyMoveStorage: MovesStorage = StorageImpl(store = Map())
}

case class StorageImpl[K, V](override val store: Map[K, Seq[V]] = Map[K, Seq[V]]()) extends Storage[K, V] {
  override def clear: Storage[K, V] = this.copy(store = Map())
  override def del(k: K): Storage[K, V] = this.copy(store = store - k)
  override def put(k: K)(v: Seq[V]): Storage[K, V] = this.copy(store = store ++ Map(k -> v))
  override def add(k: K)(v: Seq[V]): Storage[K, V] = store.get(k) match {
    case Some(vOld) => this.copy(store = (store - k) ++ Map(k -> (vOld ++ v)))
    case None => this.copy(store = store ++ Map(k -> v))
  }
  override def add(storage: Storage[K, V]): Storage[K, V] = {
    val storeAdd: Map[K, Seq[V]] = storage.store
    storeAdd.keySet.foldLeft(this: Storage[K, V])((storeAcc, k) =>
      storeAcc.add(k)(storeAdd(k)))
  }
  override def filterV(condV: V => Boolean): Storage[K, V] =
    this.copy(store = store.map {
      case (k, v) => k -> v.filter(condV)
    }.filter(_._2.nonEmpty))

  override def filterK(condK: K => Boolean): Storage[K, V] =
    this.copy(store = store.filterKeys(condK))

  override def findV(condV: V => Boolean): Option[V] = toSeq.find(condV)

  override def existsV(condV: V => Boolean): Boolean = toSeq.exists(condV)

  override def isEmpty: Boolean = store.isEmpty

  override def toSeq: Seq[V] = store.values.flatten.toSeq

  override def map(f: V => V): Storage[K, V] =
    this.copy(store = store.map {
      case (k, v) => k -> v.map(el => f(el))
    })

  override def countV: Int = this.toSeq.size
}

