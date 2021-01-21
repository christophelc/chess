package model.data

import model.Piece
import model.Piece.PieceId

case class StorageSeq[K, V](partition: V => K, override val store: Seq[V] = Seq[V]()) extends Storage[K, V] {
  override type Store = Seq[V]

  override def clear: Storage[K, V] = this.copy(store = Seq[V]())
  override def del(k: K): Storage[K, V] = this.copy(store = store.filter(partition(_) != k))
  override def get(k: K): Seq[V] = store.filter(partition(_) == k)
  override def put(k: K)(v: Seq[V]): Storage[K, V] = del(k).add(k)(v)
  override def add(k: K)(v: Seq[V]): Storage[K, V] = this.copy(store = store ++ v)
  override def add(storage: Storage[K, V]): Storage[K, V] = this.copy(store = store ++ storage.toSeq)
  override def filterV(condV: V => Boolean): Storage[K, V] = this.copy(store = store.filter(condV))
  override def filterKxV(condKV: (K, V) => Boolean): Storage[K, V] =
    this.copy(store = store.filter(v => condKV(partition(v), v)))
  override def filterKandV(condK: K => Boolean)(condV: V => Boolean): Storage[K, V] =
    this.copy(store = store.filter(p => condK(partition(p)) && condV(p)))
  override def filterK(condK: K => Boolean): Storage[K, V] =
    this.copy(store = store.filter(p => condK(partition(p))))
  override def findV(condV: V => Boolean): Option[V] = toSeq.find(condV)
  override def existsV(condV: V => Boolean): Boolean = toSeq.exists(condV)
  override def isEmpty: Boolean = store.isEmpty
  override def toSeq: Seq[V] = store
  override def map(f: V => V): Storage[K, V] = this.copy(store = store.map(p => f(p)))
  override def countV: Int = this.toSeq.size
}

