package model.data

import model.Piece
import model.Piece.PieceId

object StoragePieceSeq {
  type K = PieceId
  type V = Piece
  val emptyPiecesStorage: Storage[K, V] = StoragePieceSeq(store = Nil)

  def build(pieces: Seq[Piece]): StoragePieceSeq = StoragePieceSeq(pieces)
}

import StoragePieceSeq._

case class StoragePieceSeq(override val store: Seq[Piece] = Seq[Piece]()) extends Storage[K, V] {
  override type Store = Seq[Piece]

  override def clear: Storage[K, V] = emptyPiecesStorage
  override def del(k: K): Storage[K, V] = this.copy(store = store.filter(_.id != k))
  override def get(k: K): Seq[V] = store.filter(_.id == k)
  override def put(k: K)(v: Seq[V]): Storage[K, V] = del(k).add(k)(v)
  override def add(k: K)(v: Seq[V]): Storage[K, V] = this.copy(store = store ++ v)
  override def add(storage: Storage[K, V]): Storage[K, V] = this.copy(store = store ++ storage.toSeq)
  override def filterV(condV: V => Boolean): Storage[K, V] = this.copy(store = store.filter(condV))
  override def filterKandV(condK: K => Boolean)(condV: V => Boolean): Storage[K, V] =
    this.copy(store = store.filter(p => condK(p.id) && condV(p)))
  override def filterK(condK: K => Boolean): Storage[K, V] = this.copy(store = store.filter(p => condK(p.id)))
  override def findV(condV: V => Boolean): Option[V] = toSeq.find(condV)
  override def existsV(condV: V => Boolean): Boolean = toSeq.exists(condV)
  override def isEmpty: Boolean = store.isEmpty
  override def toSeq: Seq[V] = store
  override def map(f: V => V): Storage[K, V] = this.copy(store = store.map(p => f(p)))
  override def countV: Int = this.toSeq.size
}

