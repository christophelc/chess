package model.board

import model.Chessboard.MovesStorage
import model.{ GenericMove, Piece, Storage, Tagable }

object StorageImpl {
  val emptyMoveStorage: MovesStorage = StorageImpl(store = Map())
}

case class StorageImpl[P, M](override val store: Map[P, Seq[M]] = Map()) extends Storage[P, M] {
  override def clear: Storage[P, M] = this.copy(store = Map())
  override def del(p: P): Storage[P, M] = this.copy(store = store - p)
  override def put(p: P)(m: Seq[M]): Storage[P, M] = this.copy(store = store ++ Map(p -> m))
  override def add(p: P)(m: Seq[M]): Storage[P, M] = store.get(p) match {
    case Some(_) => this.copy(store = Map(p -> (store(p) ++ m)))
    case None => this.copy(store = store ++ Map(p -> m))
  }
  override def add(storage: Storage[P, M]): Storage[P, M] = {
    val storeAdd: Map[P, Seq[M]] = storage.store
    storeAdd.keySet.foldLeft(this: Storage[P, M])((storeAcc, p) =>
      storeAcc.add(p)(storeAdd(p)))
  }
  override def filterM(condM: M => Boolean): Storage[P, M] =
    this.copy(store = store.map {
      case (p, m) => p -> m.filter(condM)
    }.filter(_._2.nonEmpty))

  override def filterP(condP: P => Boolean): Storage[P, M] =
    this.copy(store = store.filterKeys(condP))

  override def findM(condM: M => Boolean): Option[M] = toSeq.find(condM)

  override def existsM(condM: M => Boolean): Boolean = toSeq.exists(condM)

  override def isEmpty: Boolean = store.isEmpty

  override def toSeq: Seq[M] = store.values.flatten.toSeq

  override def map(f: M => M): Storage[P, M] =
    this.copy(store = store.map {
      case (p, m) => p -> m.map(el => f(el))
    })

  override def countM: Int = this.toSeq.size
}

