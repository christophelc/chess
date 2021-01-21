package model.data

import model.Piece
import model.Piece.{ PieceId, idBishop, idKing, idKnight, idPawn, idQueen, idRook }
import scala.collection.SeqView

object PiecesData {
  type K = PieceId
  type V = Piece
  val emptyPieceBoardStorage: PiecesData = PiecesData()
}

import PiecesData._
final case class StorePieceBoard(
  rooks: Seq[Piece] = Nil,
  knights: Seq[Piece] = Nil,
  bishops: Seq[Piece] = Nil,
  queens: Seq[Piece] = Nil,
  kings: Seq[Piece] = Nil,
  pawns: Seq[Piece] = Nil)

final case class PiecesData(override val store: StorePieceBoard = StorePieceBoard()) extends Storage[K, V] {
  override type Store = StorePieceBoard

  val view: SeqView[Seq[Piece]] = Seq(
    store.rooks,
    store.knights,
    store.bishops,
    store.queens,
    store.kings,
    store.pawns).view

  /**
   * Erase all
   *
   * @return an empty Storage object
   */
  override def clear: Storage[K, V] = emptyPieceBoardStorage

  /**
   * Erase the entry for the key p
   *
   * @param k
   * @return
   */
  override def del(k: K): Storage[K, V] = k match {
    case Piece.idRook => this.copy(store = store.copy(rooks = store.rooks.filter(_.id != k)))
    case Piece.idKnight => this.copy(store = store.copy(knights = store.knights.filter(_.id != k)))
    case Piece.idBishop => this.copy(store = store.copy(bishops = store.bishops.filter(_.id != k)))
    case Piece.idQueen => this.copy(store = store.copy(queens = store.queens.filter(_.id != k)))
    case Piece.idKing => this.copy(store = store.copy(kings = store.kings.filter(_.id != k)))
    case Piece.idPawn => this.copy(store = store.copy(pawns = store.pawns.filter(_.id != k)))
  }

  override def get(k: K): Seq[V] = k match {
    case Piece.idRook => store.rooks
    case Piece.idKnight => store.knights
    case Piece.idBishop => store.bishops
    case Piece.idQueen => store.queens
    case Piece.idKing => store.kings
    case Piece.idPawn => store.pawns
  }

  /**
   * Replace if exist p -> m
   *
   * @param k the key
   * @param v the sequence of values
   * @return the update storage object
   */
  override def put(k: K)(v: Seq[V]): Storage[K, V] = k match {
    case Piece.idRook => this.copy(store = store.copy(rooks = v))
    case Piece.idKnight => this.copy(store = store.copy(knights = v))
    case Piece.idBishop => this.copy(store = store.copy(bishops = v))
    case Piece.idQueen => this.copy(store = store.copy(queens = v))
    case Piece.idKing => this.copy(store = store.copy(kings = v))
    case Piece.idPawn => this.copy(store = store.copy(pawns = v))
  }

  /**
   * Add the entry p with the values m. If there are already existing values,
   * we add them.
   *
   * @param k key
   * @param v value
   * @return
   */
  override def add(k: K)(v: Seq[V]): Storage[K, V] = k match {
    case Piece.idRook => this.copy(store = store.copy(rooks = store.rooks ++ v))
    case Piece.idKnight => this.copy(store = store.copy(knights = store.knights ++ v))
    case Piece.idBishop => this.copy(store = store.copy(bishops = store.bishops ++ v))
    case Piece.idQueen => this.copy(store = store.copy(queens = store.queens ++ v))
    case Piece.idKing => this.copy(store = store.copy(kings = store.kings ++ v))
    case Piece.idPawn => this.copy(store = store.copy(pawns = store.pawns ++ v))
  }

  override def add(storage: Storage[K, V]): Storage[K, V] =
    this.copy(store = store.copy(
      rooks = store.rooks ++ storage.get(idRook),
      knights = store.knights ++ storage.get(idKnight),
      bishops = store.bishops ++ storage.get(idBishop),
      queens = store.queens ++ storage.get(idQueen),
      kings = store.kings ++ storage.get(idKing),
      pawns = store.pawns ++ storage.get(idPawn)))

  override def filterV(condV: V => Boolean): Storage[K, V] = filterKandV(_ => true)(condV)

  override def filterKxV(condKV: (K, V) => Boolean): Storage[K, V] =
    this.copy(store = store.copy(
      rooks = store.rooks.filter(p => condKV(p.id, p)),
      knights = store.knights.filter(p => condKV(p.id, p)),
      bishops = store.bishops.filter(p => condKV(p.id, p)),
      queens = store.queens.filter(p => condKV(p.id, p)),
      kings = store.kings.filter(p => condKV(p.id, p)),
      pawns = store.pawns.filter(p => condKV(p.id, p))))

  override def filterKandV(condK: K => Boolean)(condV: V => Boolean): Storage[K, V] = {
    this.copy(store = store.copy(
      rooks = store.rooks.filter(p => condK(p.id) && condV(p)),
      knights = store.knights.filter(p => condK(p.id) && condV(p)),
      bishops = store.bishops.filter(p => condK(p.id) && condV(p)),
      queens = store.queens.filter(p => condK(p.id) && condV(p)),
      kings = store.kings.filter(p => condK(p.id) && condV(p)),
      pawns = store.pawns.filter(p => condK(p.id) && condV(p))))
  }

  /**
   * @param condK condK over key
   * @return  the new object
   */

  override def filterK(condK: K => Boolean): Storage[K, V] =
    this.copy(store = store.copy(
      rooks = store.rooks.filter(p => condK(p.id)),
      knights = store.knights.filter(p => condK(p.id)),
      bishops = store.bishops.filter(p => condK(p.id)),
      queens = store.queens.filter(p => condK(p.id)),
      kings = store.kings.filter(p => condK(p.id)),
      pawns = store.pawns.filter(p => condK(p.id))))

  override def findV(condV: V => Boolean): Option[V] =
    view.foldLeft(None: Option[V])((acc, seq) =>
      if (acc.isEmpty) seq.find(condV) else acc)

  override def existsV(condV: V => Boolean): Boolean =
    view.foldLeft(false)((acc, seq) =>
      if (!acc) seq.exists(condV) else acc)

  override def isEmpty: Boolean = !nonEmpty

  override def nonEmpty: Boolean = view.forall(_.nonEmpty)

  override def toSeq: Seq[V] = view.toSeq.flatten

  override def map(f: V => V): Storage[K, V] =
    this.copy(store = store.copy(
      rooks = store.rooks.map(f),
      knights = store.knights.map(f),
      bishops = store.bishops.map(f),
      queens = store.queens.map(f),
      kings = store.kings.map(f),
      pawns = store.pawns.map(f)))

  /**
   * Sum the size of all the values
   *
   * @return the sum
   */
  override def countV: Int = view.map(_.length).sum
}

