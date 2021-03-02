package com.evolution.homework.typeclass

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

//fill in implementation gaps here making the ImplicitsHomeworkSpec pass!
object ImplicitsHomework {
  /**
   * Lo and behold! Brand new super-useful collection library for Scala!
   *
   * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
   * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
   * of the data stored.
   *
   * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
   * a thing called size score. Its calculation rules:
   * - size score of a Byte is 1
   * - Int - 4 (as primitive JVM int consists of 4 bytes)
   * - Long - 8
   * - Char - 2 (one UTF-16 symbol is 2 bytes)
   * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
   * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
   * the fields
   * - score for any sequence (Array[T], List[T], Vector[T]) is
   * 12 (our old friend object header) + sum of scores of all elements
   * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
   */
  object SuperVipCollections4s {
    type SizeScore = Int

    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object syntax {
      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        def sizeScore: SizeScore = implicitly[GetSizeScore[T]].apply(inner) //implement the syntax!
      }
    }

    /**
     * Mutable key-value cache which limits the size score of the data scored.
     *
     * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
     * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
     * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
     * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
     *
     * @param maxSizeScore max size score for the stored data
     * @tparam K key type
     * @tparam V value type
     */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {
      //with this you can use .sizeScore syntax on keys and values
      import syntax._

      /*
      mutable.LinkedHashMap is a mutable map container which preserves insertion order - this might be useful!
       */
      private val map = mutable.LinkedHashMap.empty[K, V]
      private var totalSizeScore: SizeScore = 0

      def put(key: K, value: V): Unit = {
        val kvSizeScore = key.sizeScore + value.sizeScore
        // kv is larger than whole cache
        if (kvSizeScore > maxSizeScore) ()

        def putAndIncrementSize(key: K, value: V): Unit = {
          map.put(key, value)
          totalSizeScore += kvSizeScore
        }

        val kvInMapSizeScore = totalSizeScore + kvSizeScore
        // kv fits in a cache
        if (kvInMapSizeScore <= maxSizeScore) {
          putAndIncrementSize(key, value)
        }
        else {
          // kv can be placed in a cache but only after eviction
          while (kvSizeScore + totalSizeScore > maxSizeScore) {
            val (headKey, headValue) = map.head
            val removeEntrySizeScore = headKey.sizeScore + headValue.sizeScore
            map.remove(headKey)
            totalSizeScore -= removeEntrySizeScore
          }
          putAndIncrementSize(key, value)
        }
      }

      def get(key: K): Option[V] = map.get(key)
    }

    /**
     * Cool custom immutable multi-map collection - does not extend the standard library collection types
     * (yes, this is a feature)
     */
    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])
    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()
      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }

    /**
     * Type-class allowing us to iterate over different "collection-like" types with one type arg
     */
    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }

    implicit class IterateOps[F[_]: Iterate, T](inner: F[T]) {
      def iterator: Iterator[T] = implicitly[Iterate[F]].iterator(inner)
    }

    /**
     * Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
     */
    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]
      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    implicit class Iterate2Ops[F[_,_]: Iterate2, K, V](inner: F[K, V]) {
      def iterator1: Iterator[K] = implicitly[Iterate2[F]].iterator1(inner)
      def iterator2: Iterator[V] = implicitly[Iterate2[F]].iterator2(inner)
    }

    object instances {
      import syntax._

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }
      //Array is not an Iterable in Scala 2.13 but we still might abstract over iteration logic for both!
      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }
      //Provide Iterate2 instances for Map and PackedMultiMap!
      implicit val mapIterate: Iterate2[Map] = new Iterate2[Map] {
        override def iterator1[T, S](f: Map[T, S]): Iterator[T] = f.keys.iterator

        override def iterator2[T, S](f: Map[T, S]): Iterator[S] = f.values.iterator
      }

      implicit val packedMultiMapIterate: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = f.inner.map{case (k, _) => k}.iterator

        override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = f.inner.map{case (_, v) => v}.iterator
      }

      private val ObjectHeaderSizeScore = 12
      private val CharSizeScore = 2

      implicit val byteSizeScore: GetSizeScore[Byte] = _ => 1
      implicit val intSizeScore: GetSizeScore[Int] = _ => 4
      implicit val longSizeScore: GetSizeScore[Long] = _ => 8
      implicit val charSizeScore: GetSizeScore[Char] = _ => CharSizeScore
      implicit val stringSizeScore: GetSizeScore[String] = ObjectHeaderSizeScore + _.length * CharSizeScore
      implicit class SeqSizeScore[F[_]: Iterate, T : GetSizeScore](inner : F[T]) {
        def sizeScore: SizeScore = inner.iterator.map(_.sizeScore).sum + ObjectHeaderSizeScore
      }
      implicit class DictSizeScore[F[_, _] : Iterate2, K : GetSizeScore, V : GetSizeScore](inner : F[K, V]) {
        def sizeScore: SizeScore = {
          val keysSizeScore = inner.iterator1.map(_.sizeScore).sum
          val valuesSizeScore = inner.iterator2.map(_.sizeScore).sum
          keysSizeScore + valuesSizeScore + ObjectHeaderSizeScore
        }
      }
    }
  }

  /*
  Time to bring some business value!
  #GoodVibes #ThrowbackThursday #NoFilter #squadgoals
   */
  object MyTwitter {
    import SuperVipCollections4s._

    final case class Twit(
                           id: Long,
                           userId: Int,
                           hashTags: Vector[String],
                           attributes: PackedMultiMap[String, String],
                           fbiNotes: List[FbiNote],
                         )

    final case class FbiNote(
                              month: String,
                              favouriteChar: Char,
                              watchedPewDiePieTimes: Long,
                            )

    trait TwitCache {
      def put(twit: Twit): Unit
      def get(id: Long): Option[Twit]
    }


    import instances._
    import SuperVipCollections4s.syntax._
    implicit val fbiNotesSizeScore: GetSizeScore[FbiNote] =
      (f: FbiNote) => f.month.sizeScore + f.favouriteChar.sizeScore + f.watchedPewDiePieTimes.sizeScore
    implicit val twitSizeScore: GetSizeScore[Twit] =
      (t: Twit) => t.id.sizeScore + t.userId.sizeScore + t.hashTags.sizeScore + t.attributes.sizeScore + t.fbiNotes.sizeScore

    /*
    Return an implementation based on MutableBoundedCache[Long, Twit]
     */
    def createTwitCache(maxSizeScore: SizeScore): TwitCache = new TwitCache {
      private val cache = new MutableBoundedCache[Long, Twit](maxSizeScore)
      override def put(twit: Twit): Unit = cache.put(twit.id, twit)

      override def get(id: Long): Option[Twit] = cache.get(id)
    }
  }
}
