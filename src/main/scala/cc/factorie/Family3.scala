/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable}
import scala.util.{Random,Sorting}
import scala.util.Random
import scala.math
import scala.util.Sorting
import cc.factorie.la._
import cc.factorie.util.Substitutions
import java.io._

abstract class Family3[N1<:Variable,N2<:Variable,N3<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends FamilyWithNeighborDomains {
  type NeighborType1 = N1
  type NeighborType2 = N2
  type NeighborType3 = N3
  val neighborClass1 = nm1.erasure
  val neighborClass2 = nm2.erasure
  val neighborClass3 = nm3.erasure
  val nc1a = { val ta = nm1.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass1)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc2a = { val ta = nm2.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass2)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc3a = { val ta = nm3.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass3)) { assert(ta.length == 1); ta.head.erasure } else null }
  protected var _neighborDomain1: Domain[N1#Value] = null
  protected var _neighborDomain2: Domain[N2#Value] = null
  protected var _neighborDomain3: Domain[N3#Value] = null
  def neighborDomain1: Domain[N1#Value] = if (_neighborDomain1 eq null) throw new Error("You must override neighborDomain1 if you want to access it before creating any Factor objects") else _neighborDomain1
  def neighborDomain2: Domain[N2#Value] = if (_neighborDomain2 eq null) throw new Error("You must override neighborDomain2 if you want to access it before creating any Factor objects") else _neighborDomain2
  def neighborDomain3: Domain[N3#Value] = if (_neighborDomain3 eq null) throw new Error("You must override neighborDomain3 if you want to access it before creating any Factor objects") else _neighborDomain3

  type FactorType = Factor
  final case class Factor(_1:N1, _2:N2, _3:N3, override var inner:Seq[cc.factorie.Factor] = Nil, override var outer: cc.factorie.Factor = null) extends super.Factor {
    if (_neighborDomains eq null) {
      _neighborDomain1 = _1.domain.asInstanceOf[Domain[N1#Value]]
      _neighborDomain2 = _2.domain.asInstanceOf[Domain[N2#Value]]
      _neighborDomain3 = _3.domain.asInstanceOf[Domain[N3#Value]]
      _neighborDomains = _newNeighborDomains
      _neighborDomains += _neighborDomain1
      _neighborDomains += _neighborDomain2
      _neighborDomains += _neighborDomain3
    }
    def numVariables = 3
    def variable(i:Int) = i match { case 0 => _1; case 1 => _2; case 2 => _3; case _ => throw new IndexOutOfBoundsException(i.toString) }
    override def variables: IndexedSeq[Variable] = IndexedSeq(_1, _2, _3)
    def values: ValuesType = new Values(_1.value, _2.value, _3.value, inner.map(_.values))
    def statistics: StatisticsType = Family3.this.statistics(values)
    override def cachedStatistics: StatisticsType = Family3.this.cachedStatistics(values)
    def copy(s:Substitutions) = Factor(s.sub(_1), s.sub(_2), s.sub(_3), inner.map(_.copy(s)), outer)
  } 
  // Values
  type ValuesType = Values
  final case class Values(_1:N1#Value, _2:N2#Value, _3:N3#Value, override val inner:Seq[cc.factorie.Values] = Nil) extends super.Values {
    def statistics = Family3.this.statistics(this)
  }
  // Statistics
  def statistics(values:Values): StatisticsType
  private var cachedStatisticsArray: Array[StatisticsType] = null
  private var cachedStatisticsHash: HashMap[Product,StatisticsType] = null
  /** It is callers responsibility to clearCachedStatistics if weights or other relevant state changes. */
  override def cachedStatistics(values:Values, stats:(ValuesType)=>StatisticsType): StatisticsType =
    if (Template.enableCachedStatistics) {
    //println("Template3.cachedStatistics")
    if (values._1.isInstanceOf[DiscreteValue] && values._2.isInstanceOf[DiscreteValue] && values._3.isInstanceOf[DiscreteVectorValue] /*&& f._3.isConstant*/ ) {
      val v1 = values._1.asInstanceOf[DiscreteValue]
      val v2 = values._2.asInstanceOf[DiscreteValue]
      val v3 = values._3.asInstanceOf[DiscreteVectorValue]
      if (cachedStatisticsHash eq null) cachedStatisticsHash = new HashMap[Product,StatisticsType]
      val i = ((v1.intValue, v2.intValue, v3))
      //print(" "+((v1.intValue, v2.intValue))); if (cachedStatisticsHash.contains(i)) println("*") else println(".")
      cachedStatisticsHash.getOrElseUpdate(i, stats(values))
    } else {
      stats(values)
    }
  } else stats(values)
  override def clearCachedStatistics: Unit =  { cachedStatisticsArray = null; cachedStatisticsHash = null }
  /** Values iterator in style of specifying fixed neighbors. */
  def valuesIterator(factor:FactorType, fixed: Assignment): Iterator[Values] = {
    val fixed1 = fixed.contains(factor._1)
    val fixed2 = fixed.contains(factor._2)
    val fixed3 = fixed.contains(factor._3)
    if (fixed1 && fixed2 && fixed3) 
      Iterator.single(Values(fixed(factor._1), fixed(factor._2), fixed(factor._3)))
    else if (fixed1 && fixed2) {
      val val1 = fixed(factor._1)
      val val2 = fixed(factor._2)
      val d3 = factor._3.domain.asInstanceOf[IterableDomain[N3#Value]]
      d3.values.iterator.map(value => Values(val1, val2, value))
    } else if (fixed2 && fixed3) {
      val val2 = fixed(factor._2)
      val val3 = fixed(factor._3)
      val d1 = factor._1.domain.asInstanceOf[IterableDomain[N1#Value]]
      d1.values.iterator.map(value => Values(value, val2, val3))
    } else if (fixed1 && fixed3) {
      val val1 = fixed(factor._1)
      val d2 = factor._2.domain.asInstanceOf[IterableDomain[N2#Value]]
      val val3 = fixed(factor._3)
      d2.values.iterator.map(value => Values(val1, value, val3))
    } else if (fixed1) {
      val val1 = fixed(factor._1)
      val d2 = factor._2.domain.asInstanceOf[IterableDomain[N2#Value]]
      val d3 = factor._3.domain.asInstanceOf[IterableDomain[N3#Value]]
      (for (val2 <- d2.values; val3 <- d3.values) yield Values(val1, val2, val3)).iterator
    } else if (fixed2) {
      val d1 = factor._1.domain.asInstanceOf[IterableDomain[N1#Value]]
      val val2 = fixed(factor._2)
      val d3 = factor._3.domain.asInstanceOf[IterableDomain[N3#Value]]
      (for (val1 <- d1.values; val3 <- d3.values) yield Values(val1, val2, val3)).iterator
    } else if (fixed3) {
      val d1 = factor._1.domain.asInstanceOf[IterableDomain[N1#Value]]
      val d2 = factor._2.domain.asInstanceOf[IterableDomain[N2#Value]]
      val val3 = fixed(factor._3)
      (for (val1 <- d1.values; val2 <- d2.values) yield Values(val1, val2, val3)).iterator
    } else {
      val d1 = factor._1.domain.asInstanceOf[IterableDomain[N1#Value]]
      val d2 = factor._2.domain.asInstanceOf[IterableDomain[N2#Value]]
      val d3 = factor._3.domain.asInstanceOf[IterableDomain[N3#Value]]
      (for (val1 <- d1.values; val2 <- d2.values; val3 <- d3.values) yield Values(val1, val2, val3)).iterator
    }
  }
  /** valuesIterator in style of specifying varying neighbors */
  def valuesIterator(factor:FactorType, varying:Seq[Variable]): Iterator[Values] = {
    val varying1 = varying.contains(factor._1)
    val varying2 = varying.contains(factor._2)
    val varying3 = varying.contains(factor._3)
    if (varying1 && varying2 & varying3) {
      val d1 = factor._1.domain.asInstanceOf[IterableDomain[N1#Value]]
      val d2 = factor._2.domain.asInstanceOf[IterableDomain[N2#Value]]
      val d3 = factor._3.domain.asInstanceOf[IterableDomain[N3#Value]]
      (for (val1 <- d1.values; val2 <- d2.values; val3 <- d3.values) yield Values(val1, val2, val3)).iterator
    } else if (varying1 && varying2) {
      val d1 = factor._1.domain.asInstanceOf[IterableDomain[N1#Value]]
      val d2 = factor._2.domain.asInstanceOf[IterableDomain[N2#Value]]
      val val3 = factor._3.value
      (for (val1 <- d1.values; val2 <- d2.values) yield Values(val1, val2, val3)).iterator
    } else if (varying2 && varying3) {
      val val1 = factor._1.value
      val d2 = factor._2.domain.asInstanceOf[IterableDomain[N2#Value]]
      val d3 = factor._3.domain.asInstanceOf[IterableDomain[N3#Value]]
      (for (val2 <- d2.values; val3 <- d3.values) yield Values(val1, val2, val3)).iterator
    } else if (varying1 && varying3) {
      val d1 = factor._1.domain.asInstanceOf[IterableDomain[N1#Value]]
      val val2 = factor._2.value
      val d3 = factor._3.domain.asInstanceOf[IterableDomain[N3#Value]]
      (for (val1 <- d1.values; val3 <- d3.values) yield Values(val1, val2, val3)).iterator
    } else if (varying1) {
      val d1 = factor._1.domain.asInstanceOf[IterableDomain[N1#Value]]
      val val2 = factor._2.value
      val val3 = factor._3.value
      (for (val1 <- d1.values) yield Values(val1, val2, val3)).iterator
    } else if (varying2) {
      val val1 = factor._1.value
      val d2 = factor._2.domain.asInstanceOf[IterableDomain[N2#Value]]
      val val3 = factor._3.value
      (for (val2 <- d2.values) yield Values(val1, val2, val3)).iterator
    } else if (varying3) {
      val val1 = factor._1.value
      val val2 = factor._2.value
      val d3 = factor._3.domain.asInstanceOf[IterableDomain[N3#Value]]
      (for (val3 <- d3.values) yield Values(val1, val2, val3)).iterator
    } else {
      Iterator.single(Values(factor._1.value, factor._2.value, factor._3.value))
    }
  }
}

trait Statistics3[S1,S2,S3] extends Family {
  final case class Stat(_1:S1, _2:S2, _3:S3, override val inner:Seq[cc.factorie.Statistics] = Nil) extends super.Statistics
  type StatisticsType = Stat
}

trait VectorStatistics3[S1<:DiscreteVectorValue,S2<:DiscreteVectorValue,S3<:DiscreteVectorValue] extends VectorFamily {
  type StatisticsType = Stat
  final case class Stat(_1:S1, _2:S2, _3:S3, override val inner:Seq[cc.factorie.Statistics] = Nil) extends { val vector: Vector = _1 flatOuter (_2 flatOuter _3) } with super.Statistics {
    if (_statisticsDomains eq null) {
      _statisticsDomains = _newStatisticsDomains
      _statisticsDomains += _1.domain
      _statisticsDomains += _2.domain
      _statisticsDomains += _3.domain
    }
  }
}

trait DotStatistics3[S1<:DiscreteVectorValue,S2<:DiscreteVectorValue,S3<:DiscreteVectorValue] extends VectorStatistics3[S1,S2,S3] with DotFamily

abstract class FamilyWithStatistics3[N1<:Variable,N2<:Variable,N3<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Family3[N1,N2,N3] with Statistics3[N1#Value,N2#Value,N3#Value] {
  def statistics(values:Values): StatisticsType = Stat(values._1, values._2, values._3, values.inner.map(_.statistics))
}
