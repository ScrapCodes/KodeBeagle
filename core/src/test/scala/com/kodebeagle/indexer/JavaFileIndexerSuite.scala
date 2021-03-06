/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.kodebeagle.indexer

import java.io.StringWriter

import org.apache.commons.io.IOUtils
import org.scalatest.{BeforeAndAfterAll, FunSuite}

class JavaFileIndexerSuite extends FunSuite with BeforeAndAfterAll {
  val stream =
    Thread.currentThread().getContextClassLoader.getResourceAsStream("TransportClient.java")
  val writer = new StringWriter()
  val allOccurrences = List(74, 75, 79, 101, 103, 105, 106, 108, 109, 114, 117, 119, 120, 121, 123,
    125, 137, 139, 141, 144, 145, 150, 154, 156, 158, 160, 172, 187, 188, 189, 191, 198, 203, 204)
  val sampleRepo = Repository("sample", 0, "sample", false, "Java", "master", 0)

  override def beforeAll() {
    IOUtils.copy(stream, writer)
  }

  test("Parse a file and verify tokens when lines of context is more than file size") {
    val javaFileIndexer = new JavaFileIndexer {
      override val linesOfContext = 2000
    }
    val resultTokens = javaFileIndexer.generateTokens(Map("sample-master/Sample.java" -> writer
      .toString), List(), Some(Repository.invalid))
    assert(resultTokens.size === 1)
    val result = resultTokens.flatMap(_.tokens.flatMap(_.lineNumbers)).toList.distinct.sorted
    assert(result === allOccurrences)
  }

  test("Parse a file and verify tokens when lines of context is much less than file size") {
    val javaFileIndexer = new JavaFileIndexer {
      override val linesOfContext = 10
    }
    val result = javaFileIndexer.generateTokens(
      Map("sample-master/Sample.java" -> writer.toString), List(), Some(Repository.invalid))
    val occurrences = result.flatMap(x => x.tokens.map(_.lineNumbers)).reduce(_ ++ _)
    assert(occurrences.toList === allOccurrences)
    assert(result.size == 58)
  }

  test("Excluded imports should not be part of Tokens") {
    val javaFileIndexer = new JavaFileIndexer {
      override val linesOfContext = 20
    }
    val excludes = Set("org.apache.spark", "org.apache", "org",
      "org.apache.spark.network.protocol")
    val resultWOExcludes = javaFileIndexer.generateTokens(Map("sample-master/Sample.java" ->
      writer.toString), List(), Some(Repository.invalid))
    val resultWExcludes = javaFileIndexer.generateTokens(Map("sample-master/Sample.java" ->
      writer.toString), excludes.toList, Some(Repository.invalid))
    val expected = resultWOExcludes.flatMap(x => x.tokens.map(_.importName))
      .filterNot(_.startsWith("org.apache.spark.network.protocol"))
    val result = resultWExcludes.flatMap(x => x.tokens.map(_.importName))
    assert(result === expected)
  }

  test("Should not include global variables and declarations in indexes.") {
    val javaFileIndexer = new JavaFileIndexer {
      override val linesOfContext = 2000
    }
    val resultTokens = javaFileIndexer.generateTokens(Map("sample-master/Sample.java" ->
      writer.toString), List(), Some(Repository.invalid))
    val result = resultTokens.flatMap(_.tokens.flatMap(_.lineNumbers)).toList.distinct.sorted
    assert(!result.exists(Set(67, 68, 70)))
  }

  test("Should exclude commented code for processing indexes.") {
    val javaFileIndexer = new JavaFileIndexer {
      override val linesOfContext = 2000
    }
    val resultTokens = javaFileIndexer.generateTokens(Map("sample-master/Sample.java" ->
      writer.toString), List(), Some(Repository.invalid))
    val result = resultTokens.flatMap(_.tokens.flatMap(_.lineNumbers)).toList.distinct.sorted
    assert(!result.exists(Set(77, 82)))
  }

}

class JavaASTBasedIndexerSuite extends FunSuite with BeforeAndAfterAll {
  val stream =
    Thread.currentThread().getContextClassLoader.getResourceAsStream("TransportClient.java")
  val writer = new StringWriter()
  val allOccurrences = List(74, 75, 79, 101, 103, 105, 106, 108, 109, 112, 114, 117, 118, 119, 120,
    121, 123, 125, 137, 139, 141, 144, 145, 148, 150, 153, 154, 156, 158, 160, 172, 177, 182, 187,
    188, 189, 191, 198, 203, 204)
  val sampleRepo = Repository("sample", 0, "sample", false, "Java", "master", 0)

  override def beforeAll() {
    IOUtils.copy(stream, writer)
  }

  test("Parse a file and verify tokens") {
    val indexer = new JavaASTBasedIndexer
    val resultTokens = indexer.generateTokens(Map("sample-master/Sample.java" -> writer
      .toString), List(), Some(Repository.invalid))
    assert(resultTokens.size === 7)
    val result = resultTokens.flatMap(_.tokens.flatMap(_.lineNumbers)).toList.distinct.sorted
    assert(result === allOccurrences)
  }

  test("Excluded imports should not be part of Tokens") {
    val indexer = new JavaASTBasedIndexer
    val excludes = Set("org.apache.spark", "org.apache", "org",
      "org.apache.spark.network.protocol")
    val resultWOExcludes = indexer.generateTokens(Map("sample-master/Sample.java" ->
      writer.toString), List(), Some(Repository.invalid))
    val resultWExcludes = indexer.generateTokens(Map("sample-master/Sample.java" ->
      writer.toString), excludes.toList, Some(Repository.invalid))
    val expected = resultWOExcludes.flatMap(x => x.tokens.map(_.importName))
      .filterNot(_.startsWith("org.apache.spark.network.protocol"))
    val result = resultWExcludes.flatMap(x => x.tokens.map(_.importName))
    assert(expected === result)
  }

  test("Should not include global variables and declarations in indexes.") {
    val indexer = new JavaASTBasedIndexer
    val resultTokens = indexer.generateTokens(Map("sample-master/Sample.java" ->
      writer.toString), List(), Some(Repository.invalid))
    val result = resultTokens.flatMap(_.tokens.flatMap(_.lineNumbers)).toList.distinct.sorted
    assert(!result.exists(Set(67, 68, 70)))
  }

  test("Should exclude commented code for processing indexes.") {
    val indexer = new JavaASTBasedIndexer
    val resultTokens = indexer.generateTokens(Map("sample-master/Sample.java" ->
      writer.toString), List(), Some(Repository.invalid))
    val result = resultTokens.flatMap(_.tokens.flatMap(_.lineNumbers)).toList.distinct.sorted
    assert(!result.exists(Set(77, 82)))
  }

}

class JavaASTBasedIndexerForMethodsSuite extends FunSuite with BeforeAndAfterAll {
  val stream =
    Thread.currentThread().getContextClassLoader.getResourceAsStream("TransportClient.java")
  val writer = new StringWriter()
  val sampleRepo = Repository("sample", 0, "sample", false, "Java", "master", 0)

  override def beforeAll() {
    IOUtils.copy(stream, writer)
  }

  test("Parse a file and verify method tokens") {
    val indexer = new JavaASTBasedIndexerForMethods
    val methodTokens = indexer.generateTokensWithMethods(Map("sample-master/Sample.java" -> writer
      .toString), List(), Some(Repository.invalid))

    assert(methodTokens.size === 7)

    val importChannel: String = "io.netty.channel.Channel"
    val importObjects: String = "com.google.common.base.Objects"
    val importLogger: String = "org.slf4j.Logger"
    val importNettyUtils: String = "org.apache.spark.network.util.NettyUtils"
    val importIOE: String = "java.io.IOException"
    val importUUID: String = "java.util.UUID"
    val importTimeUnit: String = "java.util.concurrent.TimeUnit"
    val importRpcReq: String = "org.apache.spark.network.protocol.RpcRequest"
    val importEE: String = "java.util.concurrent.ExecutionException"
    val importThrowables: String = "com.google.common.base.Throwables"
    val importSFuture: String = "com.google.common.util.concurrent.SettableFuture"
    val importPrecon: String = "com.google.common.base.Preconditions"
    val importSCId: String = "org.apache.spark.network.protocol.StreamChunkId"
    val importCFReq: String = "org.apache.spark.network.protocol.ChunkFetchRequest"
    val testMethodTokens = Set(ImportsMethods(-1, "n-a/n-a/blob/n-a/Sample.java",
      Set(MethodToken(importChannel.toLowerCase, importChannel, List(204),
        Set(MethodAndLines("remoteAddress", List(204)))),
        MethodToken(importObjects.toLowerCase, importObjects, List(203),
          Set(MethodAndLines("toStringHelper", List(203))))), 0),

      ImportsMethods(-1, "n-a/n-a/blob/n-a/Sample.java",
        Set(MethodToken(importChannel.toLowerCase, importChannel, List(79),
          Set(MethodAndLines("isOpen", List(79)),
            MethodAndLines("isActive", List(79))))), 0),

      ImportsMethods(-1, "n-a/n-a/blob/n-a/Sample.java",
        Set(MethodToken(importLogger.toLowerCase, importLogger, List(139, 150, 154, 160),
          Set(MethodAndLines("trace", List(139, 150)),
            MethodAndLines("error", List(154, 160)))),
          MethodToken(importNettyUtils.toLowerCase, importNettyUtils, List(137),
            Set(MethodAndLines("getRemoteAddress", List(137)))),
          MethodToken(importChannel.toLowerCase + "futurelistener",
            importChannel + "FutureListener", List(145), Set()),
          MethodToken(importChannel.toLowerCase + "future", importChannel + "Future",
            List(148, 153, 154, 158),
            Set(MethodAndLines("cause", List(153, 154, 158)),
              MethodAndLines("isSuccess", List(148)))),
          MethodToken(importChannel.toLowerCase, importChannel, List(137, 156, 144),
            Set(MethodAndLines("close", List(156)),
              MethodAndLines("writeAndFlush", List(144)))),
          MethodToken(importIOE.toLowerCase, importIOE, List(158), Set()),
          MethodToken(importUUID.toLowerCase, importUUID, List(141),
            Set(MethodAndLines("randomUUID", List(141)))),
          MethodToken(importRpcReq.toLowerCase, importRpcReq, List(144), Set())), 0),

      ImportsMethods(-1, "n-a/n-a/blob/n-a/Sample.java",
        Set(MethodToken(importTimeUnit.toLowerCase, importTimeUnit, List(198), Set()),
          MethodToken(importChannel.toLowerCase, importChannel, List(198),
            Set(MethodAndLines("close", List(198))))), 0),

      ImportsMethods(-1, "n-a/n-a/blob/n-a/Sample.java",
        Set(MethodToken(importTimeUnit.toLowerCase, importTimeUnit, List(187), Set()),
          MethodToken(importEE.toLowerCase, importEE, List(188, 189),
            Set(MethodAndLines("getCause", List(189)))),
          MethodToken(importThrowables.toLowerCase, importThrowables, List(189, 191),
            Set(MethodAndLines("propagate", List(189, 191)))),
          MethodToken(importSFuture.toLowerCase, importSFuture, List(172, 177, 182, 187),
            Set(MethodAndLines("set", List(177)), MethodAndLines("get", List(187)),
              MethodAndLines("create", List(172)),
              MethodAndLines("setException", List(182))))), 0),

      ImportsMethods(-1, "n-a/n-a/blob/n-a/Sample.java",
        Set(MethodToken(importChannel.toLowerCase, importChannel, List(74), Set()),
          MethodToken(importPrecon.toLowerCase, importPrecon, List(74, 75),
            Set(MethodAndLines("checkNotNull", List(74, 75))))), 0),

      ImportsMethods(-1, "n-a/n-a/blob/n-a/Sample.java",
        Set(MethodToken(importSCId.toLowerCase, importSCId,
          List(120, 106, 117, 105, 108, 114), Set()),
          MethodToken(importIOE.toLowerCase, importIOE, List(123), Set()),
          MethodToken(importChannel.toLowerCase + "future", importChannel + "Future",
            List(112, 118, 119, 123),
            Set(MethodAndLines("cause", List(118, 119, 123)),
              MethodAndLines("isSuccess", List(112)))),
          MethodToken(importNettyUtils.toLowerCase, importNettyUtils, List(101),
            Set(MethodAndLines("getRemoteAddress", List(101)))),
          MethodToken(importLogger.toLowerCase, importLogger, List(103, 114, 119, 125),
            Set(MethodAndLines("trace", List(114)), MethodAndLines("debug", List(103)),
              MethodAndLines("error", List(119, 125)))),
          MethodToken(importCFReq.toLowerCase, importCFReq, List(108), Set()),
          MethodToken(importChannel.toLowerCase + "futurelistener",
            importChannel + "FutureListener", List(109), Set()),
          MethodToken(importChannel.toLowerCase, importChannel, List(101, 121, 108),
            Set(MethodAndLines("close", List(121)),
              MethodAndLines("writeAndFlush", List(108))))), 0))

    assert(methodTokens === testMethodTokens)
  }
}
