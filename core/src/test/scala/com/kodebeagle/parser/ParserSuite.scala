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

package com.kodebeagle.parser

import java.io.{InputStream, StringWriter}

import com.kodebeagle.indexer.{HighLighter, Repository}
import org.apache.commons.io.IOUtils
import org.scalatest.{BeforeAndAfterAll, FunSuite}

import scala.collection.mutable

class ParserSuite extends FunSuite with BeforeAndAfterAll {

  test("Simple repo") {
    val r = RepoFileNameParser("repo~apache~zookeeper~160999~false~Java~trunk~789.zip")
    assert(r.contains(Repository("apache", 160999, "zookeeper", false, "Java", "trunk", 789)))
  }

  test("Names with special character") {
    val r = RepoFileNameParser("/home/dir~temp/repo~apache~zookeeper-lost~160999~false~Java" +
      "~=+-trunk/2.1~789.zip")
    assert(r.contains(Repository("apache", 160999, "zookeeper-lost", false, "Java", "=+-trunk/2.1",
      789)))
  }

  test("Branch name with version number only") {
    val r = RepoFileNameParser("repo~apache~zookeeper~160999~false~Java~2.1~789.zip")
    assert(r.contains(Repository("apache", 160999, "zookeeper", false, "Java", "2.1", 789)))
  }


  test("Branch name with tilde in it.") {
    val r = RepoFileNameParser("repo~apache~zookee~per~160999~false~Java~2.~1~789.zip")
    assert(r.isEmpty)
  }

  test("Branch name absent.") {
    val r = RepoFileNameParser("repo~apache~zookeeper~160999~false~Java~789.zip")
    assert(r.contains(Repository("apache", 160999, "zookeeper", false, "Java", "master", 789)))
  }

  test("Hdfs url.") {
    val r = RepoFileNameParser(
      "/172.16.13.179:9000/user/data/github3/repo~apache~zookeeper~160999~false~Java~789.zip")
    assert(r.contains(Repository("apache", 160999, "zookeeper", false, "Java", "master", 789)))
  }

  test("Multiple valid repo names.") {
    val stream =
      Thread.currentThread().getContextClassLoader.getResourceAsStream("repo_names")
    val writer = new StringWriter()
    IOUtils.copy(stream, writer)
    val repoNames = writer.toString.split("\n").map { x =>
      (RepoFileNameParser(x), x)
    }
    assert(repoNames.filter(p = x => x._1.isEmpty) === Seq())
  }
}

class MethodVisitorSuite extends FunSuite with BeforeAndAfterAll {

  import scala.collection.JavaConversions._

  val stream: InputStream =
    Thread.currentThread.getContextClassLoader.getResourceAsStream("TransportClient.java")

  val parser: MethodVisitor = new MethodVisitor
  parser.parse(stream)

  test("Verify method visitor includes lines with usages.") {

    val lines = parser.getListOflineNumbersMap.flatMap(x => x.flatMap(_._2)).map(_.toInt)
      .toList.sorted.distinct

    assert(lines === List(74, 75, 78, 79, 101, 102, 103, 105, 106, 108, 109, 112, 113, 114, 115,
      117, 118, 119, 120, 121, 123, 124, 125, 137, 138, 139, 141, 142, 144, 145, 148, 149, 150, 152,
      153, 154, 155, 156, 158, 159, 160, 172, 174, 177, 182, 187, 188, 189, 190, 191, 198, 203,
      204))
  }

  test("Should return a Map of import with methods and lineNumbers") {
    import java.io.InputStream
    val stream: InputStream =
      Thread.currentThread.getContextClassLoader.getResourceAsStream("TransportClient.java")

    import com.kodebeagle.parser.MethodVisitorHelper._

    val testMap = List(Map("io.netty.channel.Channel" -> Map(),
      "com.google.common.base.Preconditions" -> Map("checkNotNull" ->
        List(HighLighter(74, 20, 54), HighLighter(75, 20, 54)))),

      Map("io.netty.channel.Channel" -> Map("isOpen" -> List(HighLighter(79, 12, 27)),
        "isActive" -> List(HighLighter(79, 32, 49))),
        "javax.xml.bind.annotation.XmlAnyAttribute" -> Map()),

      Map("org.slf4j.Logger" -> Map("trace" -> List(HighLighter(114, 13, 24)),
        "debug" -> List(HighLighter(103, 5, 80)),
        "error" -> List(HighLighter(119, 13, 50), HighLighter(125, 15, 85))),
        "org.apache.spark.network.protocol.ChunkFetchRequest" -> Map(),
        "io.netty.channel.ChannelFutureListener" -> Map(), "java.io.IOException" -> Map(),
        "org.apache.spark.network.util.NettyUtils" -> Map("getRemoteAddress" ->
          List(HighLighter(101, 31, 66))), "io.netty.channel.ChannelFuture" ->
          Map("cause" -> List(HighLighter(118, 27, 40), HighLighter(119, 36, 49),
            HighLighter(123, 72, 85)), "isSuccess" -> List(HighLighter(112, 15, 32))),
        "io.netty.channel.Channel" -> Map("close" -> List(HighLighter(121, 13, 27)),
          "writeAndFlush" -> List(HighLighter(108, 5, 63))),
        "org.apache.spark.network.protocol.StreamChunkId" -> Map()),

      Map("org.slf4j.Logger" -> Map("trace" -> List(HighLighter(139, 5, 49),
        HighLighter(150, 13, 97)),
        "error" -> List(HighLighter(154, 13, 50), HighLighter(160, 15, 85))),
        "io.netty.channel.ChannelFutureListener" -> Map(),
        "java.io.IOException" -> Map(), "java.util.UUID" ->
          Map("randomUUID" -> List(HighLighter(141, 37, 53))),
        "org.apache.spark.network.util.NettyUtils" ->
          Map("getRemoteAddress" -> List(HighLighter(137, 31, 66))),
        "io.netty.channel.ChannelFuture" -> Map("cause" -> List(HighLighter(153, 27, 40),
          HighLighter(154, 36, 49), HighLighter(158, 60, 73)), "isSuccess" ->
          List(HighLighter(148, 15, 32))), "io.netty.channel.Channel" -> Map("close" ->
          List(HighLighter(156, 13, 27)), "writeAndFlush" -> List(HighLighter(144, 5, 61))),
        "org.apache.spark.network.protocol.RpcRequest" -> Map()),

      Map("java.util.concurrent.TimeUnit" -> Map(), "java.util.concurrent.ExecutionException" ->
        Map("getCause" -> List(HighLighter(189, 34, 45))), "com.google.common.base.Throwables" ->
        Map("propagate" -> List(HighLighter(189, 13, 46), HighLighter(191, 13, 35))),
        "com.google.common.util.concurrent.SettableFuture" -> Map("set" ->
          List(HighLighter(177, 9, 28)), "get" -> List(HighLighter(187, 14, 57)),
          "create" -> List(HighLighter(172, 43, 65)),
          "setException" -> List(HighLighter(182, 9, 30)))),

      Map("java.util.concurrent.TimeUnit" -> Map(), "io.netty.channel.Channel" ->
        Map("close" -> List(HighLighter(198, 5, 19)))),

      Map("io.netty.channel.Channel" -> Map("remoteAddress" -> List(HighLighter(204, 28, 50))),
        "com.google.common.base.Objects" -> Map("toStringHelper" -> List(HighLighter(203, 12, 39))))
    )

    val imports = getImports(parser, Set()).map(importName => importName._1 + "." + importName._2)
    val tokenMap: List[Map[String, List[HighLighter]]] = getTokenMap(parser, imports)
    val resultTokens = getImportsWithMethodAndLineNumbers(parser, tokenMap).map(_._1)

    for (i <- resultTokens.indices) {
      val resultToken = resultTokens(i)
      val testToken = testMap(i)
      assert(resultToken.size === testToken.size)
      assert((resultToken.keySet diff testToken.keySet) === Set())
      assert((testToken.keySet diff resultToken.keySet) === Set())
      resultToken.foreach {
        case (k, v) =>
          assert(resultToken(k) === testToken(k))
      }
    }
  }

  override def afterAll() {
    stream.close()
  }
}

class GenericParserTest extends FunSuite {


  test("With dummy scala file") {
    val parsed: Option[List[String]] = GenericBraceMatchingParser(
      """
        |import abc.xyz
        |import abc.xyz;
        |
        |object Test {
        |  val g = 1
        |  def fun = {
        |   // some content.
        |   val i = 10
        |   }
        |}
        |class A {
        | val a = 1000
        |}
        |
      """.stripMargin)
    assert(parsed.isDefined)
    assert(parsed.get.exists(_.contains("val i = 10")))
    assert(parsed.get.exists(_.contains("val a = 1000")))
  }

  test("Should fail for unmatched curly brackets.") {
    val parsed: Option[List[String]] = GenericBraceMatchingParser(
      """
        |import abc.xyz
        |import abc.xyz;
        |
        |object Test {
        |  val g = 1
        |  def fun = {
        |   // some content.
        |   val i = 10
        |
        |}
        |class A {
        | val a = 1000
        |}
        |
      """.stripMargin)
    assert(parsed.isEmpty)
  }

  test("With real java file") {
    val stream: InputStream =
      Thread.currentThread.getContextClassLoader.getResourceAsStream("TransportClient.java")

    import scala.collection.JavaConversions._

    val lines: String = IOUtils.readLines(stream).reduce(_ + _)
    val parsed: Option[List[String]] = GenericBraceMatchingParser(lines)
    assert(parsed.isDefined)
  }

  test("With real js file") {
    val stream: InputStream =
      Thread.currentThread.getContextClassLoader.getResourceAsStream("background.js")

    import scala.collection.JavaConversions._

    val lines: String = IOUtils.readLines(stream).reduce(_ + _)
    val parsed: Option[List[String]] = GenericBraceMatchingParser(lines)
    assert(parsed.isDefined)
  }
}

class ScalaParserTest extends FunSuite {

  import ScalaParser._

  test("") {
    parse(
      """
        |import abc.xyz
        |import abc.xyz;
        |
        |object Test {
        |
        |  def fun = {
        |   // some content.
        |   val i = 10
        |  }
        |}
        |
      """.stripMargin)
  }
}

class ZipParserTest extends FunSuite {

  import java.util.zip.ZipInputStream

  import com.kodebeagle.crawler.ZipBasicParser._
  import com.kodebeagle.indexer.Statistics

  test("read zip file correctly") {
    val stream: InputStream = Thread.currentThread.getContextClassLoader.getResourceAsStream(
      "repo~Cascading~cascading-dbmigrate~576623~false~Java~master~65.zip")
    val (_, _, _, statistics) = readFilesAndPackages(576623, new ZipInputStream(stream))
    assert(statistics === Statistics(576623, 843, 8, 33))
  }
}
