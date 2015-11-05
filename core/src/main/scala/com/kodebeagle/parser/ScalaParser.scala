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

import java.util.regex.Pattern

import com.kodebeagle.indexer.{HighLighter, ImportsMethods, MethodToken, Repository}
import com.kodebeagle.logging.Logger
import org.scalastyle.{CheckerUtils, ScalariformAst}

import scala.collection.immutable.Iterable
import scala.util.Try
import scalariform.lexer.Tokens

object ScalaParser extends Logger {

  val importPattern: Pattern = Pattern.compile("import (.*)\\.(\\w+)")

  def extractImports(java: String, packages: List[String]): Set[(String, String)] = java.split("\n")
    .filter(x => x.trim.startsWith("import") && !x.trim.endsWith("._")) // ignore wild char imports
    .map(x => importPattern.matcher(x)).filter(_.find)
    .flatMap(x => Try((x.group(1), x.group(2).trim)).toOption)
    .filterNot { case (left, right) => packages.contains(left) }.toSet

  /**
   * Takes a line of code and cleans it for further indexing.
   */
  def cleanUpCode(line: String): String = {
    val cleaned =
      line.replaceFirst("""(\s*(import|private|public|protected|\/?\*|//).*)|\".*\"""", "")
        .replaceAll("\\W+", " ")
    if (!cleaned.isEmpty) " " + cleaned.toLowerCase + " " else ""
  }

  def tuple2ToImportString(importName: (String, String)): String = {
    importName._1 + "." + importName._2
  }

  // This is simplest and considers the whole file a single context in KodeBeagle sense.
  def generateTokens(files: Map[String, String], excludePackages: List[String],
                     repo: Option[Repository]): Set[ImportsMethods] = {
    files.map { case (fileName, fileContent) =>
      val imports: Set[(String, String)] = extractImports(fileContent, excludePackages)
      val lines: Array[String] = fileContent.split("\n")
      val tokens: Array[MethodToken] =
        (lines zip (1 to lines.length)).flatMap { case (line, lineNumber) =>
          val cleaned = cleanUpCode(line)
          val l = cleaned.split("\\s+").distinct
          val importVsLineNumber: Map[String, Set[Int]] =
            imports.map { case y@(importLeft, importRight) =>
              (l.contains(importRight.toLowerCase), lineNumber, y)
            }.filter { case (isMatched, _, _) => true }
              .map { case (_, lineNum, importPair) => (tuple2ToImportString(importPair), lineNum) }
              .groupBy { case (importName, lineNum) => importName }
              .map { case (importName, lineNumSet) => importName -> lineNumSet.map(x1 => x1._2) }

          val methodTokens: Iterable[MethodToken] =
            importVsLineNumber.map { case (importName, lineNumSet) =>
              MethodToken(importName.toLowerCase, importName,
                lineNumSet.map(x => HighLighter(x, 0, line.length)).toList,
                Set())
            }

          methodTokens
        }
      val repository: Repository = repo.getOrElse(Repository.invalid)
      ImportsMethods(repository.id, fileName, tokens.toSet[MethodToken], repository.stargazersCount)
    }.toSet
  }

  /**
   * Each block of code captured by curly braces is considered one block of context.
   * KodeBeagle is contextual search and context for most languages indented by a curly braces
   * turn out to be easy to detect. So this method is very generic in that sense. But trades
   * precision, which can be upgraded by bringing in more robust AST parser for scala.
   */
  def generateTokensWithBraceContext(files: Map[String, String], excludePackages: List[String],
                                     repo: Option[Repository]): Set[ImportsMethods] = {
    log.info("excluded:" + excludePackages.mkString("~") + "#")
    files.flatMap { case (fileName, fileContent) =>
      val imports: Set[(String, String)] = extractImports(fileContent, excludePackages)
      log.info("imports:" + imports.mkString("~") + "#")
      val lines: Array[String] = fileContent.split("\n")
      // This is inefficient to run but convenient to express.
      val fileContentWithLineNums = (for ((l, n) <- lines zip (1 to lines.length)) yield
      n + "~" + l).reduce(_ + "\n" + _)
      val codeBlocks: Option[List[String]] = GenericBraceMatchingParser(fileContentWithLineNums)
      // TODO: This sort of stitching of numbers and separating them again was needed because of
      // Parser being dumb.

      if(codeBlocks.isDefined) { codeBlocks.get.map { codeBlock =>
        val tokens: Array[MethodToken] = codeBlock.split("\n").map { lineWithLineNumber =>
          val (lineNum, line) = lineWithLineNumber.splitAt(lineWithLineNumber.indexOf("~"))
          (line, lineNum)
        }.flatMap { case (line, lineNumber) =>
          if (Try(lineNumber.toInt).isFailure) {
            Set[MethodToken]()
          } else {
            val cleaned = cleanUpCode(line)
            val l = cleaned.split("\\s+").distinct
            val importVsLineNumber: Map[String, Set[Int]] =
              imports.map { case y@(importLeft, importRight) =>
                (l.contains(importRight.toLowerCase), lineNumber.toInt, y)
              }.filter { _._1} // Whether import name matched or not.
                .map { case (_, lineNum, importPair) => (tuple2ToImportString(importPair), lineNum)}
                .groupBy { case (importName, lineNum) => importName}
                .map { case (importName, lineNumSet) => importName -> lineNumSet.map(x1 => x1._2)}

            val methodTokens: Iterable[MethodToken] =
              importVsLineNumber.map { case (importName, lineNumSet) =>
                MethodToken(importName.toLowerCase, importName,
                  lineNumSet.map(x => HighLighter(x, 0, line.length)).toList,
                  Set())
              }
            methodTokens
          }
        }

        val repository: Repository = repo.getOrElse(Repository.invalid)
        ImportsMethods(repository.id, fileName, tokens.toSet[MethodToken],
          repository.stargazersCount)
      }
      } else {Set()}

    }.toSet // TODO: These toSet are inefficient, work on them later.
  }

  // It is WIP, Offsets have to be captured from starting def to ending '}'
  // I just felt it is easier to write a parser myself. Here goes `GenericBraceMatchingParser`.
  def parse(s: String): Unit = {
    val scalariform: Option[ScalariformAst] = new CheckerUtils().parseScalariform(s)
    scalariform.map {
      x =>
        x.ast.tokens.filter(_.tokenType == Tokens.DEF).map(_.text)
    }
  }
}
