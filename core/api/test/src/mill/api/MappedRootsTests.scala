package mill.api

import utest.*

import java.nio.file.Files
import mill.api.{MappedRoots => MR}

object MappedRootsTests extends TestSuite {
  val tests: Tests = Tests {
    test("encode") {
      withTmpDir { tmpDir =>
        val workspaceDir = tmpDir / "workspace"
        val outDir = workspaceDir / "out"
        MR.withMillDefaults(outPath = outDir, workspacePath = workspaceDir) {

          def check(path: os.Path, encContains: Seq[String], containsNot: Seq[String]) = {
            val enc = MR.encodeKnownRootsInPath(path)
            val dec = MR.decodeKnownRootsInPath(enc)
            assert(path.toString == dec)
            encContains.foreach(s => assert(enc.containsSlice(s)))
            containsNot.foreach(s => assert(!enc.containsSlice(s)))

            path -> enc
          }

          val file1 = tmpDir / "file1"
          val file2 = workspaceDir / "file2"
          val file3 = outDir / "file3"

          Seq(
            "mapping" -> MR.get,
            check(file1, Seq(file1.toString), Seq("$WORKSPACE", "$MILL_OUT")),
            check(file2, Seq("$WORKSPACE/file2"), Seq("$MILL_OUT")),
            check(file3, Seq("$MILL_OUT/file3"), Seq("$WORKSPACE"))
          )
        }
      }
    }

    test("coursierCache") {
      withTmpDir { tmpDir =>
        MR.withMillDefaults(outPath = tmpDir / "out", workspacePath = tmpDir) {
          val jar = coursierCache / "https" / "repo1.maven.org" / "a.jar"
          val enc = MR.encodeKnownRootsInPath(jar)
          // Wherever this OS or `COURSIER_CACHE` puts it, not as a path under `$HOME`.
          assert(enc == "$COURSIER_CACHE/https/repo1.maven.org/a.jar")
          assert(MR.decodeKnownRootsInPath(enc) == jar.toString)
          val jdk = archiveCache / "https" / "jdk.tar.gz" / "jdk"
          val encJdk = MR.encodeKnownRootsInPath(jdk)
          assert(encJdk == "$COURSIER_ARCHIVE_CACHE/https/jdk.tar.gz/jdk")
          assert(MR.decodeKnownRootsInPath(encJdk) == jdk.toString)
        }
      }
    }

    test("mostSpecificRootWins") {
      withTmpDir { tmpDir =>
        // A home folder inside the workspace, and an out folder outside both.
        val ws = tmpDir / "ws"
        MR.withMillDefaults(outPath = tmpDir / "out", workspacePath = ws, homePath = ws / "home") {
          assert(MR.encodeKnownRootsInPath(ws / "home" / "a") == "$HOME/a")
          assert(MR.encodeKnownRootsInPath(ws / "b") == "$WORKSPACE/b")
          assert(MR.encodeKnownRootsInPath(tmpDir / "out" / "c") == "$MILL_OUT/c")
        }
      }
    }
  }

  def coursierCache: os.Path =
    os.Path(coursier.paths.CoursierPaths.cacheDirectoryPath().toAbsolutePath)
  def archiveCache: os.Path =
    os.Path(coursier.paths.CoursierPaths.archiveCacheDirectoryPath().toAbsolutePath)

  private def withTmpDir[T](body: os.Path => T): T = {
    val tmpDir = os.Path(Files.createTempDirectory(""))
    val res = body(tmpDir)
    os.remove.all(tmpDir)
    res
  }

}
