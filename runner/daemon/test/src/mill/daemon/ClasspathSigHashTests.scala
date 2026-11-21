package mill.daemon

import mill.api.{MappedRoots, PathRef}
import utest.*

object ClasspathSigHashTests extends TestSuite {
  def tests: Tests = Tests {
    test("independentOfCheckoutAndHomeLocation") {
      // A jar in the workspace and one in the home directory, e.g. a dependency.
      def hashIn(dir: os.Path, jarContents: String) = {
        val workspace = dir / "ws"
        val home = dir / "home"
        val jars = Seq(workspace / "lib/a.jar", home / ".cache/b.jar")
        for (jar <- jars) os.write.over(jar, jarContents, createFolders = true)
        MappedRoots.withMillDefaults(
          outPath = workspace / "out",
          workspacePath = workspace,
          homePath = home
        ) {
          MillBuildBootstrap.classpathSigHash(jars.map(PathRef(_)))
        }
      }
      val tmp = os.temp.dir()
      assert(hashIn(tmp / "one", "v1") == hashIn(tmp / "two", "v1"))
      // Still changes when a jar changes.
      assert(hashIn(tmp / "one", "v1") != hashIn(tmp / "one", "v2"))
    }
  }
}
