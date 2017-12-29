import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.InstallDirs as I
import Distribution.Simple.LocalBuildInfo as L
import qualified Distribution.Simple.Setup as S
import qualified Distribution.Simple.Program as P
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose, rewriteFile, notice, installOrdinaryFiles)
import Distribution.Compiler
import Distribution.PackageDescription
import Distribution.Text

import System.Environment
import System.Exit
import System.FilePath ((</>), splitDirectories,isAbsolute)
import System.Directory
import qualified System.FilePath.Posix as Px
import System.Process

main = defaultMain
