-- | Hook BNFC into the cabal build process to generate AST, lexer, parser, and printer definitions.

import Distribution.Simple            (defaultMainWithHooks, simpleUserHooks, buildHook)
import Distribution.Simple.BuildPaths (autogenPackageModulesDir)
import System.Process                 (callProcess)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \ packageDescription localBuildInfo userHooks buildFlags -> do
      -- For simplicity, generate files in build/global-autogen;
      -- there they are available to all components of the package.
      callProcess "bnfc"
        [ "-o", autogenPackageModulesDir localBuildInfo
        , "-d"
        , "src/LBNF.cf"
        ]
      -- Run the build process.
      buildHook simpleUserHooks packageDescription localBuildInfo userHooks buildFlags
  }
