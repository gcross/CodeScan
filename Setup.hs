-- @+leo-ver=4-thin
-- @+node:gcross.20091130193227.1575:@thin Setup.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091130193227.1576:<< Language extensions >>
{-# LANGUAGE PackageImports #-}
-- @-node:gcross.20091130193227.1576:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20091130193227.1577:<< Import needed modules >>
import Control.Applicative
import Control.Applicative.Infix
import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Parallel

import Data.ConfigFile hiding (options)
import Data.Either.Unwrap
import Data.Maybe
import qualified Data.Map as Map
import Data.Version

import Distribution.Package
import qualified Distribution.PackageDescription as Package

import System.Directory
import System.FilePath
import System.IO.Unsafe

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>),(</>))

import Blueprint.Configuration
import Blueprint.Error
import Blueprint.Main
import Blueprint.Miscellaneous
import Blueprint.Options
import Blueprint.Resources
import Blueprint.Tools.Ar
import Blueprint.Tools.GCC
import Blueprint.Tools.GFortran
import Blueprint.Tools.GHC
import Blueprint.Tools.Installer
import Blueprint.Tools.Ld
-- @-node:gcross.20091130193227.1577:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091130193227.1578:Options
options =
    [   ghcOptions
    ,   gccOptions
    ]
-- @-node:gcross.20091130193227.1578:Options
-- @+node:gcross.20091130193227.1579:Flags
ghc_flags = ["-O2","-fvia-C","-optc=-O3"]

gcc_flags = ["-O3","-ffast-math","-funroll-loops"]
-- @-node:gcross.20091130193227.1579:Flags
-- @+node:gcross.20091130193227.1580:Types
-- @+node:gcross.20091130193227.1581:Configuration
data Configuration = Configuration
    {   ghcConfiguration :: GHCConfiguration
    ,   gccConfiguration :: GCCConfiguration
    ,   packageDependencies :: [String]
    }
-- @-node:gcross.20091130193227.1581:Configuration
-- @-node:gcross.20091130193227.1580:Types
-- @+node:gcross.20091130193227.1582:Values
-- @+node:gcross.20091130193227.1583:source resources
source_resources = resourcesIn "sources"
-- @-node:gcross.20091130193227.1583:source resources
-- @+node:gcross.20091130193227.1584:package description
package_description = readPackageDescription "CodeScan.cabal"
-- @-node:gcross.20091130193227.1584:package description
-- @+node:gcross.20091130193227.1585:configuration file path
configurationFilePath = "CodeScan.cfg"
-- @-node:gcross.20091130193227.1585:configuration file path
-- @+node:gcross.20091130193227.1586:qualified package name
qualified_package_name =
    let PackageIdentifier (PackageName name) version = Package.package package_description
    in name ++ "-" ++ showVersion version
-- @-node:gcross.20091130193227.1586:qualified package name
-- @-node:gcross.20091130193227.1582:Values
-- @+node:gcross.20091130193227.1587:Targets
targets =
    [target "configure" configure
    ,target "reconfigure" $ makeReconfigureTarget configurationFilePath targets
    ,target "build" build
    ,target "rebuild" $ makeRebuildTarget targets
    ,target "clean" $
        makeCleanTarget
            ["objects"
            ,"digest-cache"
            ,"haskell-interfaces"
            ,"libraries"
            ,"programs"
            ]
    ,target "distclean" $
        makeDistCleanTarget
            [configurationFilePath
            ]
            targets
    ]
-- @+node:gcross.20091130193227.1588:configure
configure :: Either ErrorMessage Configuration
configure = parseCommandLineOptions options >>= \(_,options) -> runConfigurer configurationFilePath options $ do
    configurations@
        (ghc_configuration
        ,gcc_configuration
        ) <- (,)
            <$> (configureUsingSection "GHC")
            <*> (configureUsingSection "GCC")
    package_dependencies <- configurePackageResolutions ghc_configuration package_description "GHC"
    return $
        Configuration
            ghc_configuration
            gcc_configuration
            package_dependencies
-- @-node:gcross.20091130193227.1588:configure
-- @+node:gcross.20091130193227.1589:build
build = configure >>= \configuration ->
    let Right package_modules = getPackages <$> ghcConfiguration <*> packageDependencies $ configuration
        compiled_resources = 
            gccCompileAll
                (gccConfiguration configuration)
                gcc_flags
                "objects"
                "digest-cache"
            .
            ghcCompileAll
                (ghcConfiguration configuration)
                ghc_flags
                package_modules
                "objects"
                "haskell-interfaces"
                "digest-cache"
            $
            source_resources
        haskell_program_names =
            map fst
            .
            filter (uncurry (&&) . (isDotFree *** ((==) "hs")))
            .
            Map.keys
            $
            compiled_resources

        haskell_program_objects =
            map (fromJust . flip Map.lookup compiled_resources . flip (,) "o") haskell_program_names

        haskell_program_resources = flip map haskell_program_objects $ \program_object ->
            let program_name = resourceName program_object
                dependent_object_resources = findAllObjectDependenciesOf compiled_resources program_object
                object_resources =
                    if program_name == "ScanCodes"
                        then fromJust (Map.lookup ("codescan","o") compiled_resources):dependent_object_resources
                        else dependent_object_resources
                flags =
                    if program_name == "ScanCodes"
                        then "-lstdc++":"-lpqxx":"-luuid++":ghc_flags
                        else ghc_flags
            in ghcLinkProgram
                (ghcConfiguration configuration)
                flags
                "digest-cache"
                (packageDependencies configuration)
                object_resources
                program_name
                ("programs" </> program_name)
    in attemptGetDigests $ haskell_program_resources
-- @-node:gcross.20091130193227.1589:build
-- @-node:gcross.20091130193227.1587:Targets
-- @+node:gcross.20091130193227.1591:main
main = defaultMain
        (createDefaultHelpMessage options . map fst $ targets)
        targets
-- @-node:gcross.20091130193227.1591:main
-- @-others
-- @-node:gcross.20091130193227.1575:@thin Setup.hs
-- @-leo
