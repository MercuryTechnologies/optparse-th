{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-partial-fields -fno-warn-orphans #-}

module Main where

import Data.Char qualified as Char
import Data.List (dropWhileEnd)
import Data.Text qualified as T
import GHC.Generics
import Options.Applicative qualified as Options
import Options.Generic
import Options.TH
import Test.Hspec
import Prelude

newtype ViaGeneric a = ViaGeneric a
  deriving stock (Show, Eq)

instance (GenericParseRecord (Rep a), Generic a) => ParseRecord (ViaGeneric a) where
  parseRecord = fmap (ViaGeneric . to) (genericParseRecord defaultModifiers)

data UnitCmd = UnitCmd
  deriving stock (Show, Eq, Generic)

data PositionalCmd = PositionalCmd Int Char
  deriving stock (Show, Eq, Generic)

data RecordCmd = RecordCmd {recordFst :: Int, recordSnd :: Char}
  deriving stock (Show, Eq, Generic)

data SubCommands
  = Foo Int Char
  | Bar {barInt :: Int, barChar :: Char}
  deriving stock (Show, Eq, Generic)

data WrappedSubCommands w
  = FooW (w ::: Int <?> "help for int") (w ::: Char <?> "help for char")
  | BarW
      { barWInt :: w ::: Int <?> "help for int"
      , barWChar :: w ::: Char <?> "help for char"
      }
  deriving stock (Generic)

#if MIN_VERSION_optparse_generic(1,5,1)
#else
deriving stock instance (Eq a) => Eq (a <#> msg)

deriving stock instance (Eq a) => Eq (a <?> msg)

deriving stock instance (Eq a) => Eq (a <!> msg)
#endif

deriving stock instance Show (WrappedSubCommands Wrapped)

deriving stock instance Eq (WrappedSubCommands Wrapped)

data WrappedTest w
  = SomehowUnwrapped Int Char
  | WrappedNoMod (w ::: Int)
  | WrappedHelpful (w ::: Int <?> "wow helpful")
  | WrappedDef (w ::: Int <!> "1")
  | WrappedHelpfulAndDef (w ::: Int <!> "1" <?> "wow helpful")
  | WrappedDefAndHelpful (w ::: Int <?> "wow helpful" <!> "1")
  | WrappedAllTheThings {wrappedLabel :: w ::: Int <!> "1" <?> "wow help" <#> "i"}
  deriving stock (Generic)

deriving stock instance Show (WrappedTest Wrapped)

deriving stock instance Eq (WrappedTest Wrapped)

deriving stock instance Show (WrappedTest Unwrapped)

deriving stock instance Eq (WrappedTest Unwrapped)

fmap concat $
  traverse
    (deriveParseRecord defaultModifiers)
    [ ''UnitCmd
    , ''PositionalCmd
    , ''RecordCmd
    , ''SubCommands
    , ''WrappedSubCommands
    , ''WrappedTest
    ]

-- this function is mostly copied from optparse-generic, but exposing the
-- failure as a string
pureParseRecord :: ParseRecord a => [Text] -> IO (Either [String] a)
pureParseRecord args = pure do
  let infoMod = mempty
      prefsMod = mempty
  let header = Options.header ""
  let info = Options.info parseRecord (header <> infoMod)
  let prefs = Options.prefs (defaultParserPrefs <> prefsMod)
  let args' = map T.unpack args
  case Options.execParserPure prefs info args' of
    Options.Success a -> Right a
    Options.Failure f ->
      Left $ map (dropWhileEnd Char.isSpace) $ lines $ fst $ Options.renderFailure f "test"
    Options.CompletionInvoked _ -> Left $ pure "completion invoked???"
  where
    defaultParserPrefs =
      Options.multiSuffix "..."

spec :: Spec
spec = do
  let subjectGeneric ::
        forall a.
        (Show a, Eq a, ParseRecord a, Generic a, GenericParseRecord (Rep a)) =>
        [Text] ->
        (Either [String] a -> IO ()) ->
        IO ()
      subjectGeneric args k = do
        viaTH <- pureParseRecord args
        viaGeneric <- fmap (\(ViaGeneric a) -> a) <$> pureParseRecord args
        viaTH `shouldBe` viaGeneric
        k viaTH

  describe "UnitCmd" do
    let subject = subjectGeneric @UnitCmd

    it "works with empty args" do
      subject [] \th ->
        th `shouldBe` Right UnitCmd

    it "fails with other args" do
      subject ["asdf"] \th ->
        th
          `shouldBe` Left
            [ "Invalid argument `asdf'"
            , ""
            , "Usage: test"
            ]

  describe "PositionalCmd" do
    let subject = subjectGeneric @PositionalCmd

    it "works with proper args" do
      subject ["3", "a"] \th ->
        th `shouldBe` do
          Right $ PositionalCmd 3 'a'

    it "fails with wrong args" do
      subject [] \th ->
        th
          `shouldBe` Left
            [ "Missing: INT CHAR"
            , ""
            , "Usage: test INT CHAR"
            ]

  describe "RecordCmd" do
    let subject = subjectGeneric @RecordCmd

    it "works with proper args" do
      subject ["--recordFst", "3", "--recordSnd", "a"] \th ->
        th `shouldBe` do
          Right $ RecordCmd 3 'a'

    it "works with proper args in reverse order" do
      subject ["--recordSnd", "a", "--recordFst", "3"] \th ->
        th `shouldBe` do
          Right $ RecordCmd 3 'a'

    it "fails with wrong args" do
      subject ["3", "a"] \th ->
        th
          `shouldBe` Left
            [ "Invalid argument `3'"
            , ""
            , "Did you mean this?"
            , "    -h"
            , ""
            , "Usage: test --recordFst INT --recordSnd CHAR"
            ]

  describe "SubCommands" do
    let subject = subjectGeneric @SubCommands
    describe "Foo" do
      it "works as positional subcommand" do
        subject ["foo", "3", "a"] \th ->
          th `shouldBe` do
            Right $ Foo 3 'a'
    describe "Bar" do
      it "works as record subcommand" do
        subject ["bar", "--barInt", "3", "--barChar", "a"] \th ->
          th `shouldBe` do
            Right $ Bar 3 'a'

  describe "WrappedSubCommands" do
    let subject = subjectGeneric @(WrappedSubCommands Wrapped)

    it "has useful --help" do
      subject ["--help"] \th -> do
        th
          `shouldBe` Left
            [ "Usage: test (foow | barw)"
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "Available commands:"
            , "  foow"
            , "  barw"
            ]

    describe "FooW" do
      it "works as positional subcommand" do
        subject ["foow", "3", "a"] \th -> do
          th `shouldBe` do
            Right $ FooW (Helpful 3) (Helpful 'a')

      it "help is useful" do
        subject ["foow", "--help"] \th -> do
          th
            `shouldBe` Left
              [ "Usage: test foow INT CHAR"
              , ""
              , "Available options:"
              , "  -h,--help                Show this help text"
              , "  INT                      help for int"
              , "  CHAR                     help for char"
              ]

  describe "WrappedTest" do
    let subject = subjectGeneric @(WrappedTest Wrapped)
    it "help is useful" do
      subject ["--help"] \t ->
        t
          `shouldBe` Left
            [ "Usage: test (somehowunwrapped | wrappednomod | wrappedhelpful | wrappeddef |"
            , "              wrappedhelpfulanddef | wrappeddefandhelpful | wrappedallthethings)"
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "Available commands:"
            , "  somehowunwrapped"
            , "  wrappednomod"
            , "  wrappedhelpful"
            , "  wrappeddef"
            , "  wrappedhelpfulanddef"
            , "  wrappeddefandhelpful"
            , "  wrappedallthethings"
            ]
    describe "SomehowUnwrapped" do
      it "works as usual" do
        subject ["somehowunwrapped", "3", "a"] \t ->
          t `shouldBe` Right do
            SomehowUnwrapped 3 'a'
    describe "WrappedNoMod" do
      it "works as usual" do
        subject ["wrappednomod", "3"] \t ->
          t `shouldBe` Right do
            WrappedNoMod 3

    describe "WrappedHelpful" do
      it "parses correctly" do
        subject ["wrappedhelpful", "3"] \t ->
          t `shouldBe` Right do
            WrappedHelpful (Helpful 3)

      it "can be unwrapped" do
        subject ["wrappedhelpful", "3"] \t -> do
          fmap unwrapRecordWrappedTest t `shouldBe` Right do
            WrappedHelpful 3
          fmap unwrap t `shouldBe` Right do
            WrappedHelpful 3

      it "has useful help" do
        subject ["wrappedhelpful", "--help"] \t ->
          t
            `shouldBe` Left
              [ "Usage: test wrappedhelpful INT"
              , ""
              , "Available options:"
              , "  -h,--help                Show this help text"
              , "  INT                      wow helpful"
              ]

    describe "WrappedAllTheThings" do
      it "parses with long name" do
        subject ["wrappedallthethings", "--wrappedLabel", "1"] \t ->
          fmap unwrapRecordWrappedTest t `shouldBe` Right do
            WrappedAllTheThings 1

      it "parses with short name" do
        subject ["wrappedallthethings", "-i", "1"] \t ->
          fmap unwrapRecordWrappedTest t `shouldBe` Right do
            WrappedAllTheThings 1

      it "parses without option as default" do
        subject ["wrappedallthethings"] \t ->
          fmap unwrapRecordWrappedTest t `shouldBe` Right do
            WrappedAllTheThings 1

      it "has useful help" do
        subject ["wrappedallthethings", "--help"] \t ->
          fmap unwrapRecordWrappedTest t
            `shouldBe` Left
              [ "Usage: test wrappedallthethings [-i|--wrappedLabel INT]"
              , ""
              , "Available options:"
              , "  -h,--help                Show this help text"
              , "  -i,--wrappedLabel INT    wow help (default: 1)"
              ]

main :: IO ()
main = hspec spec
