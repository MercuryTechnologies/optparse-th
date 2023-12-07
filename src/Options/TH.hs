{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

-- | This module is designed to provide a @TemplateHaskell@ alternative to
-- "Options.Generic".
module Options.TH
  ( deriveParseRecord,
    module Options.Generic,
  )
where

import Data.Foldable (asum)
import Control.Applicative
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as T
import Data.Traversable
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Options.Applicative qualified as Options
import Options.Generic

-- | This function derives 'ParseRecord' for you without incurring a 'Generic'
-- dependency.
--
-- The main barrier here to fully supporting the library is that the API for
-- 'ParseRecord' does not expose the function that provides modifiers by
-- default. So we can provide an instance of 'ParseRecord', but we can't provide
-- a replacement of 'parseRecordWithModifiers', because that function is defined
-- as a top-level that delegates directly to the generic.
--
-- @
-- parseRecordWithModifiers
--     :: (Generic a, GenericParseRecord (Rep a))
--     => Modifiers
--     -> Parser
-- parseRecordWithModifiers modifiers =
--     fmap GHC.Generics.to (genericParseRecord modifiers)
-- @
--
-- This means that we need to shift the options to the compile-time site,
-- instead of the runtime site.
--
-- Likewise, we cannot provide an instance of 'Unwrappable', because it's not
-- a class - it's a type alias for 'Generic' stuff. So we need to create
-- a separate top-level function that does the unwrap.
--
-- @since 0.1.0.0
deriveParseRecord :: Modifiers -> Name -> Q [Dec]
deriveParseRecord modifiers tyName = do
  tyInfo <- reify tyName

  datatype <-
    getDatatypeForInfo tyName tyInfo


  liftA2 (<>) (datatypeToInstanceDec modifiers datatype) (datatypeToUnwrapRecordDec tyName datatype)

datatypeToUnwrapRecordDec :: Name -> Datatype -> Q [Dec]
datatypeToUnwrapRecordDec typeName datatype = do
  if datatypeIsWrapped datatype
    then do
      let fnName =
            mkName ("unwrapRecord" <> nameBase typeName)
          fnType =
            [t|$(conT typeName) Wrapped -> $(conT typeName) Unwrapped|]
          fnExpr = mkUnwrapRecordExpr datatype
          fnSig = SigD fnName <$> fnType
      (:)
        <$> fnSig
        <*> [d|
          $(varP fnName) = $(fnExpr)
          |]
    else do
      pure []

mkUnwrapRecordExpr :: Datatype -> Q Exp
mkUnwrapRecordExpr datatype = do
  let mkMatch con =
        case con of
          NormalC name bangTyps -> do
            (pat, namesAndTypes) <- do
              namesAndTypes <-
                for bangTyps \(_bang, typ) -> do
                  n <- newName "x"
                  pure (n, typ)

              pure
                ( mkConP name (map (\(varName, _fieldType) -> VarP varName) namesAndTypes)
                , namesAndTypes
                )
            body <- do
              let constr = ConE name
              fields <- traverse unwrapFields namesAndTypes
              pure $ NormalB $ foldl' AppE constr fields
            let decs =
                  []
            pure $
              Match pat body decs
          RecC name varBangTypes -> do
            (pat, varNames) <- do
              varNames <-
                for varBangTypes \(_fieldName, _bang, typ) -> do
                  n <- newName "x"
                  pure (n, typ)

              pure
                ( mkConP name (map (\(varName, _fieldType) -> VarP varName) varNames)
                , varNames
                )
            body <- do
              let constr = ConE name
              fields <- traverse unwrapFields varNames
              pure $ NormalB $ foldl' AppE constr fields
            let decs =
                  []
            pure $
              Match pat body decs
          _ ->
            fail $
              unlines
                [ "Unexpected constructor in mkUnwrapRecordExpr: "
                , "  " <> show con
                ]

  matches <-
    traverse mkMatch (datatypeConstructors datatype)

  pure $ LamCaseE (NonEmpty.toList matches)

-- | Test the type of the field. If it is unwrappable, unwrap it until it isn't.
unwrapFields :: (Name, Type) -> Q Exp
unwrapFields (varName, varTyp) =
  case varTyp of
    AppT
      ( AppT
          (ConT ((== ''(Options.Generic.:::)) -> True))
          (VarT _)
        )
      rest -> do
        tryUnwrapping varName rest
    _ ->
      varE varName

-- | The goal of this function is to test to see if the constructor is
-- unwrappable: that is, one of <?>, <!>, or <#>.
--
-- If it is unwrappable, then we call the relevant function. Note that we have
-- to try multiple times, since you can put a wrapper in any order.
tryUnwrapping :: Name -> Type -> Q Exp
tryUnwrapping varName = go []
  where
    go fns varTyp = do
      case varTyp of
        ( AppT
            (ConT ((== ''(Options.Generic.<?>)) -> True))
            rest
          )
          `AppT` _helpText ->
            do
              go (VarE 'unHelpful : fns) rest
        AppT
          ( AppT
              (ConT ((== ''(Options.Generic.<!>)) -> True))
              rest
            )
          _defVal ->
            do
              go (VarE 'unDefValue : fns) rest
        AppT
          ( AppT
              (ConT ((== ''(Options.Generic.<#>)) -> True))
              rest
            )
          _shortLabel ->
            go (VarE 'unShortName : fns) rest
        _ -> do
          foldr (\fn acc -> pure fn `appE` acc) (varE varName) fns

getDatatypeForInfo :: Name -> Info -> Q Datatype
getDatatypeForInfo tyName tyInfo =
  case tyInfo of
    TyConI dec ->
      case dec of
        DataD _xct name bndrs _mkind constructors _derivs -> do
          case constructors of
            [] ->
              fail $
                unlines
                  [ "A `ParseRecord` instance can't be generated for the following type:"
                  , "  " <> show tyName
                  , "... because it has no constructors."
                  ]
            (c : cs) -> do
              pure
                Datatype
                  { datatypeName =
                      name
                  , datatypeConstructors =
                      c :| cs
                  , datatypeIsWrapped =
                      not (null bndrs)
                  }
        NewtypeD _cxt name bndrs _mkind constructor _derivs -> do
          pure
            Datatype
              { datatypeName =
                  name
              , datatypeConstructors =
                  pure constructor
              , datatypeIsWrapped =
                  not (null bndrs)
              }
        _ ->
          fail $
            unlines
              [ "Internal error: Options.TH.getDatatypeForInfo."
              , ""
              , "This is not your fault. Open a bug report and include the following error for the context in the report: " <> show dec
              , "The GHC API provided a `TyConI` wrapping a declaration that was not a `data` or `newtype` declaration, which should never happen."
              ]
    _ -> do
      fail $
        mconcat
          [ "Expected a type constructor in 'deriveParseRecord', got: "
          , "\n\t"
          , show tyInfo
          ]

data Datatype = Datatype
  { datatypeName :: Name
  , datatypeIsWrapped :: Bool
  , datatypeConstructors :: NonEmpty Con
  }

datatypeToInstanceDec :: Modifiers -> Datatype -> Q [Dec]
datatypeToInstanceDec mods Datatype {..} = do
  let saturatedType =
        if datatypeIsWrapped
          then ConT datatypeName `AppT` ConT ''Wrapped
          else ConT datatypeName

  parseRecordExpr <-
    case datatypeConstructors of
      singleConstructor :| [] -> do
        makeSingleCommand mods singleConstructor
      subcommands -> do
        [|asum|] `appE` listE (NonEmpty.toList (fmap (makeSubcommand mods) subcommands))

  [d|
    instance ParseRecord $(pure saturatedType) where
      parseRecord =
        Options.helper <*> $(pure parseRecordExpr)
    |]

-- | This function should be called on a single constructor. No subcommand
-- should be created.
makeSingleCommand :: Modifiers -> Con -> Q Exp
makeSingleCommand Modifiers {..} con = do
  case con of
    NormalC conName bangTypes -> do
      -- In this case, we want to create a parser that parses the arguments as
      -- positional arguments.
      baseCase <- [e|pure $(conE conName)|]

      let apps expr (_bang, _type) = do
            let label = Nothing @Text
                shortName = Nothing @Char
            infixE (Just expr) (varE '(<*>)) (Just [e|parseFields Nothing $(lift label) $(lift shortName) Nothing|])

      foldl' apps (pure baseCase) bangTypes
    RecC conName varBangTypes -> do
      -- In this case, we want to create a parser that will use the field names.
      baseCase <- [e|pure $(conE conName)|]
      let apps expr (fieldName, _bang, _type) = do
            let fieldNameString =
                  nameBase fieldName
                label =
                  Just . T.pack . fieldNameModifier $ fieldNameString
                shortName =
                  shortNameModifier fieldNameString
            infixE (Just expr) (varE '(<*>)) (Just [e|parseFields Nothing $(lift label) $(lift shortName) Nothing|])

      foldl' apps (pure baseCase) varBangTypes
    _ ->
      fail $
        unlines
          [ "Expected either a normal or record constructor, got: "
          , "\t" <> show con
          , "Other constructors are not supported yet."
          ]

getConName :: Con -> Q Name
getConName = \case
  NormalC n _ ->
    pure n
  RecC n _ ->
    pure n
  other ->
    fail $
      unlines
        [ "Expected a normal or record constructor, got unsupported constructor: "
        , "\t" <> show other
        ]

-- | This function should be called with a datatype consisting of multiple
-- constructors. The constructor will be converted into a subcommand name.
makeSubcommand :: Modifiers -> Con -> Q Exp
makeSubcommand modifiers@Modifiers {..} con = do
  conName <- getConName con
  singleCommandParserExpr <- makeSingleCommand modifiers con
  let conNameString =
        nameBase conName
      name =
        constructorNameModifier conNameString

  subparserFieldsExpr <-
    [e|
      Options.command
        $(lift name)
        ( Options.info (Options.helper <*> $(pure singleCommandParserExpr)) mempty
        )
        <> Options.metavar $(lift name)
      |]

  [e|Options.subparser $(pure subparserFieldsExpr)|]

#if MIN_VERSION_template_haskell(2,18,0)
mkConP :: Name -> [Pat] -> Pat
mkConP name pats = ConP name [] pats
#else
mkConP :: Name -> [Pat] -> Pat
mkConP name pats = ConP name pats
#endif
