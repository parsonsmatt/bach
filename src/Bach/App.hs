module Bach.App
    ( runApp
    ) where

import Bach.Forge (ForgeHandle, HasForgeHandle (..), markDraft, markReady)
import Bach.Forge.GitHub (mkGitHubForgeHandle)
import Bach.Fugue (runFugue)
import Bach.Git (detectRepoContext)
import Bach.Output (outputGhActions, outputHuman, outputJson)
import Bach.Prelude
import Bach.Types
import Data.Aeson (eitherDecode, encode)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Options.Applicative
import qualified RIO.ByteString.Lazy as LBS
import RIO.Directory (getCurrentDirectory)

data AppEnv = AppEnv
    { appLogFunc :: !LogFunc
    , appForgeHandle :: !ForgeHandle
    }

instance HasLogFunc AppEnv where
    logFuncL = lens appLogFunc (\x y -> x{appLogFunc = y})

instance HasForgeHandle AppEnv where
    forgeHandleL = lens appForgeHandle (\x y -> x{appForgeHandle = y})

-- | Main entry point.
runApp :: IO ()
runApp = do
    cmd <- execParser opts
    case cmd of
        CmdFugue fugueOpts -> runFugueCommand fugueOpts
        CmdApplyPlan path -> runApplyPlanCommand path
  where
    opts =
        info
            (commandParser <**> helper)
            ( fullDesc
                <> progDesc "Bach — PR batch merge tool"
                <> header "bach - find conflict-free sets of pull requests"
            )

data Command
    = CmdFugue FugueOptions
    | CmdApplyPlan FilePath

commandParser :: Parser Command
commandParser =
    subparser
        ( command
            "fugue"
            ( info
                (CmdFugue <$> fugueOptionsParser <**> helper)
                ( fullDesc
                    <> progDesc
                        "Analyze PRs for pairwise merge conflicts and compute conflict-free batches."
                )
            )
            <> command
                "apply-plan"
                ( info
                    ( CmdApplyPlan
                        <$> argument str (metavar "FILE" <> help "Plan file to apply")
                        <**> helper
                    )
                    ( fullDesc
                        <> progDesc "Apply a previously saved plan: mark batch 1 ready, draft the rest."
                    )
                )
        )

fugueOptionsParser :: Parser FugueOptions
fugueOptionsParser =
    FugueOptions
        <$> switch
            ( long "dry-run"
                <> help "Analyze and report without modifying PR status"
            )
        <*> optional
            ( T.pack
                <$> strOption
                    ( long "base"
                        <> metavar "BRANCH"
                        <> help "Base branch to merge into (default: detected from origin/HEAD)"
                    )
            )
        <*> switch
            ( long "no-fetch"
                <> help "Skip fetching; use only local refs"
            )
        <*> parseOutputFormat
        <*> strOption
            ( long "plan-file"
                <> value "/tmp/bach-plan.json"
                <> showDefault
                <> metavar "FILE"
                <> help "Where to save the analysis plan"
            )
        <*> ( NE.fromList
                <$> some
                    ( parsePRIdentifier
                        . T.pack
                        <$> argument
                            str
                            ( metavar "PR..."
                                <> help "PR numbers or branch names"
                            )
                    )
            )
        <*> many
            ( parsePRIdentifier
                . T.pack
                <$> strOption
                    ( long "must-include"
                        <> metavar "PR"
                        <> help "PR (number or branch) that must be in the ready batch"
                    )
            )

parseOutputFormat :: Parser OutputFormat
parseOutputFormat =
    flag' JSON (long "json" <> help "Output results as JSON")
        <|> flag'
            GhActions
            (long "gh-actions" <> help "Output gh CLI commands for each batch")
        <|> pure Human

withAppEnv :: (AppEnv -> IO a) -> IO a
withAppEnv runAction = do
    logOptions <- logOptionsHandle stderr True
    let
        logOptions' =
            setLogMinLevel LevelInfo
                . setLogUseLoc False
                . setLogUseTime False
                . setLogVerboseFormat False
                $ logOptions
    withLogFunc logOptions' $ \lf -> do
        cwd <- getCurrentDirectory
        ctx <- detectRepoContext cwd
        let
            env =
                AppEnv
                    { appLogFunc = lf
                    , appForgeHandle = mkGitHubForgeHandle ctx
                    }
        runAction env

runFugueCommand :: FugueOptions -> IO ()
runFugueCommand fugueOpts =
    withAppEnv $ \env -> runRIO env $ do
        results <- runFugue fugueOpts

        -- Save plan
        liftIO $ LBS.writeFile (fuguePlanFile fugueOpts) (encode results)
        logInfo $ "Plan saved to " <> displayShow (fuguePlanFile fugueOpts)

        -- Output results
        case fugueOutput fugueOpts of
            Human -> outputHuman results
            JSON -> outputJson results
            GhActions -> outputGhActions results

        -- Apply unless dry-run
        unless (fugueDryRun fugueOpts) $ do
            logInfo "Applying results..."
            applyResults results

runApplyPlanCommand :: FilePath -> IO ()
runApplyPlanCommand path =
    withAppEnv $ \env -> runRIO env $ do
        logInfo $ "Loading plan from " <> displayShow path
        raw <- liftIO $ LBS.readFile path
        case eitherDecode raw of
            Left err -> do
                logError $ "Failed to parse plan: " <> displayShow err
                exitFailure
            Right results ->
                applyResults results

-- | Apply results: mark ready PRs as ready, draft deferred + base conflicts.
applyResults
    :: (HasForgeHandle env, HasLogFunc env) => FugueResults -> RIO env ()
applyResults results = do
    -- Draft deferred PRs
    forM_ results.frDeferred $ \pr -> do
        logInfo $ "Drafting #" <> display pr.prNumber <> "..."
        markDraft pr.prNumber

    -- Draft base conflicts
    forM_ results.frBaseConflicts $ \pr -> do
        logInfo $ "Drafting base-conflicting #" <> display pr.prNumber <> "..."
        markDraft pr.prNumber

    -- Mark ready PRs as ready
    forM_ results.frReady $ \pr -> do
        logInfo $ "Marking ready: #" <> display pr.prNumber <> "..."
        markReady pr.prNumber

    logInfo "Done."
