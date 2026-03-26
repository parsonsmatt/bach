module Bach.Types
    ( PRIdentifier (..)
    , PullRequest (..)
    , RepoContext (..)
    , MergeResult (..)
    , ConflictPair (..)
    , FugueResults (..)
    , FugueOptions (..)
    , OutputFormat (..)
    , parsePRIdentifier
    ) where

import Bach.Prelude
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.Text as T

data PRIdentifier
    = PRById !Int
    | PRByBranch !Text
    deriving stock (Show, Eq)

parsePRIdentifier :: Text -> PRIdentifier
parsePRIdentifier t =
    case readMaybe (T.unpack t) of
        Just n -> PRById n
        Nothing -> PRByBranch t

data PullRequest = PullRequest
    { prNumber :: !Int
    , prTitle :: !Text
    , prHeadRef :: !Text
    , prBaseRef :: !Text
    , prIsDraft :: !Bool
    , prUrl :: !Text
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON PullRequest where
    parseJSON = withObject "PullRequest" $ \o ->
        PullRequest
            <$> o
            .: "number"
            <*> o
            .: "title"
            <*> o
            .: "headRefName"
            <*> o
            .: "baseRefName"
            <*> o
            .: "isDraft"
            <*> o
            .: "url"

instance ToJSON PullRequest where
    toJSON pr =
        object
            [ "number" .= pr.prNumber
            , "title" .= pr.prTitle
            , "headRefName" .= pr.prHeadRef
            , "baseRefName" .= pr.prBaseRef
            , "isDraft" .= pr.prIsDraft
            , "url" .= pr.prUrl
            ]

data RepoContext = RepoContext
    { repoOwner :: !Text
    , repoName :: !Text
    , repoDefaultBase :: !Text
    , repoLocalPath :: !FilePath
    }
    deriving stock (Show, Eq)

data MergeResult
    = MergeOk !Text
    | MergeConflict ![Text]
    deriving stock (Show, Eq)

data ConflictPair = ConflictPair
    { cpLeft :: !Int
    , cpRight :: !Int
    , cpFiles :: ![Text]
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON ConflictPair where
    toJSON cp =
        object
            [ "left" .= cp.cpLeft
            , "right" .= cp.cpRight
            , "files" .= cp.cpFiles
            ]

instance FromJSON ConflictPair where
    parseJSON = withObject "ConflictPair" $ \o ->
        ConflictPair
            <$> o
            .: "left"
            <*> o
            .: "right"
            <*> o
            .: "files"

data FugueResults = FugueResults
    { frBaseConflicts :: ![PullRequest]
    , frConflictPairs :: ![ConflictPair]
    , frReady :: ![PullRequest]
    , frDeferred :: ![PullRequest]
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON FugueResults where
    toJSON r =
        object
            [ "baseConflicts" .= r.frBaseConflicts
            , "conflictPairs" .= r.frConflictPairs
            , "ready" .= r.frReady
            , "deferred" .= r.frDeferred
            ]

instance FromJSON FugueResults where
    parseJSON = withObject "FugueResults" $ \o ->
        FugueResults
            <$> o
            .: "baseConflicts"
            <*> o
            .: "conflictPairs"
            <*> o
            .: "ready"
            <*> o
            .: "deferred"

data OutputFormat = Human | JSON | GhActions
    deriving stock (Show, Eq)

data FugueOptions = FugueOptions
    { fugueDryRun :: !Bool
    , fugueBase :: !(Maybe Text)
    , fugueNoFetch :: !Bool
    , fugueOutput :: !OutputFormat
    , fuguePlanFile :: !FilePath
    , fugueTargets :: !(NonEmpty PRIdentifier)
    , fugueMustInclude :: ![PRIdentifier]
    }
    deriving stock (Show, Eq)
