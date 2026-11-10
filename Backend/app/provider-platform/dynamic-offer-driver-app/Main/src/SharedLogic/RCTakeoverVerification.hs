{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

 distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

 FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

 General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Photo check before a driver takes over an RC that is active for another driver.
--
--   * Verify ('verifyTakeoverPhoto') asks Grid whether a fresh vehicle photo matches the RC. A VALID result is
--     staged in Redis for this driver and RC, pinned to the current holder ('HolderSnapshot').
--   * Activation ('requireFreshTakeoverVerification') accepts only that server-side record, re-checked
--     against the live holder.
--   * The evidence is persisted ('persistTakeoverAudit') only after the takeover activation succeeds.
module SharedLogic.RCTakeoverVerification
  ( HolderSnapshot (..),
    StagedTakeoverVerification (..),
    TakeoverInputs (..),
    verifyTakeoverPhoto,
    requireFreshTakeoverVerification,
    findStagedTakeoverVerification,
    persistTakeoverAudit,
    holderUnlinkBlocker,
    RcLink (..),
    rcLinkCheck,
    driverActivationBlocker,
    combineTakeoverScore,
    expectedGridVehicleType,
  )
where

import qualified Control.Monad.Catch as Catch
import qualified Crypto.Hash as Hash
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Domain.Types.Common as DCommon
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverRCAssociation as DRCA
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import qualified Domain.Types.VehicleVariant as DV
import Kernel.External.Encryption
import qualified Kernel.External.Grid.Error as GridError
import qualified Kernel.External.Grid.Flow as Grid
import qualified Kernel.External.Grid.Types as Grid
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions (..))
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Network.HTTP.Types (statusCode)
import qualified Servant.Client as Servant
import qualified SharedLogic.RCTransferNotice as RCTransferNotice
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as DIQuery
import qualified Storage.Queries.DriverRCAssociation as DAQuery
import qualified Storage.Queries.FleetDriverAssociationExtra as FDA
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as RQuery
import qualified Storage.Queries.Vehicle as VQuery
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Text.Show
import Tools.Error
import qualified Tools.Grid as TGrid

-- | Who holds the RC right now (clash case 1: another driver has it active). The holder's vehicle row is
--   re-created on every activation, so its createdAt differs per ownership period, even though the
--   association row is reused (A -> B -> A).
data HolderSnapshot = HolderSnapshot
  { holderDriverId :: Id DP.Person,
    holderAssociationId :: Id DRCA.DriverRCAssociation,
    holderVehicleCreatedAt :: Maybe UTCTime
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | The four numeric answers, each on a 0-1 scale.
data TakeoverInputs = TakeoverInputs
  { plateVisible :: Double,
    plateMatchesRc :: Double,
    isRealPhoto :: Double,
    -- | Grid's 0-3 photo_quality score divided by 3.
    photoQuality :: Double
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | A VALID photo check, staged for one driver and one RC until it is used or expires.
data StagedTakeoverVerification = StagedTakeoverVerification
  { verificationId :: Text,
    driverId :: Id DP.Person,
    rcId :: Id DVRC.VehicleRegistrationCertificate,
    holder :: HolderSnapshot,
    -- | As sent to Grid: raw base64, data-URI prefix stripped.
    imageBase64 :: Text,
    imageMime :: Text,
    -- | SHA-256 (hex) of imageBase64.
    imageSha256 :: Text,
    vehicleType :: Text,
    vehicleTypeConfidence :: Maybe Double,
    inputs :: TakeoverInputs,
    takeoverScore :: Double,
    -- | The scoring config the verdict was based on.
    scoring :: DTC.TakeoverScoring,
    requestedModel :: Text,
    resolvedModel :: Maybe Text,
    verifiedAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON)

-- Not derived: never print the image.
instance Show StagedTakeoverVerification where
  show s =
    "StagedTakeoverVerification{verificationId=" <> show s.verificationId
      <> ", driverId="
      <> show s.driverId.getId
      <> ", rcId="
      <> show s.rcId.getId
      <> ", imageB64Chars="
      <> show (T.length s.imageBase64)
      <> ", takeoverScore="
      <> show s.takeoverScore
      <> "}"

takeoverKey :: Id DP.Person -> Id DVRC.VehicleRegistrationCertificate -> Text
takeoverKey driverId rcId = "RCTakeoverVerification:" <> driverId.getId <> ":" <> rcId.getId

takeoverPhotoHitsKey :: Id DP.Person -> Text
takeoverPhotoHitsKey driverId = "BPP:RCTakeoverPhoto:verify:" <> driverId.getId <> ":hitsCount"

stagedTtlSeconds :: Int
stagedTtlSeconds = 600

verifyRateLimit :: APIRateLimitOptions
verifyRateLimit = APIRateLimitOptions {limit = 5, limitResetTimeInSec = 3600}

rcNumberBounds :: (Int, Int)
rcNumberBounds = (5, 12)

-- * Holder

-- | Nothing when the RC is not active for another driver: already the requester's, free, or clash case 2
--   (only the vehicle row is with another driver), none of which needs a photo.
currentHolderSnapshot :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> DVRC.VehicleRegistrationCertificate -> m (Maybe HolderSnapshot)
currentHolderSnapshot requesterId rc =
  DAQuery.findActiveAssociationByRC rc.id True >>= \case
    Just assoc
      | assoc.driverId /= requesterId -> do
        mbVehicle <- VQuery.findById assoc.driverId
        -- Rare data state: an active association with no vehicle row. Allowed; A -> B -> A is still caught,
        -- because the holder's next activation creates a vehicle row (Nothing becomes Just).
        when (isNothing mbVehicle) $
          logWarning $ "RC takeover: holder has an active association but no vehicle row: rcId=" <> rc.id.getId <> ", holderId=" <> assoc.driverId.getId
        pure . Just $ HolderSnapshot assoc.driverId assoc.id (truncateToSeconds . (.createdAt) <$> mbVehicle)
    _ -> pure Nothing

-- | vehicle is KV-enabled: the KV/Redis copy can keep picoseconds while a DB read has microseconds, and the
--   staged snapshot round-trips through JSON. Never compare the raw value. Two ownership periods can't start
--   within the same second (the holder cutoff and ride checks prevent it), so seconds lose nothing.
truncateToSeconds :: UTCTime -> UTCTime
truncateToSeconds t = t {Time.utctDayTime = fromIntegral (floor (Time.utctDayTime t) :: Integer)}

-- | Why the current holder can't lose the RC right now, if anything: on a ride, a recent ride within the
--   cutoff (unless offline and allowed), or a brand-new account. Shared by verify and activation.
--   With the readable text activation records on the activating driver's link (it never identifies the holder).
holderUnlinkBlocker :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DTC.TransporterConfig -> UTCTime -> Id DP.Person -> m (Maybe (DriverOnboardingError, Text))
holderUnlinkBlocker transporterConfig now holderId = do
  holderInfo <- DIQuery.findById holderId >>= fromMaybeM (PersonNotFound holderId.getId)
  let canUnlinkWhenOffline = transporterConfig.allowRcUnlinkWhenDriverOffline == Just True && holderInfo.mode == Just DCommon.OFFLINE
      withinCutOff since = nominalDiffTimeToSeconds (diffUTCTime now since) <= transporterConfig.automaticRCActivationCutOff
  if
      | holderInfo.onRide -> pure $ Just (RCOtherDriverOnRide, "Vehicle is on a ride with another driver")
      | canUnlinkWhenOffline -> pure Nothing
      | otherwise ->
        RQuery.findLastRideAssigned holderId >>= \case
          Just lastRide -> pure $ bool Nothing (Just (RCOtherDriverRecentRide, "Vehicle was used for a ride recently on another account")) (withinCutOff lastRide.createdAt)
          Nothing -> do
            -- the holder hasn't taken a ride yet: protect a freshly created account instead
            person <- QPerson.findById holderId >>= fromMaybeM (PersonNotFound holderId.getId)
            pure $ bool Nothing (Just (RCOtherDriverAccountNew, "Vehicle is active on a recently created account")) (withinCutOff person.createdAt)

-- | Why the activating driver can't take this RC right now, if anything, with the readable text activation
--   records on the driver's link. Read-only: activation records the text and throws, verify only throws.
driverActivationBlocker :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DI.DriverInformation -> DTC.TransporterConfig -> DVRC.VehicleRegistrationCertificate -> m (Maybe (DriverOnboardingError, Text))
driverActivationBlocker driverInfo transporterConfig rc = do
  let gated = transporterConfig.requiresOnboardingInspection == Just True || transporterConfig.enableBotFlow == Just True || transporterConfig.unifiedOnboardingFlagsRecompute == Just True
  if
      | gated && not driverInfo.enabled -> pure $ Just (RCActivationDriverNotEnabled, "Driver is not enabled")
      | gated && not (fromMaybe False rc.approved) -> pure $ Just (RCActivationRCNotApproved, "Vehicle is not approved")
      | otherwise -> do
        -- activateRC swaps out the driver's current vehicle, which is refused mid-ride
        isOnRide <- DIQuery.findByDriverIdActiveRide (cast driverInfo.driverId)
        pure $ bool Nothing (Just (RCActivationDriverOnRide, "Driver is on a ride")) (isJust isOnRide)

-- | How the driver may use the RC: already linked, or a fleet RC they may link to.
data RcLink = Linked | UsableFleetRc

-- | The link check activation runs, read-only. Left carries the error and the readable text activation
--   records on the driver's link (Nothing: none recorded). Activation creates the association for
--   UsableFleetRc; verify never does.
rcLinkCheck :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DTC.TransporterConfig -> Id DP.Person -> DVRC.VehicleRegistrationCertificate -> UTCTime -> m (Either (DriverOnboardingError, Maybe Text) RcLink)
rcLinkCheck transporterConfig driverId rc now =
  DAQuery.findLinkedByRCIdAndDriverId driverId rc.id now >>= \case
    Just _ -> pure $ Right Linked
    Nothing
      | transporterConfig.allowDriverToUseFleetRcs /= Just True -> pure $ Left (RCActivationRCNotLinked, Just "RC is not associated with the driver")
      | otherwise ->
        FDA.findByDriverId driverId True <&> \case
          Nothing -> Left (RCActivationRCNotLinked, Nothing)
          Just fda
            | rc.fleetOwnerId == Just fda.fleetOwnerId -> Right UsableFleetRc
            | otherwise -> Left (RCActivationRCNotInFleet, Just "RC does not belong to you or your fleet.")

-- * Verify

-- | True for VALID. Precondition failures (RC not linked, no takeover situation, holder can't lose the RC
--   now) are typed errors. Every failure of the photo check itself is INVALID, and revokes an earlier VALID.
verifyTakeoverPhoto ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r, CoreMetrics m, HasRequestId r, MonadReader r m) =>
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Text ->
  Text ->
  m Bool
verifyTakeoverPhoto (driverId, merchantId, merchantOpCityId) rcNo rawImage = do
  rc <- RCQuery.findLastVehicleRCWrapper rcNo >>= fromMaybeM (RCNotFound rcNo)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  -- With the gate off no activation reads a staged check, so don't spend a Grid call on one.
  unless (transporterConfig.requireRcTakeoverPhoto == Just True) $ throwError (InvalidRequest "Vehicle photo check is not required in this city")
  -- The checks activation runs before the gate: a photo for an activation that would fail anyway is wasted.
  unless (rc.verificationStatus == Documents.VALID) $ throwError RCActivationRCNotValid
  now <- getCurrentTime
  rcLinkCheck transporterConfig driverId rc now >>= either (throwError . fst) (const $ pure ())
  driverInfo <- DIQuery.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  driverActivationBlocker driverInfo transporterConfig rc >>= mapM_ (throwError . fst)
  holder <- currentHolderSnapshot driverId rc >>= fromMaybeM RCTakeoverNoClash
  -- A same-fleet shift handover, or a vehicle type Grid can't check, needs no photo at activation, so none
  -- is checked here either.
  handover <- RCTransferNotice.isFleetHandover rc holder.holderDriverId driverId
  when (handover || isNothing (expectedGridVehicleType rc)) $ throwError RCTakeoverNoClash
  holderUnlinkBlocker transporterConfig now holder.holderDriverId >>= mapM_ (throwError . fst)
  verificationId <- generateGUID
  let ctx = StageContext {verificationId, driverId, rcId = rc.id, holder}
  mbGridCfg <- TGrid.getGridCfg merchantId merchantOpCityId
  rcNumber <- decrypt rc.certificateNumber
  verdict <- case prepareConfig transporterConfig rc mbGridCfg rcNumber of
    -- A config or data gap (no Grid config, no scoring, a vehicle type Grid can't check, an RC number that
    -- doesn't normalize) is not the photo's fault: INVALID, but no Grid call and no attempt charged.
    Left rejection -> pure $ Left rejection
    Right prepared -> do
      -- Charged only for a real photo check: precondition failures above must not burn the driver's attempts.
      checkSlidingWindowLimitWithOptions (takeoverPhotoHitsKey driverId) verifyRateLimit
      -- Plain try, not withTryCatch: that logs the raw exception, whose servant request carries the Grid key.
      -- Async exceptions (request cancelled, thread killed) are rethrown, never turned into a verdict.
      Catch.try @_ @SomeException (gridVerdict transporterConfig ctx prepared rawImage) >>= \case
        Left err
          | isJust (fromException @SomeAsyncException err) -> Catch.throwM err
          | otherwise -> pure $ Left (Rejection ("Grid call failed: " <> gridFailureReason err) Nothing Nothing)
        Right res -> pure res
  let key = takeoverKey driverId rc.id
      logVerdict result reason inputs score =
        logInfo $
          "RC takeover photo " <> result <> ": verificationId=" <> verificationId <> ", rcId=" <> rc.id.getId <> ", driverId=" <> driverId.getId
            <> maybe "" (", reason=" <>) reason
            <> maybe "" ((", inputs=" <>) . show) inputs
            <> maybe "" ((", takeoverScore=" <>) . show) score
  case verdict of
    Right staged -> do
      logVerdict "VALID" Nothing (Just staged.inputs) (Just staged.takeoverScore)
      Redis.setExp key staged stagedTtlSeconds
      pure True
    Left Rejection {..} -> do
      logVerdict "INVALID" (Just reason) rejectedInputs rejectedScore
      Redis.del key -- the latest attempt wins
      pure False

-- | The photo part of the check: decode the image, ask Grid, judge the answers.
gridVerdict ::
  (MonadFlow m, CoreMetrics m, EncFlow m r, HasRequestId r, MonadReader r m) =>
  DTC.TransporterConfig ->
  StageContext ->
  (Grid.GridCfg, DTC.TakeoverScoring, Text, Text) ->
  Text ->
  m (Either Rejection StagedTakeoverVerification)
gridVerdict transporterConfig ctx (gridCfg, scoring, expected, plate) raw =
  case decodeImage transporterConfig gridCfg raw of
    Left rejection -> pure $ Left rejection
    Right image -> do
      let questions = takeoverQuestions plate
      resp <-
        Grid.decide gridCfg $
          Grid.GridDecisionReq
            { model = gridCfg.model,
              state = "Live vehicle photo (attached image) uploaded by a driver in a ride-hailing app in India.",
              images = [image],
              questions
            }
      verifiedAt <- getCurrentTime
      pure $ judge ctx verifiedAt gridCfg scoring expected image questions resp

data StageContext = StageContext
  { verificationId :: Text,
    driverId :: Id DP.Person,
    rcId :: Id DVRC.VehicleRegistrationCertificate,
    holder :: HolderSnapshot
  }

-- | Why a photo check came out INVALID, with whatever scores were known by then (for shadow-mode calibration).
data Rejection = Rejection
  { reason :: Text,
    rejectedInputs :: Maybe TakeoverInputs,
    rejectedScore :: Maybe Double
  }

orReject :: Maybe a -> Text -> Either Rejection a
orReject m reason = maybe (Left $ Rejection reason Nothing Nothing) Right m

-- | Everything the check needs that doesn't come from the photo: Grid config, scoring, expected vehicle type
--   and the plate text.
prepareConfig ::
  DTC.TransporterConfig ->
  DVRC.VehicleRegistrationCertificate ->
  Maybe Grid.GridCfg ->
  Text ->
  Either Rejection (Grid.GridCfg, DTC.TakeoverScoring, Text, Text)
prepareConfig transporterConfig rc mbGridCfg rcNumber = do
  gridCfg <- mbGridCfg `orReject` "Grid is not configured for the city"
  scoring <- transporterConfig.takeoverScoring `orReject` "takeoverScoring is not configured"
  expected <- expectedGridVehicleType rc `orReject` "RC vehicle variant has no Grid vehicle type"
  -- the stored RC number, never client text, so nothing user-controlled reaches the prompt
  plate <- Grid.normalizeIdentifier rcNumberBounds rcNumber `orReject` "RC number does not normalize"
  pure (gridCfg, scoring, expected, plate)

decodeImage :: DTC.TransporterConfig -> Grid.GridCfg -> Text -> Either Rejection Grid.GridImage
decodeImage transporterConfig gridCfg raw =
  let docLimitBytes = fromMaybe 100 transporterConfig.maxAllowedDocSizeInMB * 1024 * 1024
      maxBytes = maybe docLimitBytes (min docLimitBytes) gridCfg.maxImageBytes
   in either (\err -> Left $ Rejection err Nothing Nothing) Right $ Grid.mkGridImageFromBase64 (Just maxBytes) raw

takeoverQuestions :: Text -> Grid.GridQuestions
takeoverQuestions plate =
  Grid.mkGridQuestions
    [ ( "vehicle_type",
        Grid.ChoiceQ
          "What kind of vehicle is the main subject of the photo?"
          ( ("auto", "Indian auto rickshaw (tuk-tuk, Bajaj RE style): a small three-wheeler with a single front wheel, an open or canvas-covered body and a driver seat in front")
              :| [ ("cab", "Four-wheeler car or taxi"),
                   ("bike", "Two-wheeler"),
                   ("other", "Some other vehicle"),
                   ("no_vehicle", "No vehicle in the photo")
                 ]
          )
      ),
      ("plate_visible", Grid.NoulQ "Is the vehicle number plate clearly visible and readable?"),
      ( "plate_matches_rc",
        Grid.NoulQ
          ( "Does the number plate in the photo read exactly " <> plate
              <> " (ignore spaces and line breaks)? Compare the characters one by one, especially the letters A, N and M and the digits 1, 2 and 3."
          )
      ),
      ("is_real_photo", Grid.NoulQ "Is this a real photo of a physical vehicle, not a photo of a screen, a printout or another photo?"),
      -- vehicle-specific wording in place of the shared-kernel built-in; same key, so Grid.getPhotoQuality still reads it
      ( Grid.photoQualityKey,
        Grid.ScoreQ
          "How usable is this photo for verifying the vehicle?"
          ("unusable: blurry, dark or plate cut off" :| ["poor", "ok", "good: sharp, well lit, full vehicle and plate visible"])
      )
    ]

-- | All five answers must be present and valid (decodeAnswers silently drops bad ones). The vehicle type is
--   a hard check, kept out of the score.
judge ::
  StageContext ->
  UTCTime ->
  Grid.GridCfg ->
  DTC.TakeoverScoring ->
  Text ->
  Grid.GridImage ->
  Grid.GridQuestions ->
  Grid.GridDecisionResp ->
  Either Rejection StagedTakeoverVerification
judge StageContext {..} verifiedAt gridCfg scoring expected image questions resp = do
  let answers = Grid.decodeAnswers questions resp
  vt <- Grid.getChoice "vehicle_type" answers `orReject` "vehicle_type answer missing or invalid"
  pv <- Grid.getNoul "plate_visible" answers `orReject` "plate_visible answer missing or invalid"
  pm <- Grid.getNoul "plate_matches_rc" answers `orReject` "plate_matches_rc answer missing or invalid"
  real <- Grid.getNoul "is_real_photo" answers `orReject` "is_real_photo answer missing or invalid"
  pq <- Grid.getPhotoQuality answers `orReject` "photo_quality answer missing or invalid"
  let inputs = TakeoverInputs {plateVisible = pv.pYes, plateMatchesRc = pm.pYes, isRealPhoto = real.pYes, photoQuality = pq.score / 3}
      reject :: Text -> Maybe Double -> Either Rejection a
      reject reason score = Left $ Rejection reason (Just inputs) score
  score <- maybe (reject "takeoverScoring could not be applied" Nothing) Right $ combineTakeoverScore scoring inputs
  unless (vt.choice == expected) $ reject ("vehicle_type " <> vt.choice <> ", expected " <> expected) (Just score)
  unless (score >= scoring.takeoverScoreThreshold) $ reject "takeoverScore below threshold" (Just score)
  unless (passesFloors scoring inputs) $ reject "a score is below its floor" (Just score)
  pure
    StagedTakeoverVerification
      { verificationId,
        driverId,
        rcId,
        holder,
        imageBase64 = image.base64Data,
        imageMime = Grid.mimeText image.mime,
        imageSha256 = sha256Hex image.base64Data,
        vehicleType = vt.choice,
        vehicleTypeConfidence = vt.confidence,
        inputs,
        takeoverScore = score,
        scoring,
        requestedModel = gridCfg.model,
        resolvedModel = resp.model,
        verifiedAt
      }

-- | The only place the score formula lives. Weights, formula and threshold come from config; there are no
--   built-in values. Nothing (so INVALID) for an unknown formula or weights it can't apply.
--
--   - "v1-geomean" (recommended): Π xᵢ^(wᵢ / Σ wᵢ). Any low score pulls the total down hard, and a zero
--     with a positive weight gives zero, so a strong score can't hide a mismatched plate.
--   - "placeholder-weighted-mean": Σ wᵢ·xᵢ / Σ wᵢ, for local testing only; a strong score can make up for a
--     weak one, so don't enforce it.
--
--   (to build later) The weight and threshold values.
combineTakeoverScore :: DTC.TakeoverScoring -> TakeoverInputs -> Maybe Double
combineTakeoverScore scoring TakeoverInputs {..}
  | not (all (\w -> w >= 0 && not (isNaN w)) weights && totalWeight > 0) = Nothing
  | otherwise = case scoring.formulaVersion of
    "v1-geomean" -> Just $ product (zipWith (\w x -> x ** (w / totalWeight)) weights xs)
    "placeholder-weighted-mean" -> Just $ sum (zipWith (*) weights xs) / totalWeight
    _ -> Nothing
  where
    weights = [scoring.plateVisibleWeight, scoring.plateMatchesRcWeight, scoring.isRealPhotoWeight, scoring.photoQualityWeight]
    xs = [plateVisible, plateMatchesRc, isRealPhoto, photoQuality]
    totalWeight = sum weights

-- | Optional hard minimums the other scores can't compensate for.
passesFloors :: DTC.TakeoverScoring -> TakeoverInputs -> Bool
passesFloors scoring inputs =
  maybe True (inputs.plateMatchesRc >=) scoring.plateMatchesRcFloor
    && maybe True (inputs.isRealPhoto >=) scoring.isRealPhotoFloor

-- | Same derivation the vehicle row uses. Categories Grid isn't asked about fail closed.
expectedGridVehicleType :: DVRC.VehicleRegistrationCertificate -> Maybe Text
expectedGridVehicleType rc =
  rc.vehicleVariant <&> DV.castVehicleVariantToVehicleCategory >>= \case
    DVC.AUTO_CATEGORY -> Just "auto"
    DVC.CAR -> Just "cab"
    DVC.MOTORCYCLE -> Just "bike"
    DVC.AMBULANCE -> Nothing
    DVC.TRUCK -> Nothing
    DVC.BUS -> Nothing
    DVC.BOAT -> Nothing
    DVC.TOTO -> Nothing
    DVC.TRAIN -> Nothing
    DVC.FLIGHT -> Nothing

sha256Hex :: Text -> Text
sha256Hex t = show (Hash.hashWith Hash.SHA256 (encodeUtf8 t :: BS.ByteString))

-- * Activation

-- | The takeover gate. Runs after every existing activation check and before the holder is unlinked. Only a
--   staged VALID for this driver and RC counts, and only while the same holder still has the RC.
requireFreshTakeoverVerification :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> DVRC.VehicleRegistrationCertificate -> m StagedTakeoverVerification
requireFreshTakeoverVerification driverId rc = do
  let key = takeoverKey driverId rc.id
  Redis.safeGet key >>= \case
    Nothing -> rejectTakeover driverId rc "Vehicle photo required to take over this RC" RCTakeoverPhotoRequired
    Just staged -> do
      live <- currentHolderSnapshot driverId rc
      unless (live == Just staged.holder) $ do
        Redis.del key
        rejectTakeover driverId rc "Vehicle status changed, new photo required to take over this RC" RCTakeoverVerificationStale
      pure staged

-- | A loggable reason for a failed Grid call. Never `show` the exception itself: ExternalAPICallError carries
--   servant's ClientError, whose request includes the Authorization / x-litellm-api-key header.
gridFailureReason :: SomeException -> Text
gridFailureReason err
  | Just (gridErr :: GridError.GridError) <- fromException err = show gridErr
  | Just (apiErr :: ExternalAPICallError) <- fromException err =
    "ExternalAPICallError: " <> case apiErr.clientError of
      Servant.FailureResponse _ resp -> "status " <> show (statusCode (Servant.responseStatusCode resp))
      Servant.DecodeFailure _ _ -> "decode failure"
      Servant.UnsupportedContentType _ _ -> "unsupported content type"
      Servant.InvalidContentTypeHeader _ -> "invalid content type header"
      Servant.ConnectionError _ -> "connection error"
  | otherwise = "unexpected error"

-- | The staged photo check for this driver and RC, if any. Used after a successful activation that found no
--   clash: the holder may have been unlinked by an earlier attempt whose activateRC threw (no rollback).
findStagedTakeoverVerification :: (MonadFlow m, CacheFlow m r) => Id DP.Person -> Id DVRC.VehicleRegistrationCertificate -> m (Maybe StagedTakeoverVerification)
findStagedTakeoverVerification driverId rcId = Redis.safeGet (takeoverKey driverId rcId)

-- | The failed attempt is the activating driver's, so the readable message goes on their link.
rejectTakeover :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> DVRC.VehicleRegistrationCertificate -> Text -> DriverOnboardingError -> m a
rejectTakeover driverId rc message err = do
  DAQuery.updateRcErrorMessage driverId rc.id message
  throwError err

-- | Called only after the takeover activation succeeded, and never fails it.
--
--   (to build later) The audit storage: S3.put of the image and the audit row. Until then, a masked log line
--   (ids and scores; no image, no RC number) is the only record, so don't enforce the gate in production.
--
--   holderUnlinkedEarlier: this activation found no clash, and the record is written from the staged holder
--   snapshot (a retry after activateRC threw, or the holder lost the RC some other way within the TTL).
persistTakeoverAudit :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => DVRC.VehicleRegistrationCertificate -> Maybe (Id DP.Person) -> Bool -> StagedTakeoverVerification -> m ()
persistTakeoverAudit rc mbPreviousHolderId holderUnlinkedEarlier staged = do
  res <- withTryCatch "persistTakeoverAudit" $ do
    rcNumber <- decrypt rc.certificateNumber
    logInfo $
      "RC takeover verified (audit storage pending): verificationId=" <> staged.verificationId
        <> ", rcId="
        <> rc.id.getId
        <> ", rcNumber="
        <> maskText rcNumber
        <> ", driverId="
        <> staged.driverId.getId
        <> ", previousHolderId="
        <> maybe "none" (.getId) mbPreviousHolderId
        <> ", holderUnlinkedEarlier="
        <> show holderUnlinkedEarlier
        <> ", vehicleType="
        <> staged.vehicleType
        <> ", inputs="
        <> show staged.inputs
        <> ", takeoverScore="
        <> show staged.takeoverScore
        <> ", scoring="
        <> show staged.scoring
  -- Single use, even when the log above failed: a leftover key could be picked up by a later activation.
  -- Its own try: the activation has already happened, so a Redis error here must not fail the request.
  delRes <- withTryCatch "persistTakeoverAudit:del" $ Redis.del (takeoverKey staged.driverId rc.id)
  case delRes of
    Left err -> logError $ "RC takeover staged key not deleted: verificationId=" <> staged.verificationId <> ", error=" <> show err
    Right () -> pure ()
  case res of
    Right () -> pure ()
    Left err ->
      logError $
        "RC_TAKEOVER_AUDIT_FAILED: verificationId=" <> staged.verificationId <> ", rcId=" <> rc.id.getId <> ", driverId=" <> staged.driverId.getId
          <> ", previousHolderId="
          <> maybe "none" (.getId) mbPreviousHolderId
          <> ", takeoverScore="
          <> show staged.takeoverScore
          <> ", error="
          <> show err
