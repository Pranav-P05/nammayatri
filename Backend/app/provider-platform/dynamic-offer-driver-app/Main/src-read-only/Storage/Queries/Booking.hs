{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Booking (module Storage.Queries.Booking, module ReExport) where

import qualified Domain.Types.Booking
import qualified Domain.Types.FareSettlement
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Booking as Beam
import Storage.Queries.BookingExtra as ReExport
import Storage.Queries.Transformers.Booking

updateEstimatedDistanceAndFare ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Domain.Types.FareSettlement.FareSettlement -> Kernel.Prelude.Maybe Kernel.Types.Common.Meters -> Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m ())
updateEstimatedDistanceAndFare estimatedFare estimatedFareSettlement estimatedDistance id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.estimatedFare estimatedFare,
      Se.Set Beam.estimatedFareSettlement (toJSON <$> estimatedFareSettlement),
      Se.Set Beam.estimatedDistance estimatedDistance,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateIsPickupOrDestinationEdited :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m ())
updateIsPickupOrDestinationEdited isPickupOrDestinationEdited id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.isPickupOrDestinationEdited isPickupOrDestinationEdited, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
