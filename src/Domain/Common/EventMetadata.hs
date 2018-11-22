module Domain.Common.EventMetadata(ProcessUid(..), AggregateUid(..)) where

import Data.UUID

-- identifie uniquement la commande (si 2 commandes on le même ProcessUid, c'est qu'il s'agit de la même commande)
newtype ProcessUid = ProcessUid UUID deriving (Eq, Show)

-- identifie uniquement le steam d'évènement (a.k.a aggregate root en DDD)
newtype AggregateUid = AggregateUid UUID deriving (Eq, Show)
