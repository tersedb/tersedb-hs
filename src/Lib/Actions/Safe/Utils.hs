module Lib.Actions.Safe.Utils where

import Control.Lens ((^.))
import Control.Monad (when)
import Lib.Types.Permission (
  CollectionPermission (Blind),
  CollectionPermissionWithExemption,
  collectionPermission,
  exemption,
 )

conditionally :: (Applicative m) => m () -> Bool -> m Bool
conditionally f t = t <$ when t f

deriveCollectionPermission
  :: CollectionPermissionWithExemption
  -> Maybe CollectionPermission
  -> CollectionPermission
deriveCollectionPermission major =
  let majorPerm = major ^. collectionPermission
      withMinor p =
        if p `min` majorPerm == Blind
          then
            if major ^. exemption
              then majorPerm
              else Blind
          else p `min` majorPerm
   in maybe majorPerm withMinor
