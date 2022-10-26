module Ctl.Internal.Wallet.Cip30.SignData where

import Prelude

import Ctl.Internal.Types.ByteArray (ByteArray)

foreign import data COSESign1Builder :: Type
foreign import newCoseSign1Builder :: Headers -> ByteArray -> COSESign1Builder 

foreign import data Headers :: Type
foreign import newHeaders :: ProtectedHeaderMap -> HeaderMap -> Headers

foreign import data HeaderMap :: Type
foreign import newHeaderMap :: HeaderMap
foreign import setAlgHeaderToEdDsa :: HeaderMap -> HeaderMap

foreign import data ProtectedHeaderMap :: Type
foreign import newProtectedHeaderMap :: HeaderMap -> ProtectedHeaderMap

