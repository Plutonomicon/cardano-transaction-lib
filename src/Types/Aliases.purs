module Types.Aliases where

-- | IMPORTANT: All functions accepting Bech32String should behave correctly in
-- | case Bech32String is not in bech32 format.
-- | The type alias is here to make the intention of code more readable.
type Bech32String = String

-- | IMPORTANT: All functions accepting Base58String should behave correctly in
-- | case Base58String is not in expected base58 format.
-- | The type alias is here to make the intention of code more readable.
type Base58String = String
