A concrete Plutus Data schema. Morally equivalent to:  Row (Row Type). This is superfluous
   in the sense that we could simply do everything here with (RowListI (RowList Type)), which
   @PlutusSchema@s are all translated to, but this facilitates a more comprehensible syntax.
   (Conversely we could rewrite all of the RowList/RowListI machinery in terms of this, but it
   would be much more difficult to read/debug/reason about).

   Here's an example:

    data FType
      = F0
          { f0A :: BigInt
          }
      | F1
          { f1A :: Boolean
          , f1B :: Boolean
          , f1C :: Boolean
          }
      | F2
          { f2A :: BigInt
          , f2B :: FType
          }

    instance
      HasPlutusSchema FType
        ( "F0" :=
              ( "f0A" := I BigInt
              :+ PNil)
           @@ Z

        :+ "F1" :=
              ( "f1A"  := I Boolean
              :+ "f1B" := I Boolean
              :+ "f1C" := I Boolean
              :+ PNil
              )
            @@ (S Z)

        :+ "F2" :=
              (  "f2A" := I BigInt
              :+ "f2B" := I FType
              :+ PNil
              )
            @@ (S (S Z))

        :+ PNil
        )

Note that a PSchema encodes two pieces of information:

1) The index of each *constructor* of a data type. This information is encoded in the "outer" RowListI.
   In the above example, these are "F0" (Z), "F1" (S Z), and "F2" (S (S Z))

2) The correct on-chain ordering of each record field. This is encoded implicitly by the ordering of the fields in each inner list (i.e. we do not provide a specific index for record fields).
   If a constructor does not have a Record argument, we do not need to encode any information about that argument. For example, the type:

    data GType
      = G0 BigInt
      | G1 Boolean Boolean Boolean
      | G2 BigInt GType

   would have a much simpler Schema:

   instance
     HasPlutusSchema GType
       ( "G0" := PNil @@ Z
       :+ "G1" := PNil @@ (S Z)
       :+ "G2" := PNil @@ (S (S Z))
       :+ PNil)

   The sole purpose of the inner list is to ensure correct translation to and from Plutus Data for record types
   when using the generic functions in ToData/FromData. Since GType's constructors do not have record arguments,
   no additional information is needed in the inner lists.
