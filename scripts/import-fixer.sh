#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

if [[ "${TRACE-0}" == "1" ]]; then
    set -o xtrace
fi

if [[ "${1-}" =~ ^-*h(elp)?$ ]]; then
    echo 'Usage: ./scripts/import-fixer.sh
This script attempts to fix imports from implicit (Something(..)) to explicit (Something(SomeConstructor, OtherConstructor)) based on a set of hardcoded types
'
    exit
fi

# cd "$(dirname "$0")"

declare -A constrs

constrs["Slot"]="Slot"
constrs["Value"]="Value"
constrs["Either"]="Left, Right"
constrs["PrivateKey"]="PrivateKey"
constrs["Credential"]="PubKeyHashCredential, ScriptHashCredential"
constrs["Address"]="BaseAddress, ByronAddress, EnterpriseAddress, RewardAddress, PointerAddress"
constrs["Proxy"]="Proxy"
constrs["ExUnits"]="ExUnits"
constrs["Transaction"]="Transaction"
constrs["CborBytes"]="CborBytes"
constrs["Coin"]="Coin"
constrs["Mint"]="Mint"
constrs["TransactionUnspentOutput"]="TransactionUnspentOutput"
constrs["TransactionOutput"]="TransactionOutput"
constrs["OutputDatum"]="OutputDatumHash, OutputDatum"
constrs["PoolPubKeyHash"]="PoolPubKeyHash"
constrs["Val"]="Val"
constrs["AssetClass"]="AssetClass"
constrs["NetworkId"]="MainnetId, TestnetId"
constrs["ScriptRef"]="NativeScriptRef, PlutusScriptRef"
constrs["RedeemerTag"]="Spend, Mint, Cert, Reward"
constrs["Maybe"]="Just, Nothing"
constrs["PaymentCredential"]="PaymentCredential"
constrs["StakeCredential"]="StakeCredential"
constrs["TransactionBuilderStep"]="SpendOutput, Pay, MintAsset, IssueCertificate, WithdrawStake"
constrs["OutputWitness"]="NativeScriptOutput, PlutusScriptOutput"
constrs["CredentialWitness"]="NativeScriptCredential, PlutusScriptCredential"
constrs["ScriptWitness"]="ScriptValue, ScriptReference"
constrs["DatumWitness"]="DatumValue, DatumReference"
constrs["RefInputAction"]="ReferenceInput, SpendInput"
constrs["ExpectedWitnessType"]="ScriptHashWitness, PubKeyHashWitness"
constrs["TxBuildError"]="WrongSpendWitnessType, IncorrectDatumHash, IncorrectScriptHash, WrongOutputType, WrongStakeCredentialType, DatumWitnessNotProvided, UnneededDatumWitness, UnneededDeregisterWitness, UnableToAddMints, RedeemerIndexingError, RedeemerIndexingInternalError, WrongNetworkId, ScriptHashAddressAndNoDatum, NoTransactionNetworkId"
constrs["Certificate"]="StakeRegistration, StakeDeregistration, StakeDelegation, PoolRegistration, PoolRetirement, GenesisKeyDelegation, MoveInstantaneousRewardsCert"
constrs["PlutusData"]="Constr, Map, List, Integer, Bytes"

for d in "src" "test" "examples"; do
    echo "processing $d"
    pushd "./$d"
    command=''
    for key in "${!constrs[@]}"; do
        command="$command"'s/\b'"$key"'(..)/'"$key(${constrs[$key]})"'/g;'
    done
    find -type f | grep '\.purs$' --color=never | xargs -I'{}' -exec sed -i -e "$command"  '{}'
    echo "$command"
    popd
done;
