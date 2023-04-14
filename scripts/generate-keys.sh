#!/usr/bin/env bash

# This script can be used to generate two private keys and print the address.
# It is based on this guide:
# https://developers.cardano.org/docs/stake-pool-course/handbook/keys-addresses/

if [[ -e payment.skey ]]; then echo 'payment.skey already exists!'; exit 1; fi
if [[ -e stake.skey ]]; then echo 'stake.skey already exists!'; exit 1; fi

# Generate a payment key
cardano-cli address key-gen \
    --verification-key-file payment.vkey \
    --signing-key-file payment.skey

# Generate a stake key
cardano-cli stake-address key-gen \
    --verification-key-file stake.vkey \
    --signing-key-file stake.skey

# Build an address
cardano-cli address build \
    --payment-verification-key-file payment.vkey \
    --stake-verification-key-file stake.vkey \
    --out-file payment.addr \
    --testnet-magic 1 # preview network, use 2 for preprod or `--mainnet` for mainnet

# Print info
echo "Created: payment.skey, payment.vkey, stake.skey, stake.vkey"
echo "Your address:"
cat payment.addr
echo
echo "Open https://docs.cardano.org/cardano-testnet/tools/faucet/ to fund it"
