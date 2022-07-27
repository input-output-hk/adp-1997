#!/usr/bin/env bash
set -euxo pipefail

export CARDANO_NODE_SRC=/nix/store/3lnkjwmqhyalbz8miv1ldsqm27k303pk-source
export NUM_PRAOS_NODES=2
export NUM_POOL_NODES=1
export NUM_NODES=$(expr $NUM_PRAOS_NODES + $NUM_POOL_NODES)
export MAX_LOVELACE_SUPPLY=1000000000
mkdir -p example
cp ${CARDANO_NODE_SRC}/cardano-cli/test/data/golden/alonzo/genesis.alonzo.spec.json example/
cardano-cli genesis create --genesis-dir example --testnet-magic 42

tmp=$(mktemp)
jq --argjson MAX_LOVELACE_SUPPLY "${MAX_LOVELACE_SUPPLY}" \
   '.activeSlotsCoeff = 0.1 | .securityParam = 10 | .epochLength = 500 | .slotLength = 0.01 | .maxLovelaceSupply = $MAX_LOVELACE_SUPPLY | .protocolParams.decentralisationParam = 0.7' example/genesis.spec.json > $tmp
cp $tmp example/genesis.spec.json
jq --argjson MAX_LOVELACE_SUPPLY "${MAX_LOVELACE_SUPPLY}" \
   '.activeSlotsCoeff = 0.1 | .securityParam = 10 | .epochLength = 500 | .slotLength = 0.01 | .maxLovelaceSupply = $MAX_LOVELACE_SUPPLY | .protocolParams.decentralisationParam = 0.7' example/genesis.json > $tmp
cp $tmp example/genesis.json
# fastTestnetOptions = TestnetOptions
#   { numPraosNodes = 2
#   , numPoolNodes = 1
#   , activeSlotsCoeff = 0.1
#   , securityParam = 10
#   , epochLength = 500
#   , slotLength = 0.01
#   , maxLovelaceSupply = 1000000000
#   , enableP2P = False
#   }

cardano-cli genesis create --genesis-dir example --testnet-magic 42 --gen-genesis-keys $NUM_NODES --gen-utxo-keys $NUM_POOL_NODES

export ALL_NODES=()
for ((i=1; i <= "${NUM_POOL_NODES}"; i++))
do
    ALL_NODES+=("node-pool${i}")
done
for ((i=1; i <= "${NUM_PRAOS_NODES}"; i++))
do
    ALL_NODES+=("node-praos${i}")
done

for node in  "${ALL_NODES[@]}"
do
    mkdir "example/${node}"
done

# Make the pool operator cold keys
# This was done already for the BFT nodes as part of the genesis creation
for ((i=1; i <= "${NUM_POOL_NODES}"; i++))
do
    cardano-cli node key-gen \
      --cold-verification-key-file "example/node-pool$i/operator.vkey" \
      --cold-signing-key-file "example/node-pool$i/operator.skey" \
      --operational-certificate-issue-counter-file "example/node-pool$i/operator.counter"

    cardano-cli node key-gen-VRF \
      --verification-key-file "example/node-pool${i}/vrf.vkey" \
      --signing-key-file "example/node-pool${i}/vrf.skey"
done

# Symlink the BFT operator keys from the genesis delegates, for uniformity
for ((i=1; i <= "${NUM_PRAOS_NODES}"; i++))
do
    ln -s "../delegate-keys/delegate${i}.skey" "example/node-praos${i}/operator.skey"
    ln -s "../delegate-keys/delegate${i}.vkey" "example/node-praos${i}/operator.vkey"
    ln -s "../delegate-keys/delegate${i}.counter" "example/node-praos${i}/operator.counter"
    ln -s "../delegate-keys/delegate${i}.vrf.vkey" "example/node-praos${i}/vrf.vkey"
    ln -s "../delegate-keys/delegate${i}.vrf.skey" "example/node-praos${i}/vrf.skey"
done

# Make hot keys for all nodes
for node in "${ALL_NODES[@]}"; do
    cardano-cli node key-gen-KES \
      --verification-key-file "example/${node}/kes.vkey" \
      --signing-key-file "example/${node}/kes.skey" \

    cardano-cli node issue-op-cert \
      --kes-period 0 \
      --kes-verification-key-file "example/${node}/kes.vkey" \
      --cold-signing-key-file "example/${node}/operator.skey" \
      --operational-certificate-issue-counter-file "example/${node}/operator.counter" \
      --out-file "example/${node}/node.cert"
done

# Make topology for all nodes
export VALENCY=$(expr $NUM_NODES - 1)
for i in "${!ALL_NODES[@]}"; do
    node="${ALL_NODES[$i]}"

    # Output node port
    port="300$(expr $i + 1)"
    echo "$port" > "example/${node}/port"

    # Get list of peer ports
    peerPorts=()
    for j in "${!ALL_NODES[@]}"; do
        peer="${ALL_NODES[j]}"
        if [[ "${peer}" != "${node}" ]]; then
            peerPorts+=("300$(expr $j + 1)")
        fi
    done

    for peerPort in "${peerPorts[@]}"; do
      cat <<EOF > "example/${node}/topology.json"
{
  "Producers": []
}
EOF

      for port in "${peerPorts[@]}"; do
        tmp=$(mktemp)
        jq --argjson PEER_PORT "${port}" \
           --argjson VALENCY "${VALENCY}" \
           '.Producers += [{"addr": "127.0.0.1", "port": $PEER_PORT, "valency": $VALENCY}]' "example/${node}/topology.json" > $tmp
        cp $tmp "example/${node}/topology.json"
      done
    done
done

# Make some payment and stake addresses
# user1..n:       will own all the funds in the system, we'll set this up from
#                initial utxo the
# pool-owner1..n: will be the owner of the pools and we'll use their reward
ALL_ADDRS=()
for ((i=1; i <= "${NUM_POOL_NODES}"; i++))
do
    ALL_ADDRS+=("user${i}")
    ALL_ADDRS+=("pool-owner${i}")
done
mkdir example/addresses

for addr in "${ALL_ADDRS[@]}"; do
    # Payment address keys
    cardano-cli address key-gen \
      --verification-key-file "example/addresses/${addr}.vkey" \
      --signing-key-file "example/addresses/${addr}.skey"

    # Stake address keys
    cardano-cli stake-address key-gen \
      --verification-key-file "example/addresses/${addr}-stake.vkey" \
      --signing-key-file "example/addresses/${addr}-stake.skey" \

    # Payment addresses
    cardano-cli address build \
      --payment-verification-key-file "example/addresses/${addr}.vkey" \
      --stake-verification-key-file "example/addresses/${addr}-stake.vkey" \
      --testnet-magic 42 \
      --out-file "example/addresses/${addr}.addr"

    # Stake addresses
    cardano-cli stake-address build \
      --stake-verification-key-file "example/addresses/${addr}-stake.vkey" \
      --testnet-magic 42 \
      --out-file "example/addresses/${addr}-stake.addr"

    # Stake addresses registration certs
    cardano-cli stake-address registration-certificate \
      --stake-verification-key-file "example/addresses/${addr}-stake.vkey" \
      --out-file "example/addresses/${addr}-stake.reg.cert"
done

# Stake address delegation certificates
for ((i=1; i <= "${NUM_POOL_NODES}"; i++)); do
    cardano-cli stake-address delegation-certificate \
      --stake-verification-key-file "example/addresses/user$i-stake.vkey" \
      --cold-verification-key-file "example/node-pool$i/operator.vkey" \
      --out-file "example/addresses/user$i-stake.deleg.cert"

    ln -s "../addresses/pool-owner$i-stake.vkey" "example/node-pool$i/owner.vkey"
    ln -s "../addresses/pool-owner$i-stake.skey" "example/node-pool$i/owner.skey"
done

# Make stake pool registration cert
for ((i=1; i <= "${NUM_POOL_NODES}"; i++)); do
  cardano-cli stake-pool registration-certificate \
    --testnet-magic 42 \
    --pool-pledge 0 \
    --pool-cost 0 \
    --pool-margin 0 \
    --cold-verification-key-file "example/node-pool$i/operator.vkey" \
    --vrf-verification-key-file "example/node-pool$i/vrf.vkey" \
    --reward-account-verification-key-file "example/node-pool$i/owner.vkey" \
    --pool-owner-stake-verification-key-file "example/node-pool$i/owner.vkey" \
    --out-file "example/node-pool$i/registration.cert"
done

# Construct transaction to:
for ((i=1; i <= "${NUM_POOL_NODES}"; i++)); do
    # Transfor all funds to user n, which delegates to pool n.
    # - Register pool-owner n stake address
    # - Register stake pool n
    # - Register the user n stake address
    # - Delegate from the user n stake address to the stake pool
    GENESIS_TX_IN=$(cardano-cli genesis initial-txin \
      --testnet-magic 42 \
      --verification-key-file "example/utxo-keys/utxo$i.vkey")

    USER_ADDR=$(cat "example/addresses/user$i.addr")

    cardano-cli transaction build-raw \
      --invalid-hereafter 1000 \
      --fee 0 \
      --tx-in "${GENESIS_TX_IN}" \
      --tx-out "${USER_ADDR}+${MAX_LOVELACE_SUPPLY}" \
      --certificate-file "example/addresses/pool-owner$i-stake.reg.cert" \
      --certificate-file "example/node-pool$i/registration.cert" \
      --certificate-file "example/addresses/user$i-stake.reg.cert" \
      --certificate-file "example/addresses/user$i-stake.deleg.cert" \
      --out-file "example/tx$i.txbody"

    # Sign with:
    #  - Initial utxo spending key for the funds
    #  - User N stake address key, to the delegation cert
    #  - Pool N owner key, to register the pool
    #  - Pool N operator key, to register the pool
    cardano-cli transaction sign \
      --signing-key-file "example/utxo-keys/utxo$i.skey" \
      --signing-key-file "example/addresses/user$i-stake.skey" \
      --signing-key-file "example/node-pool$i/owner.skey" \
      --signing-key-file "example/node-pool$i/operator.skey" \
      --testnet-magic 42 \
      --tx-body-file "example/tx$i.txbody" \
      --out-file "example/tx$i.tx"
done

# Configuration for Shelley-only cluster
cp "${CARDANO_NODE_SRC}/configuration/chairman/shelley-only/configuration.yaml" "example/configuration.yaml"
chmod 666 example/configuration.yaml

echo "TestShelleyHardForkAtEpoch: 0" >> example/configuration.yaml
echo "TestAllegraHardForkAtEpoch: 0" >> example/configuration.yaml
echo "TestMaryHardForkAtEpoch: 0" >> example/configuration.yaml
echo "TestAlonzoHardForkAtEpoch: 0" >> example/configuration.yaml
echo "TestEnableDevelopmentHardForkEras: True" >> example/configuration.yaml
echo "TestEnableDevelopmentNetworkProtocols: True" >> example/configuration.yaml

sed -i example/configuration.yaml \
    -e 's/LastKnownBlockVersion-Major: 1/LastKnownBlockVersion-Major: 5/'

echo "Nodes will start in Alonzo era from epoch 0"

for node in "${ALL_NODES[@]}"; do
    DB_DIR="example/db/$node"
    NODE_STDOUT="example/$node.stdout.log"
    NODE_STDERR="example/$node.stderr.log"
    NODE_SOCKET_PATH="example/socket/$node.socket"

    mkdir -p "example/socket"

    cardano-node run \
      --config "example/configuration.yaml" \
      --topology "example/$node/topology.json" \
      --database-path "example/$node/db" \
      --shelley-kes-key "example/$node/kes.skey" \
      --shelley-vrf-key "example/$node/vrf.skey" \
      --shelley-operational-certificate "example/$node/node.cert" \
      --host-addr "127.0.0.1" \
      --port $(cat "example/$node/port") \
      --socket-path "${NODE_SOCKET_PATH}" \
      1> $NODE_STDOUT \
      2> $NODE_STDERR &
done

# exit 1

# cardano-cli node key-gen-KES \
#     --verification-key-file example/node1/kes.vkey \
#     --signing-key-file example/node1/kes.skey
# cardano-cli node key-gen-KES \
#     --verification-key-file example/node2/kes.vkey \
#     --signing-key-file example/node2/kes.skey

# # Issue operational certificates
# cardano-cli node issue-op-cert \
#     --kes-verification-key-file example/node1/kes.vkey \
#     --cold-signing-key-file example/delegate-keys/delegate1.skey \
#     --operational-certificate-issue-counter example/delegate-keys/delegate-opcert1.counter \
#     --kes-period 0 \
#     --out-file example/node1/cert
# cardano-cli node issue-op-cert \
#     --kes-verification-key-file example/node2/kes.vkey \
#     --cold-signing-key-file example/delegate-keys/delegate2.skey \
#     --operational-certificate-issue-counter example/delegate-keys/delegate-opcert2.counter \
#     --kes-period 0 \
#     --out-file example/node2/cert


# ###

# # Generate genesis keys with special governance powers
# cardano-cli genesis key-gen-genesis \
#     --verification-key-file example/genesis-keys/genesis1.vkey \
#     --signing-key-file example/genesis-keys/genesis1.skey
# cardano-cli genesis key-gen-genesis \
#     --verification-key-file example/genesis-keys/genesis2.vkey \
#     --signing-key-file example/genesis-keys/genesis2.skey

# # Generate delegate keys for each delegate node
# # These genesis delegate nodes are like stake pool nodes, but take part in the
# # BFT overlay and don't get rewards.
# cardano-cli genesis key-gen-delegate \
#     --verification-key-file example/delegate-keys/delegate1.vkey \
#     --signing-key-file example/delegate-keys/delegate1.skey \
#     --operational-certificate-issue-counter example/delegate-keys/delegate-opcert1.counter
# cardano-cli genesis key-gen-delegate \
#     --verification-key-file example/delegate-keys/delegate2.vkey \
#     --signing-key-file example/delegate-keys/delegate2.skey \
#     --operational-certificate-issue-counter example/delegate-keys/delegate-opcert2.counter

# # Generate keys for addresses which receive funds in the initial UTxO
# cardano-cli genesis key-gen-utxo \
#     --verification-key-file example/utxo-keys/utxo1.vkey \
#     --signing-key-file example/utxo-keys/utxo1.skey
# cardano-cli genesis key-gen-utxo \
#     --verification-key-file example/utxo-keys/utxo2.vkey \
#     --signing-key-file example/utxo-keys/utxo2.skey

# # Need VRF keys to prove that the node has the right to create a block in this slot.
# cardano-cli node key-gen-VRF \
#     --verification-key-file example/delegate-keys/delegate1.vrf.vkey \
#     --signing-key-file example/delegate-keys/delegate1.vrf.skey
# cardano-cli node key-gen-VRF \
#     --verification-key-file example/delegate-keys/delegate2.vrf.vkey \
#     --signing-key-file example/delegate-keys/delegate2.vrf.skey

# # Fill in genDelegs, initialFunds, startTime
# cardano-cli genesis create --genesis-dir example/ --testnet-magic 42

# cat example/genesis.json

# # Can view hashes in genesis file with:
# cardano-cli genesis key-hash \
#   --verification-key-file example/genesis-keys/genesis1.vkey
# f42b0eb14056134323d9756fa693dba5e421acaaf84fdaff922a4c0f
# cardano-cli genesis key-hash \
#   --verification-key-file example/delegate-keys/delegate1.vkey
# e446c231ace1f29eb83827f29cb4a19e4c324229d59472c8d2dbb958
# cardano-cli node key-hash-VRF \
#   --verification-key-file example/delegate-keys/delegate1.vrf.vkey

# # genDelegs is a mapping from genesis keys to genesis delegates

# # initialFunds is a mapping from initial address to the initial values at those addresses.
# cardano-cli genesis initial-addr \
#     --verification-key-file example/utxo-keys/utxo1.vkey \
#     --testnet-magic 42
# cardano-cli genesis initial-addr \
#     --verification-key-file example/utxo-keys/utxo2.vkey \
#     --testnet-magic 42

# # Create an initial supply
# cardano-cli genesis create \
#     --testnet-magic 42 \
#     --genesis-dir example/ \
#     --supply 1000000

# # Automagic method
# cardano-cli genesis create \
#     --genesis-dir example-2/ \
#     --supply 1000000000 \
#     --gen-genesis-keys 2 \
#     --gen-utxo-keys 2 \
#     --testnet-magic 42

# # Nodes
# # Cold key/Hot (operational) key
# # Cold key signs certificate saying "this is the current hot key"
# # Shelley uses two operational keys
# #
# # So in order to run a Shelley node we will need to:
# #   - generate an operator's offline key;
# #   - generate a KES operational key;
# #   - generate a VRF operational key; and
# #   - issue an operational certificate

# # genesis delegates are operator offline keys
# # The other operator offline keys are stake pool keys. For most purposes the
# # genesis delegate and stake pool operator offline keys are the same: both get
# # used to issue operational certs.

# # Create stake pool operator keys
# mkdir example/{node1,node2}

# cardano-cli node key-gen-KES \
#     --verification-key-file example/node1/kes.vkey \
#     --signing-key-file example/node1/kes.skey
# cardano-cli node key-gen-KES \
#     --verification-key-file example/node2/kes.vkey \
#     --signing-key-file example/node2/kes.skey

# # Issue operational certificates
# cardano-cli node issue-op-cert \
#     --kes-verification-key-file example/node1/kes.vkey \
#     --cold-signing-key-file example/delegate-keys/delegate1.skey \
#     --operational-certificate-issue-counter example/delegate-keys/delegate-opcert1.counter \
#     --kes-period 0 \
#     --out-file example/node1/cert
# cardano-cli node issue-op-cert \
#     --kes-verification-key-file example/node2/kes.vkey \
#     --cold-signing-key-file example/delegate-keys/delegate2.skey \
#     --operational-certificate-issue-counter example/delegate-keys/delegate-opcert2.counter \
#     --kes-period 0 \
#     --out-file example/node2/cert

# cp source/configuration/defaults/byron-mainnet/configuration.yaml
# sed -i 's/^Protocol: RealPBFT/Protocol: TPraos/' example/configuration.yaml
# sed -i 's/^minSeverity: Info/minSeverity: Debug/' example/configuration.yaml
# sed -i 's/^TraceBlockchainTime: False/TraceBlockchainTime: True/' example/configuration.yaml

# cat > example/node1/topology.json <<EOF
# {
#   "Producers": [
#     {
#       "addr": "127.0.0.1",
#       "port": 3002,
#       "valency": 1
#     }
#   ]
# }
# EOF
# cat > example/node2/topology.json <<EOF
# {
#   "Producers": [
#     {
#       "addr": "127.0.0.1",
#       "port": 3001,
#       "valency": 1
#     }
#   ]
# }
# EOF

# # Increment start time
# cardano-cli genesis create --testnet-magic 42 --genesis-dir example/

# # Start nodes
# cardano-node run \
#     --config example/configuration.yaml \
#     --topology example/node1/topology.json \
#     --database-path example/node1/db \
#     --socket-path example/node1/node.sock \
#     --shelley-kes-key example/node1/kes.skey \
#     --shelley-vrf-key example/delegate-keys/delegate1.vrf.skey \
#     --shelley-operational-certificate example/node1/cert \
#     --port 3001
# cardano-node run \
#     --config example/configuration.yaml \
#     --topology example/node2/topology.json \
#     --database-path example/node2/db \
#     --socket-path example/node2/node.sock \
#     --shelley-kes-key example/node2/kes.skey \
#     --shelley-vrf-key example/delegate-keys/delegate2.vrf.skey \
#     --shelley-operational-certificate example/node2/cert \
#     --port 3002

# export CARDANO_NODE_SOCKET_PATH=$PWD/example/node1/node.sock
# CARDANO_NODE_SOCKET_PATH=example/node1/node.sock \
#     cardano-cli query protocol-parameters \
#     --testnet-magic 42 \
#     --shelley-mode
