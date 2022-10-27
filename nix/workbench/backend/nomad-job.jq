# Task to run in `nomad` using `podman` driver.
# https://www.nomadproject.io/docs/job-specification
# https://www.nomadproject.io/docs/job-specification/job
# https://github.com/hashicorp/nomad-driver-podman#task-configuration
################################################################################
          job "cluster" {
            region = "workbench"
            datacenters = [ "workbench" ]
            type = "service"
            reschedule {
              attempts = 0
              unlimited = false
            }
  # A service definition contains configuration that is applied to each
  # container started for that service, much like passing command-line
  # parameters to docker run.
  ##############################################################################
  "services": (
  ##############################################################################
    ############################
    # `tracer` profile services.
    ############################
    ({
      "tracer": {
          profiles: [ "tracer" ]
        , pull_policy: "never"
        , image: "${WB_TRACER_IMAGE_NAME:-\($tracerImageName)}:${WB_TRACER_IMAGE_TAG:-\($tracerImageTag)}"
        #############################################################
        # Using the 172.21.0.1 - 172.21.255.255 IP range for tracers.
        #############################################################
        , networks: {
          "cardano-cluster": {
            ipv4_address: "172.21.0.1"
          }
        }
        , volumes: [
          "TRACER:/var/cardano-tracer:rw"
        ]
        , environment: [
            "HOME=/var/cardano-tracer"
          , "TRACER_CONFIG=/var/cardano-tracer/config.json"
        ]
        # Ensure that these default values are used.
        , restart: "no"
        , logging: {driver: "json-file"}
      }
    })
    +
    ##########################
    # `node` profile services.
    ##########################
    (
        .
      |
        with_entries ( {
            key: .key
          , value: {
                profiles: [ "node" ]
              , pull_policy: "never"
              , image: "${WB_NODE_IMAGE_NAME:-\($nodeImageName)}:${WB_NODE_IMAGE_TAG:-\($nodeImageTag)}"
              ###########################################################
              # Using the 172.22.0.1 - 172.22.255.255 IP range for nodes.
              ###########################################################
              , networks: {
                  "cardano-cluster": {
                    ipv4_address: "172.22.\(.value.i / 254 | floor).\(.value.i % 254 + 1)"
                  }
                }
              , ports: [ "\(.value.port):\(.value.port)" ]
              , volumes: [
                    "NODE-\(.value.name):/var/cardano-node:rw"
                  , "GENESIS:/var/cardano-node/genesis:ro"
                  , "TRACER:/var/cardano-tracer:rw"
                ]
              , environment: (
                [
                    "HOST_ADDR=172.22.\(.value.i / 254 | floor).\(.value.i % 254 + 1)"
                  , "PORT=\(.value.port)"
                  , "DATA_DIR=/var/cardano-node"
                  , "NODE_CONFIG=/var/cardano-node/config.json"
                  , "NODE_TOPOLOGY=/var/cardano-node/topology.json"
                  , "DB_DIR=/var/cardano-node/run/current/node-\(.value.i)/db-testnet"
                  , "SOCKET_PATH=/var/cardano-node/node.socket"
                  , "TRACER_SOCKET_PATH=/var/cardano-tracer/tracer.socket"
                  , "RTS_FLAGS=+RTS -N2 -I0 -A16m -qg -qb --disable-delayed-os-memory-return -RTS"
                ]
                +
                (
                  if (.value.isProducer)
                  then
                    [
                        "SHELLEY_KES_KEY=/var/cardano-node/genesis/node-keys/node-kes\(.value.i).skey"
                      , "SHELLEY_VRF_KEY=/var/cardano-node/genesis/node-keys/node-vrf\(.value.i).skey"
                      , "SHELLEY_OPCERT=/var/cardano-node/genesis/node-keys/node\(.value.i).opcert"
                    ]
                  else
                    []
                  end
                )
                +
                (
                  if (.value.shutdown_on_slot_synced != null)
                  then
                    [
                        "SHUTDOWN_ON_SLOT_SYNCED=\(.value.shutdown_on_slot_synced)"
                    ]
                  else
                    []
                  end
                )
                +
                (
                  if (.value.shutdown_on_block_synced != null)
                  then
                    [
                        "SHUTDOWN_ON_BLOCK_SYNCED=\(.value.shutdown_on_block_synced)"
                    ]
                  else
                    []
                  end
                )
              )
              # Ensure that these default values are used.
              , restart: "no"
              , logging: {driver: "json-file"}
            }
        } )
    )
    +
    ###############################
    # `generator` profile services.
    ###############################
    ({
      "generator": {
          profiles: [ "generator" ]
        , pull_policy: "never"
        , image: "${WB_GENERATOR_IMAGE_NAME:-\($generatorImageName)}:${WB_GENERATOR_IMAGE_TAG:-\($generatorImageTag)}"
        ################################################################
        # Using the 172.23.0.1 - 172.23.255.255 IP range for generators.
        ################################################################
        , networks: {
          "cardano-cluster": {
            ipv4_address: "172.23.0.1"
          }
        }
        , volumes: (
          [
              "GENERATOR:/var/tx-generator:rw"
            , "GENESIS:/var/tx-generator/genesis:ro"
            , "GENESIS-utxo-keys:/var/tx-generator/genesis/utxo-keys:ro"
          ]
          +
          ( . | keys | map( "NODE-" + . + ":/var/tx-generator/" + . + ":rw" ) )
          +
          ( . | keys | map( "GENESIS:/var/tx-generator/" + . + "/genesis:ro" ) )
        )
        , environment: [
            "HOME=/var/tx-generator"
          , "TX_GEN_CONFIG=/var/tx-generator/run-script.json"
        ]
        # Ensure that these default values are used.
        , restart: "no"
        , logging: {driver: "json-file"}
      }
    })
  )
  ##############################################################################
  , "networks": {
  ##############################################################################
    "cardano-cluster": {
      # Networks and volumes defined as `external` are never removed.
        external: false
      , attachable: true
      , driver: "bridge"
      , driver_opts: {}
      , enable_ipv6: false
      , ipam: {
          driver: "default"
        , config: [{
          # Network Address:	    172.20.0.0
          # Subnet Mask:	        255.252.0.0
          # Wildcard Mask:	      0.3.255.255
          # Broadcast Address:	  172.23.255.255
          # Usable Host IP Range:	172.20.0.1 - 172.23.255.254
            subnet: "172.20.0.0/14"
          , ip_range: "172.20.0.0/14"
          , gateway: "172.20.255.254"
          , aux_addresses: {}
        }]
      }
    }
  }
  # Storage:
  # - https://docs.docker.com/storage/
  # - https://docs.docker.com/storage/volumes/
  # - https://docs.docker.com/engine/extend/legacy_plugins/#/volume-plugins
  ##############################################################################
  , volumes: (
  ##############################################################################
      {TRACER:
        {
          # Networks and volumes defined as `external` are never removed.
            external: false
          , driver: "local"
          , driver_opts: {
              type: "none"
            , o: "bind"
            , device: "${WB_RUNDIR:-./run/current}/tracer"
          }
        }
      }
    +
      (
          .
        |
          with_entries (
            {
                key: "NODE-\(.value.name)"
              , value: {
                  # Networks and volumes defined as `external` are never removed.
                    external: false
                  , driver: "local"
                  , driver_opts: {
                        type: "none"
                      , o: "bind"
                      , device: "${WB_RUNDIR:-./run/current}/\(.value.name)"
                  }
              }
            }
          )
      )
    +
      {GENESIS:
        {
          # Networks and volumes defined as `external` are never removed.
            external: false
          , driver: "local"
          , driver_opts: {
                type: "none"
              , o: "bind"
              , device: "${WB_RUNDIR:-./run/current}/genesis"
          }
        }
      }
    +
      {"GENESIS-utxo-keys":
        {
          # Networks and volumes defined as `external` are never removed.
            external: false
          , driver: "local"
          , driver_opts: {
                type: "none"
              , o: "bind"
              , device: "${WB_RUNDIR:-./run/current}/genesis/utxo-keys"
          }
        }
      }
    +
      {GENERATOR:
        {
          # Networks and volumes defined as `external` are never removed.
            external: false
          , driver: "local"
          , driver_opts: {
                type: "none"
              , o: "bind"
              , device: "${WB_RUNDIR:-./run/current}/generator"
          }
        }
      }
  )

}
