usage_nomad() {
     usage "nomad" "Backend:  manages a local cluster using 'nomad' (and 'podman')" <<EOF

    Please see documentation for 'wb backend' for the supported commands.

    Nomad-specific:

    service-start      RUN-DIR SERVICE
    service-stop       RUN-DIR SERVICE
    is-service-running RUN-DIR SERVICE

EOF
}

backend_nomad() {
op=${1:?$(usage_nomad)}; shift

case "$op" in

    name )
        echo 'nomad'
        ;;

    is-running )
        # Hack: Look for node-0's default port!
        test "$(sleep 0.5s; netstat -pltn 2>/dev/null | grep ':30000 ' | wc -l)" != "0"
        ;;

    setenv-defaults )
        local usage="USAGE: wb nomad $op PROFILE-DIR"
        local profile_dir=${1:?$usage}

        # Look up `supervisord` config file produced by Nix (run profile).
        setenvjqstr 'supervisord_conf' "$profile_dir"/supervisor.conf
        # The `--serverurl` argument is needed in every call to `nomad exec`.
        # The problem is that if we use "127.0.0.1:9001" as parameter (without
        # the "http" part) the container returns:
        # error: <class 'ValueError'>, Unknown protocol for serverurl 127.0.0.1:9001: file: /nix/store/izqhlj5i1x9ldyn43d02kcy4mafmj3ci-python3.9-supervisor-4.2.4/lib/python3.9/site-packages/supervisor/xmlrpc.py line: 508
        # Without using the `--serverurl` parameter at all (using INI config
        # file's [inet_http_server] port stanza) also without "http://":
        # error: <class 'socket.gaierror'>, [Errno -2] Name or service not known: file: /nix/store/hb1lzaisgx2m9n29hqhh6yp6hasplq1v-python3-3.9.10/lib/python3.9/socket.py line: 954
        # If I add the "http" part to the INI file, when starting `supervisord`
        # inside the container I get (from journald):
        # Nov 02 11:44:36 hostname cluster-18f3852f-e067-6394-8159-66a7b8da2ecc[1088457]: Error: Cannot open an HTTP server: socket.error reported -2
        # Nov 02 11:44:36 hostname cluster-18f3852f-e067-6394-8159-66a7b8da2ecc[1088457]: For help, use /nix/store/izqhlj5i1x9ldyn43d02kcy4mafmj3ci-python3.9-supervisor-4.2.4/bin/supervisord -h
        setenvjqstr 'supervisord_url' "unix:///tmp/supervisor.sock"
        # Look up `cluster` OCI image's name and tag (also Nix profile).
        setenvjqstr 'oci_image_name' ${WB_OCI_IMAGE_NAME:-$(cat "$profile_dir/clusterImageName")}
        setenvjqstr 'oci_image_tag'  ${WB_OCI_IMAGE_TAG:-$(cat  "$profile_dir/clusterImageTag")}
        # Script that creates the OCI image from nix2container layered output.
        setenvjqstr 'oci_image_skopeo_script' "$profile_dir/clusterImageCopyToPodman"
        # Can't reside inside $dir, can't use a path longer than 108 characters!
        # See: https://man7.org/linux/man-pages/man7/unix.7.html
        # char        sun_path[108];            /* Pathname */
        setenvjqstr 'podman_socket_path' "/run/user/$UID/workbench-podman.sock"
        # Set cluster's podman container defaults.
        # The workbench is expecting an specific hierarchy of folders and files.
        setenvjqstr 'container_workdir' "/tmp/cluster/"
        setenvjqstr 'container_mountpoint' "/tmp/cluster/run/current"
        # The `supervisord` binary is installed inside the container but not
        # added to $PATH (resides in /nix/store), so a desired location is
        # passed to the container as an environment variable to create a symlink
        # to it.
        setenvjqstr 'container_supervisor_nix' "/tmp/cluster/run/current/supervisor/nix-store"
        # The container need to know where `supervisord` config file is located
        # so it can be started. This is passed as an environment variable.
        setenvjqstr 'container_supervisord_conf' "/tmp/cluster/run/current/supervisor/supervisord.conf"
        # The logging level at which supervisor should write to the activity
        # log. Valid levels are trace, debug, info, warn, error and critical.
        setenvjqstr 'container_supervisord_loglevel' "info"
        ;;

    # Man pages for Podman configuration files:
    # https://man.archlinux.org/man/community/podman/podman.1.en
    # https://man.archlinux.org/man/containers.conf.5
    # https://man.archlinux.org/man/containers-storage.conf.5
    # https://man.archlinux.org/man/containers-policy.json.5

    allocate-run )
        local usage="USAGE: wb nomad $op RUN-DIR"
        local dir=${1:?$usage}; shift

        while test $# -gt 0
        do case "$1" in
               --* ) msg "FATAL:  unknown flag '$1'"; usage_docker;;
               * ) break;; esac; shift; done

        # The `genesis/utxo-keys` directory is used as a volume for the
        # `generator` service but it's not always present/created.
        if ! test -e "$dir"/genesis/utxo-keys
        then
            mkdir -p "$dir"/genesis/utxo-keys
        else
          # HACK: UGLY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          ############### FIXME: Fix it in `genesis.sh` ###############
          mv "$dir"/genesis/utxo-keys "$dir"/genesis/utxo-keys.bak
          # The `genesis/utxo-keys` directory is used as a volume for the
          # `generator` service but it's not always present/created.
          mkdir -p "$dir"/genesis/utxo-keys
          cp -r "$dir"/genesis/utxo-keys.bak/* "$dir"/genesis/utxo-keys/
        fi

        # Populate the files needed by each `supervisord` server that will run
        # for every podman container / nomad task (generator, tracer and nodes).
        # All these folder are going to be mounted on run/current/supervisor to
        # keep the supervisor.conf compatible with the supervisor backend.
        # Previously tried sharing the same folder for every supervisor but
        # difficult to debug race conditions happened (best guess is PID files).
        mkdir -p     "$dir"/generator/supervisor
        mkdir -p     "$dir"/tracer/supervisor
        for node in $(jq_tolist 'keys' "$dir"/node-specs.json)
        do
            mkdir -p "$dir"/"$node"/supervisor
        done
        local supervisord_conf=$(envjqr 'supervisord_conf')
        # These files have to be copied, not linked, because folder are going to
        # be mounted inside the container and the linked file may not be
        # accesible or may have a different path.
        cp     "$supervisord_conf" "$dir"/generator/supervisor/supervisord.conf
        cp     "$supervisord_conf" "$dir"/tracer/supervisor/supervisord.conf
        for node in $(jq_tolist 'keys' "$dir"/node-specs.json)
        do
            cp "$supervisord_conf" "$dir"/"$node"/supervisor/supervisord.conf
        done

        # Create the "cluster" OCI image.
        local oci_image_name=$(         envjqr 'oci_image_name')
        local oci_image_tag=$(          envjqr 'oci_image_tag')
        local oci_image_skopeo_script=$(envjqr 'oci_image_skopeo_script')
        msg "Creating OCI image ..."
        # TODO: for further research.
        # STORAGE_DRIVER=overlay "$oci_image_skopeo_script"
        # If podman 4.2.1 and nomad v1.3.5 this fix is not needed anymore
        # Forced the `overlay` storage driver or podman won't see the image.
        # https://docs.podman.io/en/latest/markdown/podman.1.html#note-unsupported-file-systems-in-rootless-mode
        # Error was: workbench:  FATAL: OCI image registry.workbench.iog.io/cluster:2l7wi7sh1zyp2mnl24m13ibnh2wsjvwg cannot be found by podman
        "$oci_image_skopeo_script"
        # Check that `podman` can see the "cluster" OCI image.
        if ! podman image exists "${oci_image_name}:${oci_image_tag}"
        then
            fatal "OCI image ${oci_image_name}:${oci_image_tag} cannot be found by podman"
        else
            msg "OCI image named \"${oci_image_name}:${oci_image_tag}\" created"
        fi

        # Create config files for Nomad and the Podman plugin/task driver.
        nomad_create_folders_and_config "$dir"

        # Create the Nomad job file.
        nomad_create_job_file "$dir"
        ;;

    describe-run )
        local usage="USAGE: wb nomad $op RUN-DIR"
        local dir=${1:?$usage}

        echo "  - Nomad job: $(realpath $dir)/nomad/job-cluster.hcl"
        ;;

    # Nomad-specific
    service-start )
        local usage="USAGE: wb nomad $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local service=${1:?$usage}; shift

        backend_nomad nomad-alloc-exec-supervisorctl "$dir" "$service" start "$service"
        ;;

    # Nomad-specific
    service-stop )
        local usage="USAGE: wb nomad $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local service=${1:?$usage}; shift

        backend_nomad nomad-alloc-exec-supervisorctl "$dir" "$service" stop "$service"
        ;;

    # Nomad-specific
    is-service-running )
        local usage="USAGE: wb nomad $op RUN-DIR DOCKER-SERVICE"
        local dir=${1:?$usage}; shift
        local service=${1:?$usage}; shift

        backend_nomad nomad-alloc-exec-supervisorctl "$dir" "$service" status "$service" > /dev/null && true
        ;;

    # Nomad-specific
    nomad-alloc-exec-supervisorctl )
        local usage="USAGE: wb nomad $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local task=${1:?$usage}; shift
        local action=${1:?$usage}; shift

        local nomad_alloc_id=$(envjqr 'nomad_alloc_id')
        local supervisord_url=$(envjqr 'supervisord_url')
        local container_supervisor_nix=$(envjqr 'container_supervisor_nix')
        local container_supervisord_conf=$(envjqr 'container_supervisord_conf')
        nomad alloc exec --task "$task" "$nomad_alloc_id" "$container_supervisor_nix"/bin/supervisorctl --serverurl "$supervisord_url" --configuration "$container_supervisord_conf" "$action" $@
        ;;

    start-node )
        local usage="USAGE: wb nomad $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local node=${1:?$usage}; shift

        backend_nomad service-start "$dir" $node
        # Always wait for the node to be ready.
        backend_nomad wait-node "$dir" $node
        ;;

    stop-node )
        local usage="USAGE: wb nomad $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local node=${1:?$usage}; shift

        backend_nomad service-stop "$dir" $node
        ;;

    wait-node )
        local usage="USAGE: wb nomad $op RUN-DIR [NODE-NAME]"
        local dir=${1:?$usage}; shift
        local node=${1:-$(dirname $CARDANO_NODE_SOCKET_PATH | xargs basename)}; shift
        local socket=$(backend_nomad get-node-socket-path "$dir" $node)

        local patience=$(jq '.analysis.cluster_startup_overhead_s | ceil' $dir/profile.json) i=0
        echo -n "workbench:  nomad:  waiting ${patience}s for socket of $node: " >&2
        while test ! -S $socket
        do printf "%3d" $i; sleep 1
           i=$((i+1))
           if test $i -ge $patience
           then echo
                progress "nomad" "$(red FATAL):  workbench:  nomad:  patience ran out for $(white $node) after ${patience}s, socket $socket"
                backend_nomad stop-cluster "$dir"
                fatal "$node startup did not succeed:  check logs in $(dirname $socket)/stdout & stderr"
           fi
           echo -ne "\b\b\b"
        done >&2
        echo " $node up (${i}s)" >&2
        ;;

    start-nodes )
        local usage="USAGE: wb nomad $op RUN-DIR [HONOR_AUTOSTART=]"
        local dir=${1:?$usage}; shift
        local honor_autostart=${1:-}

        local nodes=($(jq_tolist keys "$dir"/node-specs.json))
        for node in ${nodes[*]}
        do
            if test -n "$honor_autostart"
            then
                if jqtest ".\"$node\".autostart" "$dir"/node-specs.json
                then
                    backend_nomad start-node "$dir" "$node"
                fi
            else
                backend_nomad start-node "$dir" "$node"
            fi
        done

        if test ! -v CARDANO_NODE_SOCKET_PATH
        then export  CARDANO_NODE_SOCKET_PATH=$(backend_nomad get-node-socket-path "$dir" 'node-0')
        fi
        ;;

    start )
        local usage="USAGE: wb nomad $op RUN-DIR"
        local dir=${1:?$usage}; shift

        msg "Preparing podman API service for nomad driver \`nomad-driver-podman\` ..."
        nomad_start_podman_service "$dir"

        # Start `nomad` agent".
        msg "Starting nomad agent ..."
        # The Nomad agent is a long running process which runs on every machine
        # that is part of the Nomad cluster. The behavior of the agent depends
        # on if it is running in client or server mode. Clients are responsible
        # for running tasks, while servers are responsible for managing the
        # cluster.
        #
        # The Nomad agent supports multiple configuration files, which can be
        # provided using the -config CLI flag. The flag can accept either a file
        # or folder. In the case of a folder, any .hcl and .json files in the
        # folder will be loaded and merged in lexicographical order. Directories
        # are not loaded recursively.
        #   -config=<path>
        # The path to either a single config file or a directory of config files
        # to use for configuring the Nomad agent. This option may be specified
        # multiple times. If multiple config files are used, the values from
        # each will be merged together. During merging, values from files found
        # later in the list are merged over values from previously parsed file.
        #
        # Running a dual-role agent (client + server) but not "-dev" mode.
        nomad agent -config="$dir/nomad/config" >> "$dir/nomad/stdout" 2>> "$dir/nomad/stderr" &
        echo "$!" > "$dir/nomad/nomad.pid"
        setenvjqstr 'nomad_pid' $(cat $dir/nomad/nomad.pid)
        msg "Nomad started with PID $(cat $dir/nomad/nomad.pid)"

        # Wait for nomad agent:
        msg "Waiting for the listening HTTP server ..."
        local i=0
        local patience=25
        until curl -Isf 127.0.0.1:4646 2>&1 | head --lines=1 | grep --quiet "HTTP/1.1"
        do printf "%3d" $i; sleep 1
            i=$((i+1))
            if test $i -ge $patience
            then echo
                progress "nomad agent" "$(red FATAL):  workbench:  nomad agent:  patience ran out after ${patience}s, 127.0.0.1:4646"
                cat "$dir/nomad/stderr"
                backend_nomad stop-cluster "$dir"
                fatal "nomad agent startup did not succeed:  check logs"
            fi
            echo -ne "\b\b\b"
        done >&2

        msg "Starting nomad job ..."
        # Upon successful job submission, this command will immediately enter
        # an interactive monitor. This is useful to watch Nomad's internals make
        # scheduling decisions and place the submitted work onto nodes. The
        # monitor will end once job placement is done. It is safe to exit the
        # monitor early using ctrl+c.
        # On successful job submission and scheduling, exit code 0 will be
        # returned. If there are job placement issues encountered (unsatisfiable
        # constraints, resource exhaustion, etc), then the exit code will be 2.
        # Any other errors, including client connection issues or internal
        # errors, are indicated by exit code 1.
        nomad job run -verbose "$dir/nomad/job-cluster.hcl"
        # Assuming that `nomad` placement is enough wait.
        local nomad_alloc_id=$(nomad job allocs -json cluster | jq -r '.[0].ID')
        setenvjqstr 'nomad_alloc_id' "$nomad_alloc_id"
        msg "Nomad job allocation ID is: $nomad_alloc_id"
        # Show `--status` of `supervisorctl` inside the container.
        local supervisord_url=$(envjqr 'supervisord_url')
        local container_supervisor_nix=$(  envjqr 'container_supervisor_nix')
        local container_supervisord_conf=$(envjqr 'container_supervisord_conf')
        msg "Supervisor status inside container ..."
        # Print the command used for debugging purposes.
        msg "'nomad alloc exec --task node-0 \"$nomad_alloc_id\" \"$container_supervisor_nix\"/bin/supervisorctl --serverurl \"$supervisord_url\" --configuration \"$container_supervisord_conf\" status'"
        # Execute the actual command.
        nomad alloc exec --task node-0 "$nomad_alloc_id" "$container_supervisor_nix"/bin/supervisorctl --serverurl "$supervisord_url" --configuration "$container_supervisord_conf" status || true

        if jqtest ".node.tracer" "$dir"/profile.json
        then
          backend_nomad service-start "$dir" tracer
          # Wait for tracer socket
          # If tracer fails here, the rest of the cluster is brought up without
          # any problems.
          local socket=$(jq -r '.network.contents' "$dir/tracer/config.json")
          local patience=$(jq '.analysis.cluster_startup_overhead_s | ceil' "$dir/profile.json") i=0
          echo -n "workbench:  nomad:  waiting ${patience}s for socket of tracer: " >&2
          while test ! -S "$dir/tracer/$socket"
          do printf "%3d" $i; sleep 1
             i=$((i+1))
             if test $i -ge $patience
             then echo
                  progress "nomad" "$(red FATAL):  workbench:  nomad:  patience ran out for $(white tracer) after ${patience}s, socket $socket"
                  backend_nomad stop-cluster "$dir"
                  fatal "$node startup did not succeed:  check logs in $(dirname $socket)/stdout & stderr"
             fi
             echo -ne "\b\b\b"
          done >&2
          echo " tracer up (${i}s)" >&2
        fi
        ;;

    get-node-socket-path )
        local usage="USAGE: wb nomad $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}
        local node_name=${2:?$usage}

        echo -n $dir/$node_name/node.socket
        ;;

    start-generator )
        local usage="USAGE: wb nomad $op RUN-DIR"
        local dir=${1:?$usage}; shift

        while test $# -gt 0
        do case "$1" in
               --* ) msg "FATAL:  unknown flag '$1'"; usage_docker;;
               * ) break;; esac; shift; done

        backend_nomad service-start "$dir" generator
        ;;

    wait-node-stopped )
        local usage="USAGE: wb nomad $op RUN-DIR NODE"
        local dir=${1:?$usage}; shift
        local node=${1:?$usage}; shift

        progress_ne "docker" "waiting until $node stops:  ....."
        local i=0
        while backend_nomad is-service-running "$dir" "$node"
        do
          echo -ne "\b\b\b\b\b"; printf "%5d" $i >&2; i=$((i+1))
          sleep 1
        done >&2
        echo -e "\b\b\b\b\bdone, after $(with_color white $i) seconds" >&2
        ;;

    wait-pools-stopped )
        local usage="USAGE: wb nomad $op RUN-DIR"
        local dir=${1:?$usage}; shift

        local i=0 pools=$(jq .composition.n_pool_hosts $dir/profile.json) start_time=$(date +%s)
        msg_ne "nomad:  waiting until all pool nodes are stopped: 000000"
        touch $dir/flag/cluster-termination

        for ((pool_ix=0; pool_ix < $pools; pool_ix++))
        do
          while backend_nomad is-service-running "$dir" "node-${pool_ix}" && test -f $dir/flag/cluster-termination
          do
            echo -ne "\b\b\b\b\b\b"; printf "%6d" $((i + 1)); i=$((i+1))
            sleep 1
          done
          echo -ne "\b\b\b\b\b\b"; echo -n "node-${pool_ix} 000000"
        done >&2
        echo -ne "\b\b\b\b\b\b"
        local elapsed=$(($(date +%s) - start_time))
        if test -f $dir/flag/cluster-termination
        then echo " All nodes exited -- after $(yellow $elapsed)s" >&2
        else echo " Termination requested -- after $(yellow $elapsed)s" >&2; fi
        ;;

    stop-cluster )
        local usage="USAGE: wb nomad $op RUN-DIR"
        local dir=${1:?$usage}; shift
        local nomad_alloc_id=$(envjqr 'nomad_alloc_id')

        msg "Stopping generator ..."
        backend_nomad nomad-alloc-exec-supervisorctl "$dir" generator stop all || true
        msg "Stopping tracer ..."
        backend_nomad nomad-alloc-exec-supervisorctl "$dir" tracer stop all || true
        for node in $(jq_tolist 'keys' "$dir"/node-specs.json)
        do
            msg "Stopping $node ..."
            backend_nomad nomad-alloc-exec-supervisorctl "$dir" "$node" stop all || true
        done

        msg "Stopping nomad job ..."
        # FIXME:
        # ERRO[0087] Unable to get cgroup path of container: cannot get cgroup path unless container b2f4fea15a4a56591231fae10e3c3e55fd485b2c0dfb231c073e2a3c9efa0e42 is running: container is stopped
        # {"@level":"debug","@message":"Could not get container stats, unknown error","@module":"podman.podmanHandle","@timestamp":"2022-12-14T14:34:03.264133Z","driver":"podman","error":"\u0026json.SyntaxError{msg:\"unexpected end of JSON input\", Offset:0}","timestamp":"2022-12-14T14:34:03.264Z"}
        # {"@level":"debug","@message":"Could not get container stats, unknown error","@module":"podman.podmanHandle","@timestamp":"2022-12-14T14:34:16.320494Z","driver":"podman","error":"\u0026url.Error{Op:\"Get\", URL:\"http://u/v1.0.0/libpod/containers/a55f689be4d2898225c76fa12716cfa0c0dedd54a1919e82d44523a35b8d07a4/stats?stream=false\", Err:(*net.OpError)(0xc000ba5220)}","timestamp":"2022-12-14T14:34:16.320Z"}
        nomad job stop -global -no-shutdown-delay -purge -yes -verbose cluster >> "$dir/nomad/stdout" 2>> "$dir/nomad/stderr"

        local nomad_pid=$(envjqr 'nomad_pid')
        msg "Killing nomad agent (PID $nomad_pid)..."
        kill -SIGINT "$nomad_pid"
        ;;

    cleanup-cluster )
        local usage="USAGE: wb nomad $op RUN-DIR"
        local dir=${1:?$usage}; shift

        msg "nomad:  resetting cluster state in:  $dir"
        rm -f $dir/*/std{out,err} $dir/node-*/*.socket $dir/*/logs/* 2>/dev/null || true
        rm -fr $dir/node-*/state-cluster/
        # Clean nomad logs.
        rm -f $dir/nomad/nomad.log $dir/nomad/std{out,err}
        rm -rf $dir/nomad/data/*
        ;;

    * ) usage_docker;; esac
}

# Start the `podman` API service needed by `nomad`.
nomad_start_podman_service() {
    local dir=$1
    local podman_socket_path=$(envjqr 'podman_socket_path')
#    if test -S "$socket"
#    then
#        msg "Podman API service was already running"
#    else
        # The session is kept open waiting for a new connection for 60 seconds.
        # https://discuss.hashicorp.com/t/nomad-podman-rhel8-driver-difficulties/21877/4
        # `--time`: Time until the service session expires in seconds. Use 0
        # to disable the timeout (default 5).
        podman system service --time 60 "unix://$podman_socket_path" &
        local i=0
        local patience=5
        while test ! -S "$podman_socket_path"
        do printf "%3d" $i; sleep 1
            i=$((i+1))
            if test $i -ge $patience
            then echo
                progress "nomad-driver-podman" "$(red FATAL):  workbench:  nomad-driver-podman:  patience ran out after ${patience}s, socket $podman_socket_path"
                backend_nomad stop-cluster "$dir"
                fatal "nomad-driver-podman startup did not succeed:  check logs"
            fi
            echo -ne "\b\b\b"
        done >&2
#    fi
    msg "Podman API service started"
}

# Configure `nomad` and its `podman` plugin / task driver
# (Task Drivers are also called plugins because they are pluggable).
#
# WARNING: `podman`/`skopeo` are run using default parameters. Every workbench
# user is responsible for its local/global configurations.
# TODO: Unless this breaks reproducibility and with every call config files
# and parameters need to be overriden.
# For example:
# Local version of /etc/containers/containers.conf
#     mkdir -p $HOME/.config/containers/
#     touch $HOME/.config/containers/containers.conf
#     CONTAINERS_CONF=$HOME/.config/containers/containers.conf
# Local version of /etc/containers/storage.conf
# https://www.mankier.com/5/containers-storage.conf
#     mkdir -p $HOME/.local/share/containers/storage/volumes
#     touch $HOME/.config/containers/storage.conf
#     CONTAINERS_STORAGE_CONF=$HOME/.config/containers/storage.conf
# Local version of /etc/containers/policy.json
# https://www.mankier.com/5/containers-policy.json
#     mkdir -p $HOME/.config/containers/
#     touch $HOME/.config/containers/policy.json
nomad_create_folders_and_config() {
    local dir=$1
    # Folders:
    mkdir -p "$dir/nomad/config"
    mkdir -p "$dir/nomad/data"
    mkdir -p "$dir/nomad/data/plugins"
    # Podman Task Driver - Client Requirements:
    # "Ensure that Nomad can find the plugin, refer to `plugin_dir`."
    # https://www.nomadproject.io/plugins/drivers/podman#client-requirements
    ln -s "$(which nomad-driver-podman)" "$dir/nomad/data/plugins/nomad-driver-podman"
    # Config:
    # - `nomad` configuration docs:
    # - - https://developer.hashicorp.com/nomad/docs/configuration
    # - Generic `nomad` plugins / task drivers configuration docs:
    # - - https://www.nomadproject.io/plugins/drivers
    # - - https://www.nomadproject.io/docs/configuration/plugin
    # - Specific `nomad` `podman` plugin / task driver configuration docs:
    # - - https://www.nomadproject.io/plugins/drivers/podman#plugin-options
    # - - https://github.com/hashicorp/nomad-driver-podman#driver-configuration
    local podman_socket_path=$(envjqr 'podman_socket_path')
    cat > "$dir/nomad/config/nomad.hcl" <<- EOF
# Names:
########
# Specifies the region the Nomad agent is a member of. A region typically maps
# to a geographic region, for example us, with potentially multiple zones, which
# map to datacenters such as us-west and us-east.
region = "workbench"
# Specifies the data center of the local agent. All members of a datacenter
# should share a local LAN connection.
datacenter = "workbench"
# Specifies the name of the local node. This value is used to identify
# individual agents. When specified on a server, the name must be unique within
# the region.
name = "workbench"

# Paths:
########
# Specifies a local directory used to store agent state. Client nodes use this
# directory by default to store temporary allocation data as well as cluster
# information. Server nodes use this directory to store cluster state, including
# the replicated log and snapshot data. This must be specified as an absolute
# path.
data_dir  = "$dir/nomad/data"
# Specifies the directory to use for looking up plugins. By default, this is the
# top-level data_dir suffixed with "plugins", like "/opt/nomad/plugins". This
# must be an absolute path.
plugin_dir  = "$dir/nomad/data/plugins"

# Network:
##########
# Specifies which address the Nomad agent should bind to for network services,
# including the HTTP interface as well as the internal gossip protocol and RPC
# mechanism. This should be specified in IP format, and can be used to easily
# bind all network services to the same address. It is also possible to bind the
# individual services to different addresses using the "addresses" configuration
# option. Dev mode (-dev) defaults to localhost.
bind_addr = "127.0.0.1"
# Specifies the network ports used for different services required by the Nomad
# agent.
ports = {
  # The port used to run the HTTP server.
  http = 4646
  # The port used for internal RPC communication between agents and servers, and
  # for inter-server traffic for the consensus algorithm (raft).
  rpc  = 4647
  # The port used for the gossip protocol for cluster membership. Both TCP and
  # UDP should be routable between the server nodes on this port.
  serf = 4648
}
# Specifies the advertise address for individual network services. This can be
# used to advertise a different address to the peers of a server or a client
# node to support more complex network configurations such as NAT. This
# configuration is optional, and defaults to the bind address of the specific
# network service if it is not provided. Any values configured in this stanza
# take precedence over the default "bind_addr".
# If the bind address is 0.0.0.0 then the IP address of the default private
# network interface advertised. The advertise values may include an alternate
# port, but otherwise default to the port used by the bind address. The values
# support go-sockaddr/template format.
# Needed becasue of the below error message:
# "Defaulting advertise to localhost is unsafe, please set advertise manually"
advertise {
  # The address to advertise for the HTTP interface. This should be reachable by
  # all the nodes from which end users are going to use the Nomad CLI tools.
  http = "127.0.0.1:4646"
  # The address used to advertise to Nomad clients for connecting to Nomad
  # servers for RPC. This allows Nomad clients to connect to Nomad servers from
  # behind a NAT gateway. This address much be reachable by all Nomad client
  # nodes. When set, the Nomad servers will use the advertise.serf address for
  # RPC connections amongst themselves. Setting this value on a Nomad client has
  # no effect.
  rpc = "127.0.0.1:4647"
  # The address advertised for the gossip layer. This address must be reachable
  # from all server nodes. It is not required that clients can reach this
  # address. Nomad servers will communicate to each other over RPC using the
  # advertised Serf IP and advertised RPC Port.
  serf = "127.0.0.1:4648"
}
# The tls stanza configures Nomad's TLS communication via HTTP and RPC to
# enforce secure cluster communication between servers, clients, and between.
tls {
  # Specifies if TLS should be enabled on the HTTP endpoints on the Nomad agent,
  # including the API.
  http = false
  # Specifies if TLS should be enabled on the RPC endpoints and Raft traffic
  # between the Nomad servers. Enabling this on a Nomad client makes the client
  # use TLS for making RPC requests to the Nomad servers.
  rpc  = false
  # Specifies agents should require client certificates for all incoming HTTPS
  # requests. The client certificates must be signed by the same CA as Nomad.
  verify_https_client = false
  # Specifies if outgoing TLS connections should verify the server's hostname.
  verify_server_hostname = false
}

# Logging:
##########
# Specifies the verbosity of logs the Nomad agent will output. Valid log levels
# include WARN, INFO, or DEBUG in increasing order of verbosity.
log_level = "INFO"
# Output logs in a JSON format.
log_json = true
# Specifies the path for logging. If the path does not includes a filename, the
# filename defaults to nomad.log. This setting can be combined with
# "log_rotate_bytes" and "log_rotate_duration" for a fine-grained log rotation
# control.
log_file = "$dir/nomad/nomad.log"
# Specifies if the agent should log to syslog. This option only works on Unix
# based systems.
enable_syslog = false
# Specifies if the debugging HTTP endpoints should be enabled. These endpoints
# can be used with profiling tools to dump diagnostic information about Nomad's
# internals.
enable_debug = false

# Termination:
##############
# Specifies if the agent should gracefully leave when receiving the interrupt
# signal. By default, the agent will exit forcefully on any signal. This value
# should only be set to true on server agents if it is expected that a
# terminated server instance will never join the cluster again.
leave_on_interrupt = true
# Specifies if the agent should gracefully leave when receiving the terminate
# signal. By default, the agent will exit forcefully on any signal. This value
# should only be set to true on server agents if it is expected that a
# terminated server instance will never join the cluster again.
leave_on_terminate = true

# Server:
#########
# https://developer.hashicorp.com/nomad/docs/configuration/server
server {
  # Specifies if this agent should run in server mode. All other server options depend on this value being set.
  enabled = true
  # Specifies the directory to use for server-specific data, including the
  # replicated log. By default, this is the top-level "data_dir" suffixed with
  # "server", like "/opt/nomad/server". The top-level option must be set, even
  # when setting this value. This must be an absolute path.
  data_dir = "$dir/nomad/data/server"
  # Specifies the number of server nodes to wait for before bootstrapping. It is
  # most common to use the odd-numbered integers 3 or 5 for this value,
  # depending on the cluster size. A value of 1 does not provide any fault
  # tolerance and is not recommended for production use cases.
  bootstrap_expect = 1
  # Specifies the interval between the job garbage collections. Only jobs who
  # have been terminal for at least job_gc_threshold will be collected. Lowering
  # the interval will perform more frequent but smaller collections. Raising the
  # interval will perform collections less frequently but collect more jobs at a
  # time. Reducing this interval is useful if there is a large throughput of
  # tasks, leading to a large set of dead jobs. This is specified using a label
  # suffix like "30s" or "3m". job_gc_interval was introduced in Nomad 0.10.0.
  job_gc_interval = "15s"
  # Specifies the minimum time a job must be in the terminal state before it is
  # eligible for garbage collection. This is specified using a label suffix like
  # "30s" or "1h".
  job_gc_threshold = "15s"
  # Specifies if Nomad will ignore a previous leave and attempt to rejoin the
  # cluster when starting. By default, Nomad treats leave as a permanent intent
  # and does not attempt to join the cluster again when starting. This flag
  # allows the previous state to be used to rejoin the cluster.
  rejoin_after_leave = false
}

# Client:
#########
# https://developer.hashicorp.com/nomad/docs/configuration/client
client {
  enabled = true
  # Specifies the directory to use to store client state. By default, this is
  # the top-level "data_dir" suffixed with "client", like "/opt/nomad/client".
  # This must be an absolute path.
  state_dir = "$dir/nomad/data/client"
  # Specifies the maximum amount of time a job is allowed to wait to exit.
  # Individual jobs may customize their own kill timeout, but it may not exceed
  # this value.
  max_kill_timeout = "30s"
}

# Plugins:
##########
# https://developer.hashicorp.com/nomad/plugins/drivers/podman#plugin-options
plugin "nomad-driver-podman" {
  args = []
  # https://github.com/hashicorp/nomad-driver-podman#driver-configuration
  config {
    # Defaults to "unix:///run/podman/podman.sock" when running as root or a
    # cgroup V1 system, and "unix:///run/user/<USER_ID>/podman/podman.sock" for
    # rootless cgroup V2 systems.
    socket_path = "unix://$podman_socket_path"
    # Allows tasks to bind host paths (volumes) inside their container.
    volumes {
      enabled = true
    }
    # This option can be used to disable Nomad from removing a container when
    # the task exits.
    gc {
      container = true
    }
    # Allows the driver to start and reuse a previously stopped container after
    # a Nomad client restart. Consider a simple single node system and a
    # complete reboot. All previously managed containers will be reused instead
    # of disposed and recreated.
    recover_stopped = false
    # Setting this to true will disable Nomad logs collection of Podman tasks.
    # If you don't rely on nomad log capabilities and exclusively use host based
    # log aggregation, you may consider this option to disable nomad log
    # collection overhead. Beware to you also loose automatic log rotation.
    disable_log_collection = false
  }
}

# Misc:
#######
# The vault stanza configures Nomad's integration with HashiCorp's Vault. When
# configured, Nomad can create and distribute Vault tokens to tasks
# automatically. For more information on the architecture and setup, please see
# the Nomad and Vault integration documentation.
vault {
  # Specifies if the Vault integration should be activated.
  enabled = false
}
# The acl stanza configures the Nomad agent to enable ACLs and tunes various ACL
# parameters. Learn more about configuring Nomad's ACL system in the Secure
# Nomad with Access Control guide.
acl {
  # Specifies if ACL enforcement is enabled. All other ACL configuration options
  # depend on this value. Note that the Nomad command line client will send
  # requests for client endpoints such as alloc exec directly to Nomad clients
  # whenever they are accessible. In this scenario, the client will enforce
  # ACLs, so both servers and clients should have ACLs enabled.
  enabled = false
}
# The audit stanza configures the Nomad agent to configure Audit logging
# behavior. Audit logging is an Enterprise-only feature.
audit {
  # Specifies if audit logging should be enabled. When enabled, audit logging
  # will occur for every request, unless it is filtered by a filter.
  enabled = true
}
# The consul stanza configures the Nomad agent's communication with Consul for
# service discovery and key-value integration. When configured, tasks can
# register themselves with Consul, and the Nomad cluster can automatically
# bootstrap itself.
consul {
}
# Specifies if Nomad should not check for updates and security bulletins. This
# defaults to true in Nomad Enterprise.
disable_update_check = true
EOF
}

# Need to use HCL instead of JSON. The only workaround is to send commands to
# Nomad using `curl` instead of the command line (`nomad job ...`).
# - "The nomad job run command currently accepts only HCL"
# [https://github.com/hashicorp/nomad/issues/6758#issuecomment-794116722]
nomad_create_job_file() {
    local dir=$1
    # If CARDANO_MAINNET_MIRROR is present generate a list of needed volumes.
    if test -n "$CARDANO_MAINNET_MIRROR"
    then
      # The nix-store path contains 3 levels of symlinks. This is a hack to
      # avoid creating a container image with all these files.
      local immutable_store=$(readlink -f "$CARDANO_MAINNET_MIRROR"/immutable)
      local mainnet_mirror_volumes="[
          \"$CARDANO_MAINNET_MIRROR:$CARDANO_MAINNET_MIRROR:ro\"
        , \"$immutable_store:$immutable_store:ro\"
        $(find -L "$immutable_store" -type f -exec realpath {} \; | xargs dirname | sort | uniq | xargs -I "{}" echo ", \"{}:{}:ro\"")
      ]"
    else
      local mainnet_mirror_volumes="[]"
    fi
    # Create the task to run in `nomad` using `podman` driver.
    # https://www.nomadproject.io/docs/job-specification
    # https://www.nomadproject.io/docs/job-specification/job
    # https://github.com/hashicorp/nomad-driver-podman#task-configuration
cat > "$dir/nomad/job-cluster.hcl" <<- EOF
job "cluster" {
  region = "workbench"
  datacenters = [ "workbench" ]
  type = "service"
  reschedule {
    attempts = 0
    unlimited = false
  }
  # A group defines a series of tasks that should be co-located
  # on the same client (host). All tasks within a group will be
  # placed on the same host.
  group "cluster" {
    restart {
      attempts = 0
      mode = "fail"
    }
    # The network stanza specifies the networking requirements for the task
    # group, including the network mode and port allocations.
    # https://developer.hashicorp.com/nomad/docs/job-specification/network
    network {
      mode = "host"
    }
EOF
    # Hint:
    # - Working dir is: /tmp/cluster/
    # - Mount point is: /tmp/cluster/run/current
    local container_mountpoint=$(      envjqr 'container_mountpoint')
    # Nodes
    for node in $(jq_tolist 'keys' "$dir"/node-specs.json)
    do
      local task_stanza_name="$node"
      local task_stanza_file="$dir/nomad/job-cluster-task-$task_stanza_name.hcl"
      # Every node needs access to itself, "../tracer/" and "./genesis/"
      # *1 And remapping its own supervisor to the cluster/upper directory.
      # *2 And read only access to eveything else so supervisor.conf doesn't
      # complain about missing files/folders and can be shared unchanged with
      # the workbench's "supervisor" backend.
      local jq_filter="
        [
            \"${dir}/${node}/supervisor:${container_mountpoint}/supervisor:rw\"
          , \"${dir}/tracer:${container_mountpoint}/tracer:rw\"
          , \"${dir}/${node}:${container_mountpoint}/${node}:rw,exec\"
        ]
        +
        [
          \"${dir}/genesis:${container_mountpoint}/${node}/genesis:ro\"
        ]
        +
        [
          \"${dir}/generator:${container_mountpoint}/generator:ro\"
        ]
        +
        ( . | keys | map(select(. != \"${node}\")) | map(\"${dir}/\" + . + \":${container_mountpoint}/\" + . + \":ro\") )
        +
        \$mainnet_mirror_volumes
      "
      local podman_volumes=$(jq "$jq_filter" --argjson mainnet_mirror_volumes "$mainnet_mirror_volumes" "$dir"/profile/node-specs.json)
      nomad_create_task_stanza "$task_stanza_file" "$task_stanza_name" "$podman_volumes"
cat "$task_stanza_file" >> "$dir/nomad/job-cluster.hcl"
    done
    # Tracer
    local task_stanza_name_t="tracer"
    local task_stanza_file_t="$dir/nomad/job-cluster-task-$task_stanza_name_t.hcl"
    # Tracer only needs access to itself.
    # *1 And remapping its own supervisor to the cluster/upper directory.
    # *2 And read only access to eveything else so supervisor.conf doesn't
    # complain about missing files/folders and can be shared unchanged with
    # the workbench's "supervisor" backend.
    local jq_filter_t="
      [
          \"${dir}/tracer/supervisor:${container_mountpoint}/supervisor:rw\"
        , \"${dir}/tracer:${container_mountpoint}/tracer:rw\"
      ]
      +
      [
         \"${dir}/generator:${container_mountpoint}/generator:ro\"
      ]
      +
      ( . | keys | map(\"${dir}/\" + . + \":${container_mountpoint}/\" + . + \":ro\") )
    "
    local podman_volumes_t=$(jq "$jq_filter_t" "$dir"/profile/node-specs.json)
    nomad_create_task_stanza "$task_stanza_file_t" "$task_stanza_name_t" "$podman_volumes_t"
cat "$task_stanza_file_t" >> "$dir/nomad/job-cluster.hcl"
    # Generator
    local task_stanza_name_g="generator"
    local task_stanza_file_g="$dir/nomad/job-cluster-task-$task_stanza_name_g.hcl"
    # Generator needs access to itself, "./genesis", "./genesis/utxo-keys", every node inside its folder (with the genesis of every node).
    # *1 And remapping its own supervisor to the cluster/upper directory.
    # *2 And read only access to eveything else so supervisor.conf doesn't
    # complain about missing files/folders and can be shared unchanged with
    # the workbench's "supervisor" backend.
    local jq_filter_g="
      [
          \"${dir}/generator/supervisor:${container_mountpoint}/supervisor:rw\"
        , \"${dir}/tracer:${container_mountpoint}/tracer:rw\"
        , \"${dir}/generator:${container_mountpoint}/generator:rw\"
      ]
      +
      [
          \"${dir}/genesis:${container_mountpoint}/generator/genesis:ro\"
        , \"${dir}/genesis/utxo-keys:${container_mountpoint}/generator/genesis/utxo-keys:ro\"
      ]
      +
      ( . | keys | map( \"${dir}/\" + . + \":${container_mountpoint}/generator/\" + . + \":ro\" ) )
      +
      ( . | keys | map( \"${dir}/genesis:${container_mountpoint}/generator/\" + . + \"/genesis:ro\" ) )
      +
      ( . | keys | map(\"${dir}/\" + . + \":${container_mountpoint}/\" + . + \":ro\") )
    "
    local podman_volumes_g=$(jq "$jq_filter_g" "$dir"/profile/node-specs.json)
    nomad_create_task_stanza "$task_stanza_file_g" "$task_stanza_name_g" "$podman_volumes_g"
cat "$task_stanza_file_g" >> "$dir/nomad/job-cluster.hcl"
    # The end.
cat >> "$dir/nomad/job-cluster.hcl" <<- EOF
  }
}
EOF
}

nomad_create_task_stanza() {
    local file=$1
    local name=$2
    local podman_volumes=$3
    local oci_image_name=$(                envjqr 'oci_image_name')
    local oci_image_tag=$(                 envjqr 'oci_image_tag')
    local container_workdir=$(             envjqr 'container_workdir')
    local container_supervisor_nix=$(      envjqr 'container_supervisor_nix')
    local container_supervisord_conf=$(    envjqr 'container_supervisord_conf')
    local container_supervisord_loglevel=$(envjqr 'container_supervisord_loglevel')
    cat > "$file" <<- EOF
# The task stanza creates an individual unit of work, such as a
# Docker container, web application, or batch processing.
task "$name" {
  driver = "podman"
  # https://github.com/hashicorp/nomad-driver-podman#task-configuration
  config {
    # The image to run. Accepted transports are docker (default if missing),
    # oci-archive and docker-archive. Images reference as short-names will be
    # treated according to user-configured preferences.
    image = "${oci_image_name}:${oci_image_tag}"
    # Always pull the latest image on container start.
    force_pull = false
    # Podman redirects its combined stdout/stderr logstream directly to a Nomad
    # fifo. Benefits of this mode are: zero overhead, don't have to worry about
    # log rotation at system or Podman level. Downside: you cannot easily ship
    # the logstream to a log aggregator plus stdout/stderr is multiplexed into a
    # single stream.
    logging = {
      # The other option is: "journald"
      driver = "nomad"
    }
    # The hostname to assign to the container. When launching more than one of a
    # task (using count) with this option set, every container the task starts
    # will have the same hostname.
    hostname = "$name"
    network_mode = "host"
    # A list of /container_path strings for tmpfs mount points. See podman run
    # --tmpfs options for details.
    tmpfs = [
      "/tmp"
    ]
    # A list of host_path:container_path:options strings to bind host paths to
    # container paths. Named volumes are not supported.
    volumes = ${podman_volumes}
    # The working directory for the container. Defaults to the default set in
    # the image.
    working_dir = "${container_workdir}"
  }
  env = {
    SUPERVISOR_NIX = "${container_supervisor_nix}"
    SUPERVISORD_CONFIG = "${container_supervisord_conf}"
    SUPERVISORD_LOGLEVEL = "${container_supervisord_loglevel}"
  }
  # Specifies the duration to wait for an application to gracefully quit before
  # force-killing. Nomad first sends a kill_signal. If the task does not exit
  # before the configured timeout, SIGKILL is sent to the task. Note that the
  # value set here is capped at the value set for max_kill_timeout on the agent
  # running the task, which has a default value of 30 seconds.
  # Avoid: WARN[0120] StopSignal SIGTERM failed to stop container XXX in 10
  # seconds, resorting to SIGKILL
  kill_timeout = "10s"
  # Specifies a configurable kill signal for a task, where the default is SIGINT
  # (or SIGTERM for docker, or CTRL_BREAK_EVENT for raw_exec on Windows). Note
  # that this is only supported for drivers sending signals (currently docker,
  # exec, raw_exec, and java drivers).
  kill_signal = "SIGINT"
}
EOF
}

# TODO: Finally!!! I can use JSON instead of HCL: "nomad job run --json FILE"
# https://github.com/hashicorp/nomad/blob/main/CHANGELOG.md#130-may-11-2022
# https://github.com/hashicorp/nomad/pull/12591
# cat > "$dir/nomad/job-cluster.jq" <<- EOF
# {
#   "Job": {
#       "Region": "workbench"
#     , "Namespace": null
#     , "ID": "cluster"
#     , "Name": "cluster"
#     , "Type": "service"
#     , "Priority": null
#     , "AllAtOnce": null
#     , "Datacenters": [
#         "workbench"
#       ]
#     , "Constraints": null
#     , "Affinities": null
#     , "Update": null
#     , "Multiregion": null
#     , "Spreads": null
#     , "Periodic": null
#     , "ParameterizedJob": null
#     , "Reschedule": {
#           "Attempts": 0
#         , "Interval": null
#         , "Delay": null
#         , "DelayFunction": null
#         , "MaxDelay": null
#         , "Unlimited": false
#       },
#     , "Migrate": null
#     , "Meta": null
#     , "ConsulToken": null
#     , "VaultToken": null
#     , "Stop": null
#     , "ParentID": null
#     , "Dispatched": false
#     , "DispatchIdempotencyToken": null
#     , "Payload": null
#     , "ConsulNamespace": null
#     , "VaultNamespace": null
#     , "NomadTokenID": null
#     , "Status": null
#     , "StatusDescription": null
#     , "Stable": null
#     , "Version": null
#     , "SubmitTime": null
#     , "CreateIndex": null
#     , "ModifyIndex": null
#     , "JobModifyIndex": nul
#     , "TaskGroups": [
#         {
#             "Name": "cluster"
#           , "Count": null,
#           , "Constraints": null
#           , "Affinities": null
#           , "Spreads": null
#           , "Volumes": null,
#           , "RestartPolicy": {
#                 "Interval": null
#               , "Attempts": 0
#               , "Delay": null
#               , "Mode": "fail"
#             }
#           , "ReschedulePolicy": null
#           , "EphemeralDisk": null
#           , "Update": null
#           , "Migrate": null
#           , "Networks": [
#               {
#                   "Mode": "host"
#                 , "Device": ""
#                 , "CIDR": ""
#                 , "IP": ""
#                 , "DNS": null
#                 , "ReservedPorts": null
#                 , "DynamicPorts": null
#                 , "Hostname": ""
#                 , "MBits": null
#               }
#             ],
#           , "Meta": null
#           , "Services": null
#           , "ShutdownDelay": null
#           , "StopAfterClientDisconnect": null
#           , "MaxClientDisconnect": null
#           , "Scaling": null
#           , "Consul": null
#           , "Tasks": [
#             ]
#         }
#       ]
#   }
# }
# EOF

#####################################################################
# If you start nomad in "-dev" mode, this what its config looks like:
#####################################################################

# $ nomad agent-info -json
# {
#     "config": {
#         "DataDir": "",
#         "EnableSyslog": false,
#         "Files": null,
#         "HTTPAPIResponseHeaders": {},
#         "TLSConfig": {
#             "Checksum": "",
#             "EnableHTTP": false,
#             "KeyFile": "",
#             "KeyLoader": {},
#             "TLSCipherSuites": "",
#             "TLSMinVersion": "",
#             "TLSPreferServerCipherSuites": false,
#             "CAFile": "",
#             "VerifyServerHostname": false,
#             "VerifyHTTPSClient": false,
#             "EnableRPC": false,
#             "RPCUpgradeMode": false,
#             "CertFile": ""
#         },
#         "Vault": {
#             "Addr": "https://vault.service.consul:8200",
#             "ConnectionRetryIntv": 30000000000.0,
#             "Enabled": null,
#             "Namespace": "",
#             "Role": "",
#             "TLSKeyFile": "",
#             "TLSServerName": "",
#             "TLSCaPath": "",
#             "TLSSkipVerify": null,
#             "TLSCaFile": "",
#             "AllowUnauthenticated": true,
#             "TLSCertFile": "",
#             "TaskTokenTTL": "",
#             "Token": ""
#         },
#         "Addresses": {
#             "Serf": "127.0.0.1",
#             "HTTP": "127.0.0.1",
#             "RPC": "127.0.0.1"
#         },
#         "Consul": {
#             "Auth": "",
#             "ClientAutoJoin": true,
#             "KeyFile": "",
#             "Tags": null,
#             "Token": "",
#             "Addr": "127.0.0.1:8500",
#             "CAFile": "",
#             "EnableSSL": false,
#             "ServerHTTPCheckName": "Nomad Server HTTP Check",
#             "ServerRPCCheckName": "Nomad Server RPC Check",
#             "CertFile": "",
#             "ClientHTTPCheckName": "Nomad Client HTTP Check",
#             "Namespace": "",
#             "VerifySSL": true,
#             "ServerServiceName": "nomad",
#             "AllowUnauthenticated": true,
#             "AutoAdvertise": true,
#             "ChecksUseAdvertise": false,
#             "ClientServiceName": "nomad-client",
#             "GRPCAddr": "",
#             "ServerAutoJoin": true,
#             "ServerSerfCheckName": "Nomad Server Serf Check",
#             "ShareSSL": null,
#             "Timeout": 5000000000.0
#         },
#         "DevMode": true,
#         "NodeName": "",
#         "Plugins": null,
#         "Ports": {
#             "HTTP": 4646.0,
#             "RPC": 4647.0,
#             "Serf": 4648.0
#         },
#         "Client": {
#             "GCMaxAllocs": 50.0,
#             "HostNetworks": null,
#             "Servers": null,
#             "ChrootEnv": {},
#             "Enabled": true,
#             "BridgeNetworkSubnet": "",
#             "DisableRemoteExec": false,
#             "MinDynamicPort": 20000.0,
#             "AllocDir": "",
#             "BridgeNetworkName": "",
#             "Meta": {
#                 "connect.gateway_image": "envoyproxy/envoy:v${NOMAD_envoy_version}",
#                 "connect.log_level": "info",
#                 "connect.proxy_concurrency": "1",
#                 "connect.sidecar_image": "envoyproxy/envoy:v${NOMAD_envoy_version}"
#             },
#             "NetworkSpeed": 0.0,
#             "NomadServiceDiscovery": true,
#             "Artifact": {
#                 "HgTimeout": "30m",
#                 "S3Timeout": "30m",
#                 "GCSTimeout": "30m",
#                 "GitTimeout": "30m",
#                 "HTTPMaxSize": "100GB",
#                 "HTTPReadTimeout": "30m"
#             },
#             "ClientMaxPort": 14512.0,
#             "GCInterval": 600000000000.0,
#             "GCParallelDestroys": 2.0,
#             "NoHostUUID": true,
#             "ClientMinPort": 14000.0,
#             "NetworkInterface": "lo",
#             "GCInodeUsageThreshold": 99.0,
#             "MaxDynamicPort": 32000.0,
#             "NodeClass": "",
#             "Options": {
#                 "driver.raw_exec.enable": "true",
#                 "driver.docker.volumes": "true",
#                 "test.tighten_network_timeouts": "true"
#             },
#             "Reserved": {
#                 "MemoryMB": 0.0,
#                 "ReservedPorts": "",
#                 "CPU": 0.0,
#                 "Cores": "",
#                 "DiskMB": 0.0
#             },
#             "BindWildcardDefaultHostNetwork": true,
#             "CpuCompute": 0.0,
#             "MaxKillTimeout": "30s",
#             "TemplateConfig": {
#                 "ConsulRetry": null,
#                 "DisableSandbox": false,
#                 "FunctionBlacklist": null,
#                 "FunctionDenylist": [
#                     "plugin",
#                     "writeToFile"
#                 ],
#                 "NomadRetry": null,
#                 "BlockQueryWaitTime": null,
#                 "BlockQueryWaitTimeHCL": "",
#                 "MaxStale": null,
#                 "MaxStaleHCL": "",
#                 "VaultRetry": null
#             },
#             "GCDiskUsageThreshold": 99.0,
#             "HostVolumes": null,
#             "CgroupParent": "",
#             "MemoryMB": 0.0,
#             "ReserveableCores": "",
#             "ServerJoin": {
#                 "RetryInterval": 30000000000.0,
#                 "RetryJoin": [],
#                 "RetryMaxAttempts": 0.0,
#                 "StartJoin": null
#             },
#             "StateDir": "",
#             "CNIConfigDir": "/opt/cni/config",
#             "CNIPath": "/opt/cni/bin"
#         },
#         "DisableUpdateCheck": false,
#         "LeaveOnInt": false,
#         "LogRotateBytes": 0.0,
#         "EnableDebug": true,
#         "Region": "global",
#         "ACL": {
#             "Enabled": false,
#             "PolicyTTL": 30000000000.0,
#             "ReplicationToken": "",
#             "TokenTTL": 30000000000.0
#         },
#         "LogFile": "",
#         "Sentinel": {
#             "Imports": null
#         },
#         "Telemetry": {
#             "DisableDispatchedJobSummaryMetrics": false,
#             "PublishAllocationMetrics": true,
#             "StatsiteAddr": "",
#             "CirconusCheckID": "",
#             "CirconusCheckInstanceID": "",
#             "CollectionInterval": "1s",
#             "DataDogAddr": "",
#             "CirconusCheckSearchTag": "",
#             "CirconusCheckSubmissionURL": "",
#             "CirconusSubmissionInterval": "",
#             "DisableHostname": false,
#             "CirconusAPIApp": "",
#             "CirconusAPIToken": "",
#             "CirconusCheckDisplayName": "",
#             "CirconusCheckForceMetricActivation": "",
#             "PrefixFilter": null,
#             "PrometheusMetrics": true,
#             "FilterDefault": null,
#             "PublishNodeMetrics": true,
#             "CirconusAPIURL": "",
#             "CirconusBrokerID": "",
#             "CirconusBrokerSelectTag": "",
#             "CirconusCheckTags": "",
#             "DataDogTags": null,
#             "StatsdAddr": "",
#             "UseNodeName": false
#         },
#         "UI": {
#             "Vault": {
#                 "BaseUIURL": ""
#             },
#             "Consul": {
#                 "BaseUIURL": ""
#             },
#             "Enabled": true
#         },
#         "Audit": {
#             "Enabled": null,
#             "Filters": null,
#             "Sinks": null
#         },
#         "Autopilot": {
#             "CleanupDeadServers": null,
#             "DisableUpgradeMigration": null,
#             "EnableCustomUpgrades": null,
#             "EnableRedundancyZones": null,
#             "LastContactThreshold": 200000000.0,
#             "MaxTrailingLogs": 250.0,
#             "MinQuorum": 0.0,
#             "ServerStabilizationTime": 10000000000.0
#         },
#         "BindAddr": "127.0.0.1",
#         "Datacenter": "dc1",
#         "LogLevel": "DEBUG",
#         "LogRotateDuration": "",
#         "LogRotateMaxFiles": 0.0,
#         "Version": {
#             "Revision": "",
#             "Version": "1.3.5",
#             "VersionMetadata": "",
#             "VersionPrerelease": ""
#         },
#         "LeaveOnTerm": false,
#         "PluginDir": "",
#         "SyslogFacility": "LOCAL0",
#         "AdvertiseAddrs": {
#             "HTTP": "127.0.0.1:4646",
#             "RPC": "127.0.0.1:4647",
#             "Serf": "127.0.0.1:4648"
#         },
#         "DisableAnonymousSignature": true,
#         "Limits": {
#             "HTTPMaxConnsPerClient": 100.0,
#             "HTTPSHandshakeTimeout": "5s",
#             "RPCHandshakeTimeout": "5s",
#             "RPCMaxConnsPerClient": 100.0
#         },
#         "LogJson": false,
#         "Server": {
#             "MinHeartbeatTTL": 0.0,
#             "PlanRejectionTracker": {
#                 "NodeThreshold": 100.0,
#                 "NodeWindow": 300000000000.0,
#                 "Enabled": false
#             },
#             "StartJoin": [],
#             "ServerJoin": {
#                 "StartJoin": null,
#                 "RetryInterval": 30000000000.0,
#                 "RetryJoin": [],
#                 "RetryMaxAttempts": 0.0
#             },
#             "DataDir": "",
#             "DeploymentQueryRateLimit": 0.0,
#             "RaftProtocol": 3.0,
#             "RetryInterval": 0.0,
#             "RetryJoin": [],
#             "RetryMaxAttempts": 0.0,
#             "Search": {
#                 "LimitResults": 100.0,
#                 "MinTermLength": 2.0,
#                 "FuzzyEnabled": true,
#                 "LimitQuery": 20.0
#             },
#             "EnabledSchedulers": null,
#             "JobGCInterval": "",
#             "JobGCThreshold": "",
#             "RedundancyZone": "",
#             "LicenseEnv": "",
#             "UpgradeVersion": "",
#             "FailoverHeartbeatTTL": 0.0,
#             "HeartbeatGrace": 0.0,
#             "NodeGCThreshold": "",
#             "RaftBoltConfig": null,
#             "BootstrapExpect": 1.0,
#             "DeploymentGCThreshold": "",
#             "EnableEventBroker": true,
#             "EventBufferSize": 100.0,
#             "AuthoritativeRegion": "",
#             "CSIVolumeClaimGCThreshold": "",
#             "Enabled": true,
#             "MaxHeartbeatsPerSecond": 0.0,
#             "RaftMultiplier": null,
#             "RejoinAfterLeave": false,
#             "CSIPluginGCThreshold": "",
#             "EvalGCThreshold": "",
#             "NonVotingServer": false,
#             "NumSchedulers": null,
#             "DefaultSchedulerConfig": null,
#             "LicensePath": ""
#         }
#     },
#     "member": {
#         "Addr": "127.0.0.1",
#         "DelegateCur": 4,
#         "DelegateMax": 5,
#         "DelegateMin": 2,
#         "Name": "HOSTNAME.global",
#         "Port": 4648,
#         "ProtocolCur": 2,
#         "ProtocolMax": 5,
#         "ProtocolMin": 1,
#         "Status": "alive",
#         "Tags": {
#             "region": "global",
#             "build": "1.3.5",
#             "bootstrap": "1",
#             "role": "nomad",
#             "vsn": "1",
#             "raft_vsn": "3",
#             "rpc_addr": "127.0.0.1",
#             "port": "4647",
#             "dc": "dc1",
#             "expect": "1",
#             "id": "dd1798fb-cc99-cb49-fc4f-dfaa318cdaeb"
#         }
#     },
#     "stats": {
#         "raft": {
#             "commit_index": "15",
#             "protocol_version": "3",
#             "protocol_version_min": "0",
#             "snapshot_version_max": "1",
#             "num_peers": "0",
#             "term": "2",
#             "snapshot_version_min": "0",
#             "last_log_term": "2",
#             "fsm_pending": "0",
#             "applied_index": "15",
#             "latest_configuration": "[{Suffrage:Voter ID:dd1798fb-cc99-cb49-fc4f-# dfaa318cdaeb Address:127.0.0.1:4647}]",
#             "last_contact": "0",
#             "last_log_index": "15",
#             "latest_configuration_index": "0",
#             "last_snapshot_index": "0",
#             "last_snapshot_term": "0",
#             "state": "Leader",
#             "protocol_version_max": "3"
#         },
#         "serf": {
#             "members": "1",
#             "event_time": "1",
#             "health_score": "0",
#             "query_time": "1",
#             "encrypted": "false",
#             "coordinate_resets": "0",
#             "left": "0",
#             "event_queue": "0",
#             "query_queue": "0",
#             "failed": "0",
#             "member_time": "1",
#             "intent_queue": "0"
#         },
#         "runtime": {
#             "kernel.name": "linux",
#             "arch": "amd64",
#             "version": "go1.19.1",
#             "max_procs": "32",
#             "goroutines": "229",
#             "cpu_count": "32"
#         },
#         "vault": {
#             "token_next_renewal_time": "",
#             "tracked_for_revoked": "0",
#             "token_ttl": "0s",
#             "token_expire_time": "",
#             "token_last_renewal_time": ""
#         },
#         "nomad": {
#             "server": "true",
#             "leader": "true",
#             "leader_addr": "127.0.0.1:4647",
#             "bootstrap": "true",
#             "known_regions": "1"
#         },
#         "client": {
#             "known_servers": "127.0.0.1:4647",
#             "num_allocations": "0",
#             "last_heartbeat": "344.334288ms",
#             "heartbeat_ttl": "18.525481644s",
#             "node_id": "0984fe0c-ec66-947c-9591-946b228bef6a"
#         }
#     }
# }
#
# $ nomad node status -json
# [
#     {
#         "Address": "127.0.0.1",
#         "CreateIndex": 7,
#         "Datacenter": "dc1",
#         "Drain": false,
#         "Drivers": {
#             "qemu": {
#                 "Attributes": null,
#                 "Detected": false,
#                 "HealthDescription": "",
#                 "Healthy": false,
#                 "UpdateTime": "2022-12-12T19:33:29.449697777Z"
#             },
#             "exec": {
#                 "Attributes": null,
#                 "Detected": false,
#                 "HealthDescription": "Driver must run as root",
#                 "Healthy": false,
#                 "UpdateTime": "2022-12-12T19:33:29.449414346Z"
#             },
#             "java": {
#                 "Attributes": null,
#                 "Detected": false,
#                 "HealthDescription": "Driver must run as root",
#                 "Healthy": false,
#                 "UpdateTime": "2022-12-12T19:33:29.449423032Z"
#             },
#             "raw_exec": {
#                 "Attributes": {
#                     "driver.raw_exec": "true"
#                 },
#                 "Detected": true,
#                 "HealthDescription": "Healthy",
#                 "Healthy": true,
#                 "UpdateTime": "2022-12-12T19:33:29.449473176Z"
#             },
#             "mock_driver": {
#                 "Attributes": {
#                     "driver.mock": "true"
#                 },
#                 "Detected": true,
#                 "HealthDescription": "Healthy",
#                 "Healthy": true,
#                 "UpdateTime": "2022-12-12T19:33:29.449495047Z"
#             },
#             "docker": {
#                 "Attributes": null,
#                 "Detected": false,
#                 "HealthDescription": "Failed to connect to docker daemon",
#                 "Healthy": false,
#                 "UpdateTime": "2022-12-12T19:33:29.449609612Z"
#             }
#         },
#         "ID": "0984fe0c-ec66-947c-9591-946b228bef6a",
#         "LastDrain": null,
#         "ModifyIndex": 9,
#         "Name": "HOSTNAME",
#         "NodeClass": "",
#         "SchedulingEligibility": "eligible",
#         "Status": "ready",
#         "StatusDescription": "",
#         "Version": "1.3.5"
#     }
# ]
