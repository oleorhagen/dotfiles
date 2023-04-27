#
# Utility functions for Zsh
#


function cm () {
    echo >&2 ~/mendersoftware/*$1*
    if [ "$1" = "c" ]; then
        [ $# -lt 2 ] && echo >&2 "The c(hoice) option requires a second option" && return 1
        select dir in ~/mendersoftware/*$2*; do
            local -r _dir=$dir
            break
        done
    else
        local -r _dir=$(echo ~/mendersoftware/*$1${2:+*$2}* | cut -d' ' -f1)
    fi
    cd ${_dir}
}

function mcd() {
    mkdir -p $1
    cd $1
}

# Create a folder and move into it in one command
function mkcd() { mkdir -p "$@" && cd "$_"; }


function cdl() {
    cd $1
    ls
}

function backup() {
    cd "$1"{,.bak}
}

function gfind() {
    find / -iname $@ 2>/dev/null
}

function lfind() {
    find ${PWD} -name $@ 2>/dev/null
}

function rtfm() {
    help $@ || man $@ || ${BROWSER:?Unset browser var} "https://google.com/search?q=$@"
}

function docker-container-shell() {
    if [[ $# -ne 1 ]]; then
        cat <<-EOF >&2
        docker-container-shell requires one argument.
        Usage:
        docker-container-shell <container-search-string>
EOF
    fi
    docker exec -it $(docker ps | grep "${1}" | cut -d' ' -f1) /bin/bash
}

#
# Show the version of a mender-service in the given release version of Mender
# (Always a puzzle to figure out)
#
# TODO - add posibility to print the actual release versions, not just integration version
function menderversion () {
    if [[ $# -ne 2 ]]; then
        echo >&2 "Usage: menderversion <service> <release-version>"
    fi
    release_tool.py --version-of "$1" --in-integration-version "mendersoftware/$2"
}

function appendworkdirtopath () {
    export PATH="${PWD}:$PATH"
}

# Automatically source Python virtualenv if present
function venv() {
    if [ -d .venv ]; then
        source .venv/bin/activate
    else
        echo >&2 "No .venv directory found in the current directory"
    fi
}

function released_debian_package_version_exists() {
	if [[ $# != 1 ]]; then
		echo >&2 "Usage: released_debian_package_version_exists version-tag"
	fi
	wget https://d1b0l86ne08fsf.cloudfront.net/$1/dist-packages/debian/armhf/mender-client_$1-1_armhf.deb
}

function acceptance-tests() {
	cd ~/mendersoftware/meta-mender/tests/acceptance
}
function integration-tests() {
	cd ~/mendersoftware/integration/tests
}

function makefile-verify() {
	make --warn-undefined-variables
}

function bitbake-list-image-packages() {
  if [ $# -ne 1 ]; then
    echo >&2 "No image/package given"
  fi
  bitbake -g "$1" && cat pn-depends.dot | grep -v -e '-native' | \
      grep -v digraph | grep -v -e '-image' | awk '{print $1}' | sort | uniq
}

function bitbake() {
    # Build using Docker crops/poky and the Ubuntu image This way I don't have
    # to build locally, and have proper dependencies installed
    #
    # $@ - Passed on to the bitbake command line
    #

    # Start the container
    #
    # Give it a low priority in the scheduler (Since nicing doesn't help with
    # docker since it's not inherited from the current process)
    local -r args="$@"
    set -x
    docker run \
           --cpu-shares=256 \
           --mount 'type=bind,src=/home/olepor,dst=/home/olepor/' \
           --rm -it crops/poky:ubuntu-22.04 \
           --workdir /home/olepor/yocto/mender/ \
           /bin/bash -c "source poky/oe-init-build-env && bitbake ${args}"
    set +x

    # Write to the stdin in the container process

    echo >&2 "Done writing to the container"

    # Then write the command to it's standard input
}

function everyn () {
  if [[ $# -le 1 ]]; then
    echo >&2 "Usage: everyn seconds function"
    echo >&2 "NArgs: $#"
    return 1
  fi
  local TIME="$1"
  shift
  while :
  do
        "$@"
        sleep ${TIME}
  done
}


# Use explainshell.com to explain weird bash commands to me
function explain {
    # base url with first command already injected
    # $ explain tar
    #   => http://explainshel.com/explain/tar?args=
    url="http://explainshell.com/explain/$1?args="

    # removes $1 (tar) from arguments ($@)
    shift;

    # iterates over remaining args and adds builds the rest of the url
    for i in "$@"; do
        url=$url"$i""+"
    done

    # dump the explanation to the terminal. Strip off mumbojumbo
    w3m -dump "$url" | awk 'NR > 8 {print}'
}

function srcyocto {
    cd ~/yocto/qemu-setup
    source ./poky/oe-init-build-env build
}


#
# newest-file
#
# Lists the newest n files in the current dir
#
function newest-file {
	  ls -t . | head -"${1:-1}"
}

function inityocto() {
	  cd ~/yocto/qemu-setup/
	  source poky/oe-init-build-env
	  cd -2
}

# Switches to and from the freestanding i3-config.
function i3switchconfig() {
    (
        set -vxe
        cd ~/.config/i3/
        tmpf=$(mktemp --suffix=i3conf)
        cp config $tmpf
        cp config.bak config
        cp $tmpf config.bak
        i3-msg restart
        # rm $tmpf
    )
}

function i3multiscreensetup() {
    set -xve
    xrandr --output DP-2-2 --right-of DP-2-1 \
           --auto --output DP-2-1 --right-of eDP-1 \
           --rotate right --auto --output eDP-1 --auto
    # Move the workspaces to the correct monitors
    i3-msg '[workspace=7]' move workspace to output DP-2-1
    i3-msg '[workspace=8]' move workspace to output DP-2-2
    i3-msg '[workspace=9]' move workspace to output DP-2-2
    i3-msg '[workspace=10]' move workspace to output DP-2-2
}


function dockercontainerlog() {
    docker logs $(docker ps -a | grep $1 | awk '{print $1}')
}

function dockergetcontainerid() {
    docker ps -a | grep $1 | cut -d' ' -f 1 | pbcopy && echo >&2 "hash copied to clipboard"
}

function calc () {
	  case $# in
		    0)
			      bc
			      ;;
		    *)
			      bc -l <<< "$@"
			      ;;
	  esac
}

# shell-format
#
# $1 - the file(s) to format with shfmt
# Currently supports our mender required format.
function shell-format() { shfmt \
        -ln bash \
        -i 4 \
        -l \
        -sr \
        -ci \
        -kp \
        -bn \
        -w \
        $1
}

#
# encrypt
#
# $1 - recipient
# $2 - msg
#
function encrypt () {
    [[ $# -ne 2 ]] && { printf >&2 "encrypt\nusage:\n\t\$1 - recipient\n\t\$2 - msg"; return 1; }
    gpg --encrypt --armor --recipient "$2" "$1"
}

#
# encrypt
#
# $1 - recipient
# $2 - msg
#
function encrypt () {
    [[ $# -ne 1 ]] && { printf >&2 "decrypt\nusage:\n\t\$1 - msg\n"; return 1; }
    gpg --decrypt "$1"
}

#
# plot
#
# A wrapper around gnuplot /dev/stdin to plot sequences directly in the terminal
#
function plot () {
    gnuplot -p -e 'plot "/dev/stdin"'
}

#
# kubectl wrapper to enable kubeon if it is off
#
function kubectl () {
	if [[ "${KUBE_PS1_ENABLED}" != "on" ]]; then
		kubeon
	fi
	/usr/bin/kubectl "$@"
}

function simplestats () {
    R --no-echo -e 'x <- scan(file="stdin", quiet=TRUE); summary(x)'
}

function sum () {
    awk '
{s += $1}
END {print s}
'
}
