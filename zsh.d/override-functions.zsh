#
# Override known functions/programs
#


#
# Overrides
#

#
# Pip
#
# Do not allow wildly installing global PiP packages
function pip () {
    if [[ -z "${VIRTUAL_ENV}" ]]; then
        echo >&2 "Installing Python packages outside of a virtual environment is not allowed!"
        return 1
    fi
    pip "$@"
}
