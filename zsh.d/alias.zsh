
# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
#
# --------MENDER---------
# Source the oe-init-build-env with every new terminal-session
# source ~/poky/oe-init-build-env -- error, adds the build dir in home

# ------- Aliases -------
alias lsa="ls -a"
# Needs to have the BUILDDIR set
alias dockdev='BUILDDIR=/home/opus/ docker-compose -f docker-compose.yml -f docker-compose.storage.minio.yml -f docker-compose.demo.yml -f docker-compose.client.yml -f docker-compose.client-dev.yml run mender-client'
# add user to the integration test environment through the mender-useradm microservice
alias addusr='sudo docker-compose exec mender-useradm \
                         /usr/bin/useradm \
                         create-user \
                         --username=user@host.com \
                         --password=rootpass'
alias bb='bitbake'
alias open='xdg-open'
alias calculator='bc'

# Simple function to output whatever is on the clip-board
# to the given file.
#function pasteto(destfile) {
#
#}
#
# Aliases
if [[ -z "${WAYLAND_DISPLAY}" ]]; then
	alias pbcopy='xclip -selection clipboard'
	alias pbpaste='xclip -selection clipboard -o'
else
	alias pbcopy="wl-copy"
	alias pbpaste="wl-paste"
fi
# alias pip=pip3
alias ma='mender-artifact'

# added by travis gem
[ -f /home/olepor/.travis/travis.sh ] && source /home/olepor/.travis/travis.sh

# Alias the sourcing of this file (.zshrc)
alias zshsrc='source ~/.zshrc'

# Make sure xbindkeys is sourced
# xbindkeys --poll-rc

alias please="sudo"

alias ssh="TERM=xterm-256color ssh"
alias sshcloud="TERM=xterm-256color ssh -o UserKnownHostsFile=/dev/null"

#
# Pretty cat <3
#
#alias cat=bat

alias release_tool=release_tool.py
alias rt=release_tool.py

# pwndbg
alias pwndbg='gdb -x /usr/share/pwndbg/gdbinit.py'

# by default, only ping 3 times for good luck
alias ping='ping -c 3'

# Alias pup to htmlq
alias htmlq='pup'

# Alias to fq (binary query)
alias bq='fq'

# Always use parallell make
alias make='make --jobs=4'

# kubectl (k8s)
alias k=kubectl

# nm = names (short for)
alias names=nm

# YAML query language - yamlq
alias yamlq=yq

# binary query language - fq
alias binaryq=fq

# bitcake is much better than bitbake
alias bitcake=bitbake

# Used too often
alias fuck='sudo !!'

# 1337 echo
alias 3cho='echo'

# Cal start on monday
alias cal="cal --monday"

# Hackeriet password store alias
alias hackerpass='PASSWORD_STORE_DIR="$HOME/.hackeriet_pass" pass'

# Edit moo config
alias mooconf="vim ~/.moo/config.hcl"

# orhagen k8s
alias ko="KUBECONFIG=/home/olepor/.kube/k3s-orhagen.yaml kubectl"

# hackeriet hackerpass
alias hackerpass='PASSWORD_STORE_DIR="$HOME/.hackeriet_pass" pass'

# Stop and remove all running docker containers
alias dsr='docker stop $(docker ps -aq) && docker rm $(docker ps -aq)'

# Alias lsusb to cyme
alias lsusb='cyme'

# Alias kop (k8s for my home k8s)
alias kop='KUBECONFIG=/home/olepor/.kube/k3s-orhagen.yaml kubectl'

# Alias for login to my self-hosted postgres
PGPASSWORD="$(pass show private/home/proxmox/postgresql-postgres-account)" psql --host orhagen.no --username postgres
