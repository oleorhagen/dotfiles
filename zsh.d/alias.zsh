
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
