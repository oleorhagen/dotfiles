
function show_dir_history_global_status {
    if [[ "$_per_directory_history_is_global" == "true" ]]; then
        echo "GH"
    else
        echo "LH"
    fi
}

PROMPT="%(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}[$?]➜ %f)"
PROMPT+=' $(show_dir_history_global_status) %{$fg[cyan]%}%c%{$reset_color%} $(git_prompt_info)'

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[blue]%}git:(%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[yellow]%}✗"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%})"
