
function show_dir_history_global_status {
    if [[ "$_per_directory_history_is_global" == "true" ]]; then
        echo "GH"
    else
        echo "LH"
    fi
}

PROMPT+="%(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}[%?]➜ %f)"
PROMPT+=' $(show_dir_history_global_status) %{$fg[cyan]%}%c%{$reset_color%} $(git_prompt_info)'

# Right prompt
#RPROMPT+="$(gcloud_ps1)"

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[blue]%}git:(%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[yellow]%}✗"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%})"

#
## Customise the aws cli ps prompt string
#

# sets the prefix of the AWS_PROFILE. Defaults to <aws:.
ZSH_THEME_AWS_PROFILE_PREFIX="%{$fg_bold[green]%}<aws"

# sets the suffix of the AWS_PROFILE. Defaults to >.
ZSH_THEME_AWS_PROFILE_SUFFIX=">%{$reset_color%}"

# sets the prefix of the AWS_REGION. Defaults to <region:.
ZSH_THEME_AWS_REGION_PREFIX="%{$fg_bold[yellow]%}<region"

# sets the suffix of the AWS_REGION. Defaults to >.
ZSH_THEME_AWS_REGION_SUFFIX=">%{$reset_color%}"

# sets the divider between ZSH_THEME_AWS_PROFILE_SUFFIX and ZSH_THEME_AWS_REGION_PREFIX. Defaults to  (single space).
ZSH_THEME_AWS_DIVIDER=" "
