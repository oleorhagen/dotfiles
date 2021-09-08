

if [ -d "$HOME/.bookmarks" ]
then
    export CDPATH=".:$HOME/.bookmarks:/"
    alias goto="cd -P"
fi
