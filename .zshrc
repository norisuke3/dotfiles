# zmodload zsh/zprof && zprof

typeset -U path PATH # PATHの重複登録防止
export LANG=ja_JP.UTF-8
bindkey -e

autoload -U compinit
compinit

HISTFILE=$HOME/.zsh-history           # 履歴をファイルに保存する
HISTSIZE=100000                       # メモリ内の履歴の数
SAVEHIST=100000                       # 保存される履歴の数
setopt extended_history               # 履歴ファイルに時刻を記録
function history-all { history -E 1 } # 全履歴の一覧を出力する

setopt hist_ignore_dups     # ignore duplication command history list
setopt hist_ignore_all_dups # the same command in the past is removed
setopt share_history        # share command history data
setopt hist_ignore_space

local RED=$'%{\e[1;31m%}'
local GREEN=$'%{\e[1;32m%}'
local YELLOW=$'%{\e[1;33m%}'
local BLUE=$'%{\e[1;34m%}'
local CYAN=$'%{\e[1;36m%}'
local DEFAULT=$'%{\e[1;m%}'

local PCOLOR=$GREEN

PROMPT=$PCOLOR'[${USER}@${HOSTNAME}]%(!.#.$) '$DEFAULT
RPROMPT=$RED'[%~]'$DEFAULT
setopt PROMPT_SUBST

# my abbrev
typeset -A myabbrev
myabbrev=(
    "ll"    "| less"
    "lg"    "| grep"
    "tx"    "tar -xvzf"
)

my-expand-abbrev() {
    local left prefix
    left=$(echo -nE "$LBUFFER" | sed -e "s/[_a-zA-Z0-9]*$//")
    prefix=$(echo -nE "$LBUFFER" | sed -e "s/.*[^_a-zA-Z0-9]\([_a-zA-Z0-9]*\)$/\1/")
    LBUFFER=$left${myabbrev[$prefix]:-$prefix}" "
}
zle -N my-expand-abbrev
bindkey     " "         my-expand-abbrev

# EXTENDED_GLOB
setopt EXTENDED_GLOB

# key bind for history search
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# setopt
setopt auto_pushd
setopt correct
setopt list_packed
setopt auto_cd

# sudo -e with Emacs
export EDITOR=emacsclient

# load .zshrc.local
if [[ -f ~/.zshrc.local ]]; then
    source ~/.zshrc.local
fi

# zprof
# Uncomment the first line for showing profile on login
if (which zprof > /dev/null 2>&1) ;then
  zprof
fi
