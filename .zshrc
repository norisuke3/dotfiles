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
setopt PROMPT_SUBST         # プロンプト表示時に$VARや$(...)を展開する

local RED=$'%{\e[1;31m%}'
local GREEN=$'%{\e[1;32m%}'
local YELLOW=$'%{\e[1;33m%}'
local BLUE=$'%{\e[1;34m%}'
local CYAN=$'%{\e[1;36m%}'
local DEFAULT=$'%{\e[1;m%}'

local PCOLOR=$GREEN

autoload -Uz vcs_info       # vsc_info: git status を反映する。(左プロンプトで使用)
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "%F{yellow}!"
zstyle ':vcs_info:git:*' unstagedstr "%F{red}+"
zstyle ':vcs_info:*' formats "(%F{green}%b%c%u$PCOLOR)"
zstyle ':vcs_info:*' actionformats '[%b|%a]'
precmd () { vcs_info }

local _HOST=$HOST
if [[ "$HOST" == "pi" ]]; then
  _HOST="pi-server"
fi

# prompt parts
local _USER_PART="${PCOLOR}[${USER[1,4]}@"
local _HOST_PART="${YELLOW}${_HOST}${PCOLOR}]"
local _VCS_PART='${vcs_info_msg_0_}'   # ← これは表示時に展開させたいのでクォート
local _SIGN_PART='%(!.#.$) '
local _RESET_PART="${DEFAULT}"

# 左プロンプト
PROMPT="${_USER_PART}${_HOST_PART}${_VCS_PART}${PCOLOR}${_SIGN_PART}${_RESET_PART}"
# 右プロンプト
RPROMPT=$RED'[%~]'$DEFAULT

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

# load .zshrc.bindkeys
if [[ -f ~/.zshrc.bindkeys ]]; then
    source ~/.zshrc.bindkeys
fi

# load .zshrc.local
if [[ -f ~/.zshrc.local ]]; then
    source ~/.zshrc.local
fi

# zprof
# Uncomment the first line for showing profile on login
if (which zprof > /dev/null 2>&1) ;then
  zprof
fi

# zaw
# zaw cdr -  http://yagays.github.io/blog/2013/05/20/zaw-zsh/
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-max 5000
zstyle ':chpwd:*' recent-dirs-default yes
zstyle ':completion:*' recent-dirs-insert both

if [ -f "$HOME/go/src/github.com/zsh-users/zaw/zaw.zsh" ]; then
    source ~/go/src/github.com/zsh-users/zaw/zaw.zsh
else
    echo "run 'ghq get https://github.com/zsh-users/zaw.git' to use zaw"
fi
zstyle ':filter-select' case-insensitive yes # 絞り込みをcase-insensitiveに
bindkey '^h' zaw-history
bindkey '^@' zaw-cdr                         # zaw-cdrをbindkey
