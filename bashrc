# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# quick hack to disable input source change and reclaim C-SPC
ibus exit

export TERM=xterm-256color
echo fs.inotify.max_user_watches=582222 | sudo tee -a /etc/sysctl.conf && sudo sysctl -p

PATH=$PATH\:~/mongo/mongodb-linux-x86_64-2.0.2/bin ; export PATH

#export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-i386/bin/
export PATH=$PATH:/usr/lib/jvm/java-7-openjdk-i386/bin/
export PATH=$PATH:/usr/lib/postgresql/9.1/bin

export PASSWORD_STORE_DIR=/home/vic/datav/repo/pass

set -o vi
#setxkbmap -query
#setxkbmap -layout us # reset to querty for training purposes
#echo 'o.yqtxmal !;g.pf 00 o.yqtxmal !nafrgy go !kapcaby ekl'

# create script from vimdow code parsing xrandr resolutions
# xrandr --output LVDS1 --primary --mode 1600x900 --below DP2
# xrandr --output DP2 --mode 1920x1080 

#alias vi="gvim +'silent! !hooker 4 && hooker 3' +'au VimLeave * !hooker 1' "
alias stone='ls ~/inkspree/inkstone/target | grep standalone | xargs -I {} java -jar ~/inkspree/inkstone/target/"{}" '

function exhibit() {
  arg=$((($@ * 2) - 1))
  command hooker $arg
}

function em() {
  command emacsclient -a "" -c &
  command emacs "$@"
}

# sound
alias lower="amixer get Master | awk '/Mono:/ { print \$3 - 2 }' | xargs amixer set Master"
alias louder="amixer get Master | awk '/Mono:/ { print \$3 + 2 }' | xargs amixer set Master"

# screen
alias off="sudo vbetool dpms off"
alias on="sudo vbetool dpms on"
alias auto="sudo sh -c 'vbetool dpms off; read ans; vbetool dpms on'"

# brightness
alias saver="xrandr -q | awk '/connected primary/ {print \$1}' | xargs -I {} xrandr --output {} --brightness 0.6"
alias dim="xrandr -q | awk '/connected primary/ {print \$1}' | xargs -I {} xrandr --output {} --brightness 0.8"
alias light="xrandr -q | awk '/connected primary/ {print \$1}' | xargs -I {} xrandr --output {} --brightness 1"
alias bright="xrandr -q | awk '/connected primary/ {print \$1}' | xargs -I {} xrandr --output {} --brightness 1.1"
alias over="xrandr -q | awk '/connected primary/ {print \$1}' | xargs -I {} xrandr --output {} --brightness 1.3"

# letters
alias a='ls -a'
function b() {
  if [[ "$@" == '' ]]; then command git branch -v --all
  else command git branch -v --all | grep "$@"; fi
}
function B() {
  command git branch | grep "$@" | xargs git cherry -v --abbrev
}
alias c='hooker 1'
alias d="date +'[%Yw%W] %b %-e %a <%H:%M>'"
alias f='clear && git diff'
function F() {
  command clear
  if [ -z `git diff --cached` ]; then command git whatchanged --oneline
  else command git diff --cached; fi
}
alias g='git log --graph --decorate --oneline --all | less'
alias h='hooker 7'
alias l=ls
alias m='hooker 5'
alias r='hooker 9 && hooker 33'
alias s='git status'
function S() {
  command git stash save -k unstaged
  # run tests to make sure commit isnt broken
}
alias t='hooker 3'
function w() {
  command gvim -c 'e +set\ lines=24\ \|\ set\ columns=80\ \|\ vs\ ~/todo ~/tada'
}

alias | sed -E "s/^alias ([^=]+)='(.*)'$/alias \1 \2 \$*/g; s/'\\\''/'/g;" > ~/.emacs.d/eshell/alias 

alias gvim='UBUNTU_MENUPROXY= gvim'
alias ror="./ror"

# goes to the supplied folder without arguments,
# or to it's first grep matching subfolder of an argument query.
function cdgrep() {
  if [[ "$2" == '' ]]; then
    command cd "$1"
    return 1;
  fi
  declare -a target=( $(ls -a "$2" | grep "$1") )
  command cd "$2""${target[0]}"
}

function cdg() { cdgrep "$@" ~/goldfeld/; }
function cdb() { cdgrep "$@" ~/.vim/bundle/; }

DOWEDITOR=gvim

function tnt() {
  if [[ "$@" == '' ]]; then command gvim --servername TNT
  else command gvim --servername TNT --remote "$@"; fi
}

function dow() {
  if [[ "$@" == '' ]]; then
    declare -a arglist=()

    while read line; do 
      case $line in
        # if it's a file, add it to the arglist
        /*) arglist=( "${arglist[@]}" $line ) ;;
        .*) arglist=( "${arglist[@]}" $line ) ;;
        # if it's a bash command, execute the line (minus first char)
        \$*) command ${line:1} ;;
      esac
    done < ".dow"
    
    if [ "${#arglist[@]}" -ne 0 ]; then
      "${DOWEDITOR}" "${arglist[@]}"
    fi

    #i=0
    #i=$[i+1]
  fi
  return 1;
  #for i in $*; do
  #  echo $i
  #done;
  #echo "$@"
  #echo "${DOWEDITOR}"
  #echo "${EDITOR:-${VISUAL:-vi}}"
}


# If not running interactively, don't do anything
[ -z "$PS1" ] && return


# Invoke GnuPG-Agent the first time we login.

# Does `.gpg-agent-info' exist and points to a gpg-agent process accepting signals?
if [ -f $HOME/.gpg-agent-info ] && \
    kill -0 $(cut -d: -f 2 $HOME/.gpg-agent-info) 2>/dev/null
then
# Yes, `.gpg-agent.info' points to valid gpg-agent process;
    # Indicate gpg-agent process
    GPG_AGENT_INFO=$(cat $HOME/.gpg-agent-info | cut -c 16-)
else
# No, no valid gpg-agent process available;
    # Start gpg-agent
    eval $(gpg-agent --daemon --no-grab --write-env-file $HOME/.gpg-agent-info)
fi
export GPG_TTY=$(tty)
export GPG_AGENT_INFO


# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
