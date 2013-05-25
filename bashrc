# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

PATH=$PATH\:~/mongo/mongodb-linux-x86_64-2.0.2/bin ; export PATH

set -o vi

# letters
alias a='ls -a'
function b() {
  if [[ "$@" == '' ]]; then
    command git branch -v --all
    return 1;
  fi
  command git branch -v --all | grep "$@"
}
alias c='git diff --cached'
alias d="date +'[%Yw%W] %b %-e %a <%H:%M>'"
alias f='git diff'
alias g='git log --graph --decorate --oneline --all | less'
alias l=ls
alias s='git status'

alias gvim='UBUNTU_MENUPROXY= gvim'
alias ror="./ror"

# goes to the supplied folder without arguments,
# or to it's first grep matching subfolder of an argument query.
function cdgrep() {
  if [[ "$1" == '' ]]; then
    command cd "$2"
    return 1;
  fi
  declare -a target=( $(ls -a "$2" | grep "$1") )
  command cd "$2""${target[0]}"
}

function cdg() { cdgrep "$@" ~/goldfeld/; }
function cdb() { cdgrep "$@" ~/.vim/bundle/; }

DOWEDITOR=gvim

function t() {
  command gvim -c 'e +set\ lines=24\ \|\ set\ columns=80\ \|\ vs\ ~/todo ~/tada'
}

function tnt() {
  if [[ "$@" == '' ]]; then
    command gvim --servername TNT
    return 1;
  fi
  command gvim --servername TNT --remote "$@"
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

function git () {
	case "$PWD": in
		$HOME/goldfeld/*)
			command git -c user.email=vic@longstorm.org -c user.name="Vic Goldfeld" "$@"
			;;
		$HOME/.vim/bundle/*)
			command git -c user.email=vic@longstorm.org -c user.name="Vic Goldfeld" "$@"
			;;
		$HOME/vtex/*)
			command git -c user.email=bernardo@vtex.com.br -c user.name="Bernardo Szpilman" "$@"
			;;
		*)
			command git "$@"
			;;
	esac
}

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

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
