## screen and zsh side's functions.

# zle widget.
function anything-zsh-screen-hardcopy () {
  command screen \
    -X eval \
      "hardcopy -h ${ANYTHING_ZSH_SCREEN_EXCHANGE}" \
      'scrollback 0' \
      'stuff "^U"' \
      'stuff "cd ~/^M"' \
      'stuff "^L"'
}

# specialized startup command
function anything-zsh-screen-rc () {
  ANYTHING_ZSH_SCREEN_EXCHANGE=${1}
  if [ -z ${ANYTHING_ZSH_SCREEN_EXCHANGE} ]; then
    ANYTHING_ZSH_SCREEN_EXCHANGE=${HOME}/tmp/anything-zsh-screen-exchange
  fi

  # formatting and messages
  zstyle ':completion:*' verbose yes
  zstyle ':completion:*' list-separator '--'
  zstyle ':completion:*' group-name ''
  zstyle ':completion:*:descriptions' format '* %d'
  zstyle ':completion:*:messages' format '%d'
  zstyle ':completion:*:warnings' format 'No matches for: %d'
  zstyle ':completion:*:corrections' format '%d (errors: %e)'

  bindkey -e
  zle -N anything-zsh-screen-hardcopy
  bindkey '^X^X' anything-zsh-screen-hardcopy
  bindkey '^I' complete-word

  LISTMAX=-1
  PS1='> '
}

## emacs side's functions.
function azs-run1 () {
  local sessionname=${1}
  local scrollback=${2}
  shift 2

  command screen -x ${sessionname} \
    -X eval \
      'stuff "^U"' \
      "scrollback ${scrollback}" \
      ${argv}
}

function anything-zsh-screen-run () {
  local exchange=${1}
  local sessionname=${2}
  local scrollback=${3}
  shift 3 # treat rest arguments as the screen commands.

  rm -f ${exchange}
  azs-run1 \
    ${sessionname} ${scrollback} \
    ${argv}

  azs-run-await ${exchange}
  return $?
}

function azs-run-await () {
  local exchange=${1}

  zmodload zsh/stat
  local ret=-1
  local size=-1
  for _i in {1..20}; do
    if [[ -f ${exchange} ]]; then
      builtin stat -H h ${exchange}
      if (( ${size} == $h[size] )); then
        ret=0
        break;
      else
        size=$h[size]
      fi
    fi
    sleep 0.01
    ret=-1
  done
  return $ret
}
