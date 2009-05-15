# screen and zsh side's functions.
function anything-zsh-screen-hardcopy () {
  local exchange="${HOME}/tmp/anything-zsh-screen-exchange"
  command screen \
    -X eval \
      "hardcopy -h ${exchange}" \
      'scrollback 0' \
      'stuff "^L"'
}

function anything-zsh-screen-rc () {
  LISTMAX=65535
  zstyle ':completion:*:descriptions' format '* %d'
  zle -N anything-zsh-screen-hardcopy
  bindkey '^X^X' anything-zsh-screen-hardcopy

  PS1='> '
}

# emacs side's functions.
function anything-zsh-screen-run0 () {
  command screen -x anything-zsh-screen \
    -X eval \
      'stuff "^U"' \
      'stuff "^L"' \
      'scrollback 65535' \
      "stuff \"${1}^I^X^X\""
}

function anything-zsh-screen-run () {
  local exchange="${HOME}/tmp/anything-zsh-screen-exchange"

  rm -f ${exchange}
  anything-zsh-screen-run0 "${1}"

  zmodload zsh/stat
  local ret=-1
  local size=-1
  for _i in {1..20}; do
    if test -f ${exchange}; then
      builtin stat -H h ${exchange}
      if test $size -eq $h[size]; then
        ret=0
        break;
      else
        size=$h[size]
      fi
    fi
    sleep 0.01
    ret=-1
  done

  if test ${ret} -eq 0; then
    perl -w -nl -i \
      -e "my \$s = \"${1}\";" \
      -e '$s =~ s/\s+$//;' \
      -e 'm{^> $s} .. eof and not m/^$/ and print;' \
      ${exchange}
    ruby1.9 -w -nl -i \
      -e  '
        case $_
        when /\A>/
          next
        when /\A\*/
          next
        when / -- /
          print $_.split(/\s+/).take(1).join
        else
          print $_.split(/\s+/).join("\n")
        end
      ' ${exchange}
    ret=$?
  fi
  return $ret
}
