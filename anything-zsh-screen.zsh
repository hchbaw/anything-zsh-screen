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

## shell utility functions.
function anything-zsh-screen-recompile () {
  local z=${1}
  [[ -z ${z} ]] && z=~/.emacs.d/anything-zsh-screen.zsh
  autoload -U zrecompile
  [[ -f ${z} ]] && zrecompile -p ${z}
}
function anything-zsh-screen-clean () {
  local z=${1}
  [[ -z ${z} ]] && z=~/.emacs.d/anything-zsh-screen.zsh.zwc
  rm -f ${z}
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
  local elisp_arg=${4}
  local zle_name=${5}
  shift 5 # treat rest arguments as the screen commands.

  rm -f ${exchange}
  azs-run1 \
    ${sessionname} ${scrollback} \
    ${argv}

  azs-run-process ${elisp_arg} ${zle_name} ${exchange}
  return $?
}

function azs-run-process () {
  local elisp_arg=${1}
  local zle_name=${2}
  local exchange=${3}

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

  if (( ${ret} == 0 )); then
    local capture_start_regexp='^>' # screen side's prompt.
    [[ -z ${ANYTHING_ZSH_SCREEN_DEBUG} ]] || cp ${exchange} /tmp/azs.debug.tmp1
    perl -w -nl -i \
      -e "my \$s = \"${capture_start_regexp}\";" \
      -e 'm{$s} .. eof and not m/^$/ and print;' \
      ${exchange}

    [[ -z ${ANYTHING_ZSH_SCREEN_DEBUG} ]] || cp ${exchange} /tmp/azs.debug.tmp2
    ruby1.9 --disable-gems \
      -w -l -i \
      -e  '
        ELISP_ARG=ARGV.shift.strip
        ZLE_NAME=ARGV.shift.strip

        process = ->acc, cs, z, buf, e {
          case e
          when /\A>\s+(.+)/
            buf = $1
          when /\Acorrections \(errors:/
            z = e
          when /\A\*\s+(.+)/
            if buf != "" and buf != ELISP_ARG
              v = buf
              acc << {name:"#{ZLE_NAME}",
                      cands:[->{ "(#{v.inspect} . #{v.inspect})" }]}
              buf = ""
            end
            if cs.size != 0
              acc << {name:z, cands:cs}
            end
            cs = []
            z = $1
          when / -- /
            cs << ->{
              if z == "directory stack"
                "(#{e.inspect} . #{e.split(/ -- /).map(&:strip).last.inspect})"
              else
                "(#{e.inspect} . #{e.split(/\s+/).take(1).join().inspect})"
              end
            }
          else
            if z == "" # continuous parts running through the command line.
              buf << e
            else
              cs << ->{
                e.split(/\s+/).map {|ee|
                  "(#{ee.inspect} . #{ee.inspect})"
                }.join(" ")
              }
            end
          end
          [acc, cs, z, buf]
        }

        readlines.map(&:strip).tap {|o|
          o << "* dummy"
        }.drop(1).reduce([[], [], "", ""]) {|a, e|
          process[*a, e]
        }.tap do |o, *_|
          s = ""
          s << "("
          o.each {|e|
            s << "((name . #{e[:name].inspect})"
            s <<  "(candidates . (#{e[:cands].map(&:[]).join(" ")}))"
            s <<  "(type . anything-zsh-screen-read))"
          }
          s << ")"

          puts s
        end
      ' ${elisp_arg} ${zle_name} ${exchange}
    ret=$?
  fi
  return $ret
}
