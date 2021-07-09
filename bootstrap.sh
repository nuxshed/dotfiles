#!/usr/bin/env bash
# this script installs all the stuff required for my configs
# once i finish writing it, that is.
# alas, for now, it is left unfinished

if [[ $(uname) == "Linux" ]]; then
  echo "you are using linux"
  if [[ $(cut -d' ' -f1-2 /etc/issue) == "Arch Linux" ]]; then
    echo "you are using arch linux"
  else
    echo "there was an error determining which linux distro you use."
  fi
elif [[ $(uname) == "Darwin" ]]; then
  echo "you are using macos"
else
  echo "There was an issue while trying to find out which OS you use."
fi
