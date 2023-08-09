#!/usr/bin/env bash

# ensure that `.bashrc.d` files are sourced in
if [ -e "$HOME/.bashrc" ]; then 
  # we assume that if `.bashrc.d` is mentioned
  # in `$HOME/.bashrc`, then it's sourced in
  if [ -z "$(cat "$HOME/.bashrc" | grep -F bashrc.d)" ]; then 
     # if it can't pick up `.bashrc.d`, tell it to
     # source all files inside `.bashrc.d`
     echo 'for i in $(ls -A $HOME/.bashrc.d/); do source $HOME/.bashrc.d/$i; done' \
       >> "$HOME/.bashrc"
  fi
else
  # create bashrc if it doesn't exist, and tell it to source
  # all files in `.bashrc.d`
  touch "$HOME/.bashrc"
  echo 'for i in $(ls -A $HOME/.bashrc.d/); do source $HOME/.bashrc.d/$i; done' \
    > "$HOME/.bashrc"
fi