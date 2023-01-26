#!/usr/bin/fish

if status --is-interactive
  cowsay -f dragon (fortune -a)
end

