#!/usr/bin/fish

if status --is-interactive
  cowsay -f /usr/share/cowsay/tux.cow (fortune -a)
end

