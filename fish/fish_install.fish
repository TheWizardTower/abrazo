#!/usr/bin/env fish

set -l conf_files alias budspencer direnv ls-color ssh-agent zz_fortune
for path_entry in $conf_files
    echo $path_entry
    if test ! -e ~/.config/fish/conf.d/"$path_entry".fish
        ln -s ~/abrazo/fish/conf.d/"$path_entry".fish ~/.config/fish/conf.d/"$path_entry".fish
    end
end

for path_entry in (ls -1 paths.d)
    echo $path_entry
    set target_file "$HOME/.config/fish/conf.d/$path_entry.fish"
    if test ! -e "$target_file"
        ln -s ~/abrazo/fish/paths.d/$path_entry "$target_file"
    end
end

if ! fish -c "fisher --help" >/dev/null
    curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish \
        | source \
        && fisher install jorgebucaran/fisher
end

for package in (cat fishfile)
    echo $package
    fisher install $package
end

# Cleanup old/dangling symlinks from previous configurations
set -l current_conf (ls paths.d conf.d 2>/dev/null | basename -s .fish)
for link in ~/.config/fish/conf.d/*.fish(.:t)
    if not contains "$link" $current_conf
        echo "Removing stale symlink: $link"
        rm "$HOME/.config/fish/conf.d/$link.fish"
    end
end

echo "Fish installation complete."
