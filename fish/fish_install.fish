#!/usr/bin/env fish

set -l conf_files alias budspencer direnv ls-color mcfly ssh-agent zz_fortune
for file in $conf_files
	echo $file
	if test ! -e ~/.config/fish/conf.d/$file.fish
		ln -s ~/abrazo/fish/conf.d/$file.fish ~/.config/fish/conf.d/$file.fish
        end
end

for path_entry in (ls -1 paths.d)
	echo $path_entry
	if test ! -e ~/.config/fish/paths.d/$file
		ln -s ~/abrazo/fish/paths.d/$file ~/.config/fish/conf.d/$file
        end
end

if ! fish -c "fisher --help" > /dev/null
	curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish \
		| source \
		&& fisher install jorgebucaran/fisher
end

for package in (cat fishfile)
	echo $package
	fisher install $package
end
