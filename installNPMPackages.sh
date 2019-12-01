#!/usr/bin/env bash

pushd ~/ || exit

for ii in \
        acorn\
        jshint\
        tern\
        youtube-dl
do
    npm install $ii
    ln -s ~/node_modules/$ii/bin/$ii ~/local/bin/$ii
    ls ~/node_modules/$ii/bin/$ii
done

npm install eslint eslint-config-standard eslint-plugin-react
ln -s ~/node_modules/eslint/bin/eslint.js ~/local/bin/eslint

npm install js-beautify
ln -s ~/node_modules/js-beautify/js/bin/css-beautify.js  ~/local/bin/css-beautify
ln -s ~/node_modules/js-beautify/js/bin/html-beautify.js ~/local/bin/html-beautify
ln -s ~/node_modules/js-beautify/js/bin/js-beautify.js   ~/local/bin/js-beautify

npm install standard
ln -s ~/node_modules/standard/bin/cmd.js ~/local/bin/standard

popd || exit
