#!/usr/bin/env bash

for ii in \
        acorn\
        jshint\
        standard\
        tern\
        youtube-dl
do
    npm install $ii
    ln -s ~/node_modules/$ii/bin/$ii ~/local/bin/$ii
    ls ~/node_modules/$ii/bin/$ii
done

npm install eslint eslint-config-standard eslint-plugin-react
ln -s ~/node_modules/eslint/bin/eslint.js ~/local/bin/eslint.js

npm install js-beautify
ln -s ~/node_modules/js-beautify/js/bin/css-beautify.js  ~/local/bin/css-beautify.js
ln -s ~/node_modules/js-beautify/js/bin/html-beautify.js ~/local/bin/html-beautify.js
ln -s ~/node_modules/js-beautify/js/bin/js-beautify.js   ~/local/bin/js-beautify.js

npm install standard
ln -s ~/node_modules/standard/bin/cmd.js ~/local/bin/cmd.js

