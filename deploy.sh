#!/usr/bin/env bash

# * https://blog.bloomca.me/2017/12/15/how-to-push-folder-to-github-pages.html

rm -rf gh-pages-dist
cp -r .dist gh-pages-dist
cd gh-pages-dist && rm cli web && mv web.html index.html
git init && git add .
git commit -m "Initial commit"
git remote add origin https://github.com/retho/haskell-simple-interactive-interpreter-web.git
git push --force origin master:gh-pages
