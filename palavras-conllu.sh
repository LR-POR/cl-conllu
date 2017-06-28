#!/bin/bash

# For preprocessing ".conllu" output from Palavras (which doesn't follows exactly the conllu specification)

# usage: ./palavras-conllu.sh [filename]

sed \
    -e 's/<ß>\|<\/ß>//' \
    -e 's/^\(\([^	]\+	\)\{8\}\)	$/\1_	_/' \
    -e 's/\$\(.\+\) \#\([0-9]\+\)->\([0-9]\)$/\2	\1	\1	PUNCT	PUNCT	_	\3	punct	_	_/' \
    $1
