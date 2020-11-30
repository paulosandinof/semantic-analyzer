# semantic-analyzer

Semantic anayler that generates intermediate code for a subset of the C language.

# how to run

use these commands in this order:
* $ lex lexer.l
* $ yacc parser.y -d -v -g
* $ gcc y.tab.c -o semantic_analyser
* $ semantic_analiser test.c

alternatively you can do

    $ chmod +x compile
    $ ./compile
    $ semantic_analiser test.c
