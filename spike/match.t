# match.t
# Test the auto-matching behavior of the parser

use test_spike;

#use Data::Dumper::Simple;

plan tests => 1 * blocks() + 3 * 10;

run_tests;

__DATA__

=== TEST 1: match_re returns matched substring
--- grammar

identifier: /[A-Za-z]\w*/

--- input

match_re

--- ast
"match_re"



=== TEST 2: ditto, invalid input
--- input

232

--- ast
undef



=== TEST 3: match_str returns matched substring
--- grammar

keyword: 'procedure'
       | 'if'
       | 'while'

--- input
if
--- ast
'if'



=== TEST 4: match_str: 'while'
--- input
   while
--- ast
'while'



=== TEST 5: match_str: 'procedure'
--- input
procedure
--- ast
"procedure"



=== TEST 6: match_str, fail to match
--- input
for
--- ast
undef



=== TEST 7: concat returns last item by default
--- grammar

if_stmt: 'if' '(' /[01]/ ')' block

block: /{[^}]*}/

--- input
if (1) {
    say "ok!";
}
--- ast
q[{
    say "ok!";
}]



=== TEST 8: chained rules, if_statement
--- grammar
program: statement

statement: if_statement
         | assignment

if_statement: 'if' '(' expression ')' block

expression: /[1-9]\d+\b/

block: /{[^}]*}/

assignment: var ':=' expression

var: /[A-Za-z]\w*/

--- input
if (32) { print 'yay!' }

--- ast
"{ print 'yay!' }"



=== TEST 9: chained rules, assignment
--- input
foo := 25
--- ast
25



=== TEST 10: modifier '(s)', 3 elems
--- grammar
program: number(s)
       | ':' var(s?)
       | '=>' operator(?)

number: /[1-9]\d*/

var: /[A-Za-z]\w*/

operator: /[-+]/

--- input
1 2 3
--- ast
[1,2,3]



=== TEST 11: modifier '(s)', 1 elem
--- input
15
--- ast
[15]



=== TEST 12: modifier '(s?)', 3 elems
--- input
:foo bar baz
--- ast
[qw(foo bar baz)]



=== TEST 13: modifier '(s?)', 1 elem
--- input
:yay5
--- ast
['yay5']



=== TEST 14: modifier '(s?)', 0 elem
--- input
:
--- ast
[]



=== TEST 15: modifier '(?)', 1 elem
--- input
=> +
--- ast
['+']



=== TEST 16: modifier '(?)', 0 elem
--- input
=>
--- ast
[]



=== TEST 17: modifier '(s /../)', 3 elems
--- grammar
program: number(s /;/)
       | ':' var(s? /,/)

number: /[1-9]\d*/

var: /[A-Za-z]\w*/

--- input
13; 25 ; 37
--- ast
[13,25,37]



=== TEST 18: modifier '(s /../)', 1 elem
--- input
13
--- ast
[13]



=== TEST 19: modifier '(s /../)', 0 elem
--- input
--- ast
undef



=== TEST 20: modifier '(s? /../)', 5 elems
--- input
:cat, bird , dog,pig, man
--- ast
[qw( cat bird dog pig man )]



=== TEST 21: modifier '(s? /../)', 1 elem
--- input
 : clover
--- ast
['clover']



=== TEST 22: modifier '(s? /../)', 0 elem
--- input
:
--- ast
[]



=== TEST 23: modifier '(s /(..)/)', 3 elems
--- grammar
program: number(s /(=>|,)/)
       | ':' var(s? /([-+])/)

number: /[1-9]\d*/

var: /[A-Za-z]\w*/

--- input
13, 25 => 37
--- ast
[13, ',', 25, '=>', 37]



=== TEST 24: modifier '(s /(..)/)', 1 elem
--- input
13
--- ast
[13]



=== TEST 25: modifier '(s /(..)/)', 0 elem
--- input
--- ast
undef



=== TEST 26: modifier '(s? /(..)/)', 5 elems
--- input
:cat+ bird - dog + pig + man
--- ast
[qw( cat + bird - dog + pig + man )]



=== TEST 27: modifier '(s? /(..)/)', 1 elem
--- input
 : clover
--- ast
['clover']



=== TEST 28: modifier '(s? /(..)/)', 0 elem
--- input
:
--- ast
[]



=== TEST 29: modifier '<leftop: a /../ b>'
--- grammar

program: expr(s)

expr: <leftop: term /[-+]/ term>

term: <leftop: factor /[*\/]/ factor>

factor: /[1-9]\d*/

--- input
31/32 + 2*25*123 - 3*5
6*5/4/2
--- ast
[
    [[31,32], [2,25,123], [3,5]],
    [[6,5,4,2]],
]



=== TEST 30: modifier '<leftop: a /(..)/ b>'
--- grammar

program: expr(s)

expr: <leftop: term /([-+])/ term>

term: <leftop: factor /([*\/])/ factor>

factor: /[1-9]\d*/

--- input
31/32 + 2*25*123 - 3*5 - 6*5/4/2
--- ast
[[ [31,'/',32], '+', [2,'*',25,'*',123], '-', [3,'*',5], '-', [6,'*',5,'/',4,'/',2] ]]



=== TEST 31: regex
--- grammar

regex: /\/(\\\/|[^\/])*\//

--- input
/\/(\\\/|[^\/])*\//
--- ast chop
<<'EOC';
/\/(\\\/|[^\/])*\//
EOC
