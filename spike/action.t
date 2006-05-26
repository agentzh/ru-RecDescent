# action.t
# Test user-defined actions in grammars

use strict;
use warnings;

use File::Temp qw/ tempfile /;
use Test::Base;
use Text::Balanced;
#use Data::Dumper::Simple;

plan tests => 1 * blocks() + 3 * 4;

my $pmfile;
my @pmfiles;

mkdir 'tmp' if !-d 'tmp';

my $counter = 0;
my $parser;

filters {
    ast => 'eval',
};

run {
    my $block = shift;
    my $gm = $block->grammar;
    my $input = $block->input;
    my $expected_ast = $block->ast;
    my $name = $block->name;

    if (defined $gm) {
        my ($fh, $gmfile) =
            tempfile('gm_XXXXXX', SUFFIX => '.grammar', UNLINK => 1, DIR => 'tmp');
        #warn "Grammar File: $gmfile";
        print $fh $gm;
        close $fh;
        my $class = 'Parser' . (++$counter);
        is system($^X, 'spike.pl', '-m', "-n $class", $gmfile), 0, "$name - spike.pl";
        ($pmfile = $gmfile) =~ s/\.grammar$/.pm/;
        ok -f $pmfile, "$name - $pmfile ok";
        ok require $pmfile, "$name - load module $pmfile ok";
        $parser = $class->new;
    }

    #$::RD_TRACE = 1;
    my $ast = $parser->parse($input);
    #warn Dumper($ast);
    is_deeply $ast, $expected_ast, "$name - parse tree ok";
    push @pmfiles, $pmfile;
};

for my $file (@pmfiles) {
    unlink $pmfile;
}

__DATA__

=== TEST 1: basic
--- grammar

if_stmt: 'if' cond block { [@item] }

cond: '(' /\d+/ ')' { [@item] }

block: '{' /[^}]*/ '}' { [@item] }

--- input
if (32) {
    say "hello";
}
--- ast
['if_stmt', 'if', ['cond', '(', 32, ')'], ['block', '{', qq{say "hello";\n}, '}']]



=== TEST 2: <commit> and <uncommit> are also in @item
--- grammar

if_stmt: 'if' <commit> cond block <uncommit> 'else' block { [@item] }

cond: '(' /\d+/ ')' { [@item] }

block: '{' /[^}]*/ '}' { [@item] }

--- input
if (32) {a} else {b}
--- ast
[
 'if_stmt', 'if', '<commit>', ['cond', '(', 32, ')'], ['block', '{', 'a', '}'],
 '<uncommit>', 'else', ['block', '{', 'b', '}']
]



=== TEST 3: action is also a subrule per se ($text ok)
--- grammar

regex: {Text::Balanced::extract_delimited($text,'/')}

--- input
/\/(\\\/|[^\/])*\//
--- ast chop
<<'EOC';
/\/(\\\/|[^\/])*\//
EOC



=== TEST 4: action is also a subrule per se ($text and @item ok)
--- grammar

regex: {Text::Balanced::extract_delimited($text,'/')} {[@item]}

--- input
/\/(\\\/|[^\/])*\//
--- ast
$a = <<'EOC';
/\/(\\\/|[^\/])*\//
EOC
CORE::chop $a;
['regex',$a]
