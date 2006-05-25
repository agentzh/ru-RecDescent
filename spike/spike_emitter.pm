#: spike_emitter.pm
#: Simple perl code emitter for BNF
#: 2006-05-25 2006-05-25

package Spike::Emitter;

use strict;
use warnings;
use Template;
use Data::Dumper::Simple;

my $TT = Template->new;

sub emit {
    my ($self, $ast) = @_;
    #warn Dumper($ast);
    $ast = adjust_ast($ast);
    #warn Dumper($ast);
    my $buffer;
    $TT->process(\*DATA, $ast, \$buffer)
        || die $TT->error(), "\n";
    $buffer;
}

sub adjust_ast {
    my $ast = shift;
    my (%altern, %concat, %atoms);
    my $new_ast = {
        startrule   => $ast->{startrule},
        alternation => \%altern,
        concat      => \%concat,
        atoms       => \%atoms,
    };
    my %rules = %{ $ast->{rules} };
    while (my ($rulename, $rprods) = each %rules) {
        my @prods = @$rprods;
        if (@prods == 1) {
            my @items = emit_prod( $prods[0] );
            if (@items > 1) {
                $concat{$rulename} = \@items;
            } else {
                $atoms{$rulename} = $items[0];
            }
        }
        else {
            my @branches;
            for my $i (0..$#prods) {
                my $prodname = "${rulename}_production_" . ($i+1);
                push @branches, $prodname;
                my @items = emit_prod( $prods[$i] );
                if (@items > 1) {
                    $concat{$prodname} = \@items;
                } else {
                    $atoms{$prodname} = $items[0];
                }
            }
            $altern{$rulename} = \@branches;
        }
    }
    $new_ast;
}

sub emit_prod {
    my $prod = shift;
    my @items = @$prod;
    if ($items[0] =~ /^<error/) {
        return ("error()");
    }
    for my $item (@items) {
        if (ref $item) {
            if ($item->[1] eq 's') {
                if ($item->[2]) {
                    if ($item->[2] =~ /^\//) {
                        $item->[2] = "q" . $item->[2];
                    }
                    $item = "repeat_1_n_sep( sub { \&$item->[0] }, $item->[2] )";
                } else {
                    $item = "repeat_1_n( sub { \&$item->[0] } )";
                }
            }
            elsif ($item->[1] eq 's?') {
                if ($item->[2]) {
                    if ($item->[2] =~ /^\//) {
                        $item->[2] = "q" . $item->[2];
                    }
                    $item = "repeat_0_n_sep( sub { \&$item->[0] }, $item->[2] )";
                } else {
                    $item = "repeat_0_n( sub { \&$item->[0] } )";
                }
            }
            elsif ($item->[1] eq '?') {
                $item = "repeat_0_1( sub { \&$item->[0] } )";
            }
            elsif (@$item == 3 and $item->[1] =~ /^\//) {
                $item = "match_leftop( \\\&$item->[0], q$item->[1], \\\&$item->[2] )"
            }
            else {
                die "Unknown modifier $item->[1]\n";
            }
        }
        elsif ($item =~ /^['"]/) {
            $item = "match_str($item)";
        }
        elsif ($item =~ /^\//) {
            $item = "match_re(q$item)";
        }
        elsif ($item =~ /^\w+$/) {
            $item = "$item()";
        }
    }
    @items;
}

1;
__DATA__
#: parser.pl

package X;

our ($str, $pos, $level);

package Parser;

use strict;
use warnings;

local $/;
my $src = <>;
die "No input source code.\n" if !defined $src;
print "\n", _parse($src) ? 'success' : 'fail', "\n";

sub _rulename {
    my $sub = (caller 2)[3];
    $sub =~ s/^\w+:://g;
    $sub;
}

sub _try {
    my $rule;
    if (@_) {
        $rule = shift;
    } else {
        $rule = _rulename;
    }
    $X::level++;
    my $indent = '  ' x $X::level;
    if (!defined $X::saved_pos or $X::saved_pos != $X::pos) {
        my $next = substr($X::str, $X::pos, 15);
        $next =~ s/\n/\\n/g;
        $next =~ s/\t/\\t/g;
        if (length(substr $X::str, $X::pos) > 15) {
            $next .= '...';
        }
        print "${indent}trying $rule...    [$next]\n";
        $X::saved_pos = $X::pos;
    } else {
        print "${indent}trying $rule...\n";
    }
}

sub _fail {
    my $rule;
    if (@_) {
        $rule = shift;
    } else {
        $rule = _rulename;
    }
    my $indent = '  ' x $X::level;
    print "${indent}FAIL to match $rule...\n";
    $X::level--;
}

sub _success {
    my $rule;
    if (@_) {
        $rule = shift;
    } else {
        $rule = _rulename;
    }
    my $indent = '  ' x $X::level;
    print "${indent}>>MATCH<< $rule...\n";
    $X::level--;
}

sub _parse {
    my ($s) = @_;
    $X::str = $s;
    $X::pos = 0;
    $X::level = 0;
    return [% startrule %]();
}

[% FOREACH rule = alternation.keys -%]
sub [% rule %] {
    _try;
    my $commit;
    [%- productions = alternation.$rule %]
    [%- FOREACH production = productions %]
    if (&[% production %](\$commit)) {
        _success;
        return 1;
    }
      [%- IF production != productions.last %]
    return undef if $commit;
      [%- END %]
    [%- END %]
    undef;
}

[% END -%]

[%- FOREACH rule = concat.keys -%]
sub [% rule %] {
    my $rcommit = shift;
    _try;
    my $saved_pos = $X::pos;
  [%- first = 1 %]
  [%- FOREACH atom = concat.$rule %]
    [%- IF atom == '<commit>' %]
    $$rcommit = 1;
    [%- ELSIF atom == '<uncommit>' %]
    $$rcommit = undef;
    [%- ELSE %]
    if (!&[% atom %]) {
      [%- IF first %]
          [%- first = 0 %]
      [%- ELSE %]
        $X::pos = $saved_pos;
      [%- END %]
        _fail;
        return undef;
    }
    [%- END %]
  [%- END %]
    _success;
    return 1;
}

[% END -%]

[%- FOREACH rule = atoms.keys -%]
sub [% rule %] {
    _try;
    my $match = [% atoms.$rule %];
    if ($match) {
        _success;
        return 1;
    } else {
        _fail;
        return undef;
    }
}

[% END -%]
sub match_str {
    my $target = shift;
    _try("'$target'");
    my $s = $X::str;
    pos($s) = $X::pos;
    if ($s =~ m/\G\s+/gc) {
        $X::pos += length($&);
    }
    #warn substr($s, $X::pos), "\n";
    my $len = length($target);
    my $match = (substr($s, $X::pos, $len) eq $target);
    if (!$match) {
        _fail("'$target'");
        return undef;
    }
    $X::pos += $len;
    _success("'$target'");
    return 1;
}

sub match_re {
    my $re = shift;
    _try("/$re/");
    my $s = $X::str;
    pos($s) = $X::pos;
    if ($s =~ m/\G\s+/gc) {
        $X::pos += length($&);
    }
    if ($re eq "^\\Z") {
        #warn "Matching end of file";
        if ($X::pos == length($X::str)) {
            _success("/$re/");
            return 1;
        }
        _fail("/$re/");
        return undef;
    }
    if ($s !~ /\G(?:$re)/) {
        _fail("/$re/");
        return undef;
    }
    $X::pos += length($&);
    _success("/$re/");
    return 1;
}

sub repeat_1_n_sep {
    my ($coderef, $sep) = @_;
    if (!$coderef->()) {
        return undef;
    }
    while (1) {
        my $saved_pos = $X::pos;
        return 1 if !match_re($sep);
        if (!$coderef->()) {
            $X::pos = $saved_pos;
            return 1;
        }
        elsif ($X::pos == $saved_pos) {
            return 1;
        }
    }
    1;
}

sub repeat_1_n {
    my ($coderef) = @_;
    my $match = $coderef->();
    if (!$match) {
        return undef;
    }
    while (1) {
        my $saved_pos = $X::pos;
        return 1 if !$coderef->() or $X::pos == $saved_pos;
    }
    1;
}

sub repeat_0_n_sep {
    my ($coderef, $sep) = @_;
    if (! $coderef->()) {
        return 1;
    }
    while (1) {
        my $saved_pos = $X::pos;
        return 1 if !match_re($sep);
        if (!$coderef->()) {
            $X::pos = $saved_pos;
            return 1;
        }
        elsif ($X::pos == $saved_pos) {
            return 1;
        }
    }
}

sub repeat_0_n {
    my $coderef = $_[0];
    if (! $coderef->()) {
        return 1;
    }
    while (1) {
        my $saved_pos = $X::pos;
        return 1 if !$coderef->() or $X::pos == $saved_pos;
    }
}

sub repeat_0_1 {
    my $coderef = $_[0];
    $coderef->();
    1;
}

sub match_leftop {
    my ($sub1, $sep, $sub2) = @_;
    return undef if !$sub1->();
    while (1) {
        my $saved_pos = $X::pos;
        return 1 if !match_re($sep);
        if (!$sub2->()) {
            $X::pos = $saved_pos;
            return 1;
        }
        elsif ($X::pos == $saved_pos) {
            return 1;
        }
    }
}

sub error {
    undef;
}