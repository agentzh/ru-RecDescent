#: spike.pl
#: 2006-05-25 2006-05-26

use strict;
use warnings;

use Getopt::Std;
use FindBin;
use lib $FindBin::Bin;

use spike_parser;
use spike_emitter;

my %opts;
getopts('n:', \%opts);

my $infile = shift or
    die "Usage: spike [-n <package-name>] <grammar-file>.\n";

my $filetype = 'lua';
my $package = $opts{n} || 'Parser';

open my $in, $infile or
    die "Can't open $infile for reading: $!\n";
my $src;
{ local $/; $src = <$in>; }
close $in;

my $parser = Spike::Parser->new;
my $ast = $parser->parse($src);
defined $ast or die "Bad grammar!\n";

my $code = Spike::Emitter->emit($ast, $filetype, $package);
defined $code or die "Can't emit parser!\n";

my $outfile = $infile;
if ($outfile !~ s/\.grammar$/.$filetype/) {
    $outfile .= ".$filetype";
}
open my $out, "> $outfile" or
    die "Can't open $outfile for writing: $!\n";
print $out $code;
close $out;
print "$outfile generated.\n";

