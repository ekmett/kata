#!/usr/bin/perl
use strict;
use warnings;
use Acme::LOLCAT;

my $echo = undef;
while(<>) {
    ## header
    $echo = 1 if $_ =~ /Project-Id-Version/;
    if ($echo) {
        print;
    } else {
        # TODO: preserve --options and quoted strings?
        print lc (translate $_);
    }
}
