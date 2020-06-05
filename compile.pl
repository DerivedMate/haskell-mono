#!/usr/bin/perl
use warnings;
use strict;

my $in = shift;
my ($out) = ($in =~ /(\w+)\.hs$/);

my $err = system "ghc $in";
die $! if $err;

$err = system "chmod +x $out";
die $! if $err;