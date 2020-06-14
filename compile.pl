#!/usr/bin/perl
use warnings;
use strict;

my $in = shift;
my ($out) = ($in =~ /(\w+)\.hs$/);

my $err = system "ghc -O $in";
die $! if $err;

$err = system "chmod +x $out";
die $! if $err;