#!/usr/bin/perl -w -- # -*- Perl -*-

use strict;

my $template = "dbl10n.template";
my $dbl10n   = "dbl10n.dsl";
my @languages = ();
my %langsection = ();
my $inlist = 0;
my $historical = 0;

open (F, $template);
open (G, ">$dbl10n");

while (<F>) {
    if (/\%\%\/?LANGUAGES\%\%/ || /\%\%HISTORICAL\%\%/) {
	$inlist = 1 if $& eq '%%LANGUAGES%%';
	$inlist = 0 if $& eq '%%/LANGUAGES%%';
	$historical = 1 if $& eq '%%HISTORICAL%%';
	next;
    }

    if ($inlist && /^;; (\S+)\s+-/) {
	my $lang = $1;
	my $section = $1;

	if ($historical) {
	    $lang =~ /^(\S+)\((\S+)\)/;
	    $lang = $1;
	    $section = $2;
	}

	$section =~ s/\_//sg;
#	print "$lang = $section\n";

	push (@languages, $lang);
	$langsection{$lang} = $section;
    }

    if (/<!\[\%l10n-XX/) {
	my $line = $_;
	foreach my $lang (@languages) {
	    my $section = $langsection{$lang};
	    $_ = $line;
	    s/\"XX\"/\"$lang\"/g;
	    s/XX/$section/g;
	    print G $_;
	}
    } else {
	print G $_;
    }
}

close (F);
close (G);
