#!/bin/perl -- # -*- Perl -*-

# Charset hacking...

while (<>) {
    s/\&\#(\d+);/sprintf("\\U-%04X;", $1)/egs;
    print;
}
