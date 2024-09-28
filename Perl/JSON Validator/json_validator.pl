#!/usr/bin/perl

use strict;
use warnings;
use lib 'C:\Strawberry\perl\site\lib\Term';
use JSON;#::Parse 'parse_json';
#use Term::ANSIColor;
BEGIN {
    eval {
        require Term::ANSIColor;
        Term::ANSIColor->import();
        1;
    } or do {
        die "Term::ANSIColor module is required but not installed. Install it using 'cpanm Term::ANSIColor'.\n";
    };
}

# Check if a file was provided as argument
my $filename = shift or die "Usage: $0 <filename>\n";

# Read JSON from file
open my $fh, '<', $filename or die "Could not open file '$filename' $!";
my $json_text = do { local $/; <$fh> };
close $fh;

# Parse JSON
eval {
    my $json_data = decode_json($json_text);
    print colored("JSON is valid\n", 'bold white on_green');
};
if ($@) {
    print colored("JSON is invalid: $@\n", 'bold white on_red');
}
