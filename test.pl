# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..10\n"; }
END {print "not ok 1\n" unless $loaded;}
use Sub::Approx;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

sub aa { 'aa' }

sub bb { 'bb' }

print &a eq 'aa' ? '' : 'not ', "ok 2\n";

print &b eq 'bb' ? '' : 'not ', "ok 3\n";

eval "&c";
print $@ ? '' : 'not ', "ok 4\n";

Sub::Approx->import(match => \&always_bb);

print &a eq 'bb' ? '' : 'not ', "ok 5\n";

Sub::Approx->import(match => sub { 'aa' });

print &b eq 'aa' ? '' : 'not ', "ok 6\n";

sub always_bb {
  'bb';
}


package x;
Sub::Approx->import(match => sub {scalar reverse shift});
eval {foo()};
print "not ok 7\n" if $@;
sub oof {print "ok 7\n"}

Sub::Approx->import(match => 'text_soundex'); 
eval {bar()};
print "not ok 8\n" if $@;
sub baar {print "ok 8\n"}
sub qux  {12}

package y;
Sub::Approx->import(match => sub {shift; return @_});
print &x::quux()==12 ? '' : 'not ', "ok 9\n";
print &y::quux()==23 ? '' : 'not ', "ok 10\n";
sub flurble {23}
