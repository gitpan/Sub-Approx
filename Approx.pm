package Sub::Approx;

use strict;
use vars qw($VERSION @ISA $AUTOLOAD);

$VERSION = '0.05';

use Carp;

# Global configuration data
my %CONF;

# import is called when another script uses this module.
# All we do here is overwrite the callers AUTOLOAD function
# with our own.
sub import  {
  my $class = shift;

  no strict 'refs'; # WARNING: Deep magic here!
  my $pkg =  caller(0);
  *{"${pkg}::AUTOLOAD"} = \&AUTOLOAD;

  %CONF = @_ if @_;
  my $default = 'text_soundex';

  # Functions called to handle our built-in matchers.
  my %funcs = (text_soundex => [\&setup_text_soundex,
				\&match_text_soundex],
	       text_metaphone => [\&setup_text_metaphone,
				  \&match_text_metaphone],
	       string_approx => [\&setup_string_approx,
				 \&match_string_approx]);

  # Work out which matcher function to use. There are four valid options:
  # 1/ $CONF{match} is empty - use default matcher (&match_text_soundex)
  # 2/ $CONF{match} is a code ref - use the referenced subroutine
  # 3/ $CONF{match} is a scalar - use one of the predefined matchers
  # and two invalid ooptions:
  # 1/ $CONF{match} is a scalar that doesn't match the predefined matchers
  # 2/ $CONF{match} is some other kind of reference.

  if (exists $CONF{match}) {
    if (ref $CONF{match} eq 'CODE') {
      croak 'Invalid matcher passed to Sub::Approx' 
	unless defined &{$CONF{match}};
    } elsif (ref $CONF{match} eq '') {
      if (exists $funcs{$CONF{match}}) {
	$funcs{$CONF{match}}->[0]->();
	$CONF{match} = $funcs{$CONF{match}}->[1];
      } else {
	croak 'Invalid matcher passed to Sub::Approx';
      }
    } else {
      croak 'Invalid matcher passed to Sub::Approx';
    }
  } else {
    $funcs{$default}->[0]->();
    $CONF{match} = $funcs{$default}->[1];
  }

  # Work out which chooser to use. $CONF{choose} is either non-existant
  # or a reference to the subroutine to use.
  if (exists $CONF{choose}) {
    if (ref $CONF{choose} ne 'CODE') {
      croak 'Invalid chooser passed to Sub::Approx';
    }
    croak 'Invalid chooser passed to Sub::Approx' 
      unless defined &{$CONF{choose}};    croak 'Invalid chooser passed to Sub::Approx' 
      unless defined &{$CONF{choose}};

  } else {
    $CONF{choose} = \&choose;
  }
}

#
# The next three subroutines are used to set up the default matchers.
# Only one of these will ever get called.
# Notice that we 'require' and 'import' rather than 'use' the modules
# because 'use' happens at compile time and all of the modules would
# be loaded into memory.
sub setup_text_soundex {
  require Text::Soundex;
  Text::Soundex->import;
}

sub setup_text_metaphone {
  require Text::Metaphone;
  Text::Metaphone->import;
}

sub setup_string_approx {
  require String::Approx;
  String::Approx->import('amatch');
}

sub match_text_soundex {
  my ($wantsub, @subs) = @_;
  my %cache;

  # For each subroutine name, we work out the equivalent soundex value
  # and store it in the cache hash. Actually we store a list of
  # function names against each soundex value.
  foreach my $sub (@subs) {
    push @{$cache{soundex($sub)}}, $sub;
  }

  # Now work out the soundex value for the function that has been called
  $wantsub = soundex($wantsub);
  
  return @{$cache{$wantsub}} if (exists $cache{$wantsub});
  return;
}

sub match_text_metaphone {
  my ($wantsub, @subs) = @_;

  my %cache;

  # For each subroutine name, we work out the equivalent metaphone value
  # and store it in the cache hash. Actually we store a list of
  # function names against each metaphone value.
  foreach my $sub (@subs) {
    push @{$cache{Metaphone($sub)}}, $sub;
  }

  # Now work out the metaphone value for the function that has been called
  $wantsub = Metaphone($wantsub);
  
  return @{$cache{$wantsub}} if (exists $cache{$wantsub});
  return;
}
  
sub match_string_approx {
  my ($wantsub, @subs) = @_;

  # Luckily, the author of String::Approx makes this
  # really easy 
  return amatch($wantsub, @subs);
}

sub choose {
  $_[rand @_];
}

# AUTOLOAD is a function which is called when a given subroutine
# name can't be found in the current package. In the import function
# above we have already arranged that our calling package will use
# this AUTOLOAD instead of its own.
sub AUTOLOAD {
  my @c = caller(0);
  my ($pkg, $sub) = $AUTOLOAD =~ /^(.*)::(.*)$/;

  my @subs;

  no strict 'refs'; # WARNING: Deep magic here!

  # Iterate across the keys of the stash for our calling package.
  # For each typeglob found, work out if it contains a function
  # definition. If it does, then work out the equivalent soundex
  # value and store it in the cache hash. Actually we store a list
  # of function names against each soundex value.
  foreach (keys %{"${pkg}::"}) {
    my $glob = $::{$_};
    
    $glob =~ s/^\*${pkg}:://;

    next unless defined &{"*${pkg}::$glob"};
    next if $glob eq 'AUTOLOAD'; # This would be fun, but Bad
    push @subs, $glob;
  }

  # Call the subroutine that will look for matches
  my @matches = $CONF{match}->($sub, @subs);

  # See if a function (or functions) exist with the same soundex value.
  # If so, pick one at random to call and call it using magic goto.
  # If not, die recreating Perl's usual behaviour.
  if (@matches) {
    $sub = "${pkg}::" . $CONF{choose}->(@matches);
    goto &$sub;
  } else {
    die "REALLY Undefined subroutine $AUTOLOAD called at $c[1] line $c[2]\n";
  }
}

1;
__END__

=head1 NAME

Sub::Approx - Perl module for calling subroutines by approximate names!

=head1 SYNOPSIS

  use Sub::Approx;
  
  sub a {
    # blah...
  }

  &aa; # executes &a if &aa doesn't exist.

  use Sub::Approx (match => 'text_metaphone');
  use Sub::Approx (match => 'string_approx');
  use Sub::Approx (match => 'text_soundex');
  use Sub::Approx (match => \&my_matcher);
  use Sub::Approx (match => \&my_matcher, \&my_chooser);

=head1 DESCRIPTION

This is _really_ stupid. This module allows you to call functions by
_approximate_ names. Why you would ever want to do this is a complete
mystery to me. It was written as an experiment to see how well I 
understood typeglobs and AUTOLOADing.

To use it, simply include the line:

  use Sub::Approx;

somewhere in your program. Then each time you call a function that doesn't
exist in the the current package Perl will search for a function with
approximately the same name. The meaning of 'approximately the same' is
configurable. The default is to find functions with the same Soundex
value (as defined by Text::Soundex) as the missing function. There are
two other built-in matching styles using Text::MetaPhone and 
String::Approx. To use either of these use:

  use Sub::Approx (match => 'text_metahpone');

or

  use Sub::Approx (match => 'string_approx');

when using Sub::Approx.

You can also use your own function to do the matching. Your function
should expect to receive the name of the missing functions followed by
a list containing all valid function names and should return a list
of all matching functions. For example:

  sub my_matcher {
    my $sub_wanted = shift;

    my @subs = @_;

    return @subs;
}

This example isn't particularly useful as it says that all function
names are an equally good match. To use this match function in place of 
the standard ones, give Sub::Approx a reference to the function like this:

  use Sub::Approx (match => \&my_matcher);

Having retrieved a list of matches, we need to select one of them to
run. The default behaviour is to pick one at random, but again you can
configure this behaviour by writing a function. This function will be
passed a list of matching function names and should return the name of
the function to run. For example:

  sub my_chooser {
    return shift;
  }

which will return the first function name in the list. To make Sub::Approx
use this function in place of the standard one, give Sub::Approx a
refernce to the function like this:

  use Sub::Approx (choose => \&my_chooser);

You can, of course, define both a matcher and a chooser like this:

  use Sub::Approx (match => \&my_matcher, choose => \&my_chooser);

or use you own chooser in conjunction with a standard matcher like this:

  use Sub::Approx (match => 'text_metaphone',
                   choose => \&my_chooser);

=head1 CAVEAT

I can't stress too strongly that this will make your code completely 
unmaintainable and you really shouldn't use this module unless you're 
doing something very stupid.

=head1 ACKNOWLEDGEMENTS

This idea came to me whilst sitting in Mark-Jason Dominus' "Tricks of
the Wizards" tutorial. In order to protect his reputation I should
probably point out that just as the idea was forming in my head he
clearly said that this kind of thing was a very bad idea.

Leon Brocard  is clearly as mad as me as he pointed out some important bugs
and helped massively with the 'fuzzy-configurability'.

=head1 AUTHOR

Dave Cross <dave@dave.org.uk>

With lots of help from Leon Brocard <leon@astray.com>

=head1 SEE ALSO

perl(1).

=cut
