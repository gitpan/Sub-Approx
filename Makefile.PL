use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	=> 'Sub::Approx',
    'VERSION_FROM' => 'Approx.pm', # finds $VERSION
    'PREREQ_PM' => { GlobWalker => 0 },
);
