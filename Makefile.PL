#!/usr/bin/perl

use ExtUtils::MakeMaker;

sub MY::postamble {
   package MY;
   "\npredist: all\n" .
#   "\tcvs2cl\n" .
   "\t\$(MAKE) manifest\n" .
   "\t\$(MAKE) tardist\n"
}

WriteMakefile(
   MAKEFILE => 'Makefile',
   VERSION_FROM => 'lib/POE/Session/GladeXML2.pm',
   NAME => 'POE::Session::GladeXML2',
   PREREQ_PM => {
      'POE' => '0.31',
      'Gtk2::GladeXML' => '0',
      'POE::Loop::Glib' => '0.0022',
   },
   dist => {
      COMPRESS     =>'gzip',
      SUFFIX       =>'gz',
      DIST_DEFAULT => 'predist',
   },
);
