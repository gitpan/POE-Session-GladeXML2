package Foo;

use Gnome2;
use POE qw(Loop::Glib);
use POE::Session::GladeXML2;


sub on_delete {
   print "exiting\n";
   return 0;
}

sub on_button1_clicked {
   print "click\n";
}

sub package_start {
	warn "BEGIN @_";
}

sub new {
   my ($class) = @_;

   my $self = {};
   bless $self, $class;
   my $s = POE::Session::GladeXML2->create (
	       glade_object => $self,
	       glade_file => 'test.glade',
	       #glade_mainwin => 'window2',
	       #inline_states => { _start => \&package_start },
	       #package_states => [ $class => {_start => "package_start" }],
	       object_states => [ $self => {_start => "package_start" }],
	    );
   $self->{'session'} = $s;
   return $self;
}

package main;

Gnome2::Program->init (qw(test 1));
my $foo = Foo->new;
POE::Kernel->run;
