#!/usr/bin/perl -w
# --
# bin/otrs.ITSMTrace.pl - generate trace from objects and links
# Copyright (C) 2011-2014 Thales Austria GmbH, http://www.thalesgroup.com/
# --
# This is a simple wrapper for commandline access to all 
# capabilities of the tracer (not just those selected by
# SysConfig).
# --
# This software comes with ABSOLUTELY NO WARRANTY and WITHOUT ANY SUPPORT. 
# For license information, see the enclosed file COPYING-ITSMTrace
# (GNU AFFERO GENERAL PUBLIC LICENSE, version 3). 
# If you did not receive this file, see 
# http://www.gnu.org/licenses/agpl-3.0.html.
# --

use strict;
use warnings;

use File::Basename;
use FindBin qw($RealBin);
use lib dirname($RealBin);
use lib dirname($RealBin).'/Kernel/cpan-lib';
use lib '/opt/otrs', '/opt/otrs/Kernel/cpan-lib';

use vars qw($VERSION);
$VERSION = 0.2;

use Getopt::Long;
use Kernel::Config;
use Kernel::System::Encode;
use Kernel::System::Log;
use Kernel::System::Main;
use Kernel::System::Time;
use Kernel::System::DB;
use Kernel::System::ITSMConfigItem;
use Kernel::System::GeneralCatalog;
use Kernel::System::LinkObject;
use Kernel::System::Web::Request;
use Kernel::Output::HTML::Layout; 



use Kernel::System::ITSMTrace;

# common objects
our %CommonObjects = ();
$CommonObjects{ConfigObject} = Kernel::Config->new();
$CommonObjects{EncodeObject} = Kernel::System::Encode->new(%CommonObjects);
$CommonObjects{LogObject}    = Kernel::System::Log->new(
    LogPrefix => 'otrs.ITSMTrace.pl',
    %CommonObjects,
);
$CommonObjects{MainObject}           = Kernel::System::Main->new(%CommonObjects);
$CommonObjects{TimeObject}           = Kernel::System::Time->new(%CommonObjects);
$CommonObjects{DBObject}             = Kernel::System::DB->new(%CommonObjects);
$CommonObjects{ConfigItemObject}     = Kernel::System::ITSMConfigItem->new(%CommonObjects);
$CommonObjects{GeneralCatalogObject} = Kernel::System::GeneralCatalog->new(%CommonObjects);
$CommonObjects{LinkObject}           = Kernel::System::LinkObject->new(%CommonObjects);
$CommonObjects{ServiceObject}        = Kernel::System::Service->new(%CommonObjects);
$CommonObjects{RequestObject}        = Kernel::System::Web::Request->new(%CommonObjects);
#$CommonObjects{LayoutObject}         = Kernel::Output::HTML::Layout->new(%CommonObjects, Lang => 'en');

my $USAGE = <<EOT;
otrs.ITSMTrace.pl <Revision $VERSION> - Produce graphical/textual trace
of dependencies between services and config items.

For quick start, try 
  $0 -ServiceID 0 -ObjectTypes Service
to see a (textual) breakdown of all your services, 
then go into more details with
  $0 -ServiceID one-of-your-service-IDs -CompactTrace
  $0 -ConfigItemID one-of-your-CI-IDs -CompactTrace
to become more specific.

Recognized comandline options:
    # Constraints
	# list of identifiers
	-LinkTypes=s{,}
	-ObjectTypes=s{,}
	# integer
	-MaxTraceDepth=i
	# flags
	-IncludeInvalidObjects
	-TraceIncident
	-CompactTrace
	-EnableClustering
    # Input
    	-ServiceID=i
    	-ConfigItemID=i
    # Output
	-OutputFormat=s
	-OutputOptions=s
    # Misc
    	-Debug

See perldoc Kernel/System/ITSMTrace.pm for a detailed description
of possible parameters and their values.
EOT

my %Options;

GetOptions(
    \%Options,
    # Constraints
	# list of identifiers
	'LinkTypes=s{,}',
	'ObjectTypes=s{,}',
	# integer
	'MaxTraceDepth=i',
	# flags
	'IncludeInvalidObjects',
	'TraceIncident',
	'CompactTrace',
	'EnableClustering',
    # Input
    	'ServiceID=i',
    	'ConfigItemID=i',
    # Output
	'OutputFormat=s',
	'OutputOptions=s',
    # Misc
    	'Debug+',
);

if ( ! defined $Options{ServiceID} && ! defined $Options{ConfigItemID} ) {
    print $USAGE;
    exit 64;
}

my $T = Kernel::System::ITSMTrace->new(%CommonObjects, Debug => $Options{Debug}||0);

$T->GetKnownLinkTypes;		# just to show them in debug mode
$T->GetKnownObjectTypes;	# just to show them in debug mode

# Set constraints
my $Ok = $T->SetConstraints( %Options );

# Generate trace
my $Content = $T->Trace( %Options );
if ( ref $Content ) {
    # (potentially) multi-part output
    print $Content->{text} if $Content->{text};
    print $Content->{html} if $Content->{html};
    print $Content->{dot}  if $Content->{dot};
    if ( $Content->{map} ) {
	$Content->{map} =~ s/>\s*$/ Usemap="trace" Ismap>/;
	print $Content->{map};
    }
#    print $Content->{png};	# binary!
}
else
{
    print $Content;
}

1;
