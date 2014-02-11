# --
# Kernel/Modules/AgentCMDBExplorer.pm - the OTRS::ITSM Trace module
# Copyright (C) 2014 Belnet, http://www.belnet.be
# Copyright (C) 2011 Thales Austria GmbH, http://www.thalesgroup.com/
# --
# $Id: AgentCMDBExplorer.pm $
# --
# This software comes with ABSOLUTELY NO WARRANTY and WITHOUT ANY SUPPORT. 
# For license information, see the enclosed file COPYING-CMDBExplorer
# (GNU AFFERO GENERAL PUBLIC LICENSE, version 3). 
# If you did not receive this file, see 
# http://www.gnu.org/licenses/agpl-3.0.html.
# --


package Kernel::Modules::AgentCMDBExplorer;

use strict;
use warnings;

use Kernel::System::Service;
use Kernel::System::ITSMConfigItem;
use Kernel::System::LinkObject;

use Kernel::System::CMDBExplorer;

use vars qw($VERSION);
$VERSION = 0.8;


=head1 NAME

Kernel::Modules::AgentCMDBExplorer - URL interface for tracing links
=cut


=head1 SYNOPSIS

This module translates the URL into calls to C<Kernel::System::CMDBExplorer>
to generate a HTML page (currently a single PNG image). 


=cut


=head1 URL INTERFACE

This module is invoked through a "Trace" link on the service or
config item zoom pages, configurable through SysConfig in the 
module registration for this module.

For a detailed description of parameters and their possible values 
see C<Kernel::System::CMDBExplorer>. Note that the URL can be controlled 
through SysConfig, so it is possible to override the built-in defaults.

=over 4

  http://otrs-server/otrs/index.pl?Action=AgentCMDBExplorer;

    ServiceID=<uint>;			(passed from calling module)
    ConfigItemID=<uint>;                (passed from calling module)

    LinkTypes=ltype1,ltype2,...;
	default: ComposedOf,DependsOn,RelevantTo

    ObjectTypes=otype1,otype2,...;
	default: Service,ITSMConfigItem

    IncludeInvalidObjects=1;		default: 0 (=off)
    TraceIncidentState=1;		default: 0 (=off)
    CompactFlow=1;			default: 0 (=full trace)
    EnableClustering=1;			default: 0 (=off)
    MaxTraceDepth=<uint>;		default: 0 (=unlimited)
    
=back

=cut


=head1 PUBLIC INTERFACE

=over 4

=cut

#
########################################################################


###  C o n s t r u c t o r  ############################################
#

=item new()

=cut

sub new {
    my ( $Type, %Param ) = @_;

    # allocate new hash for object
    my $Self = {%Param};
    bless( $Self, $Type );

    # check needed objects
    for my $Object (qw( MainObject ConfigObject ParamObject 
	    	        DBObject LayoutObject LogObject) ) {
        if ( !$Self->{$Object} ) {
            $Self->{LayoutObject}->FatalError( Message => "Got no $Object!" );
        }
    }
    $Self->{ServiceObject} = Kernel::System::Service->new(%Param);
    $Self->{LinkObject} = Kernel::System::LinkObject->new(%Param);

    return $Self;
} # new()
#
########################################################################


###  Me t h o d s  #####################################################
#

=item Run()

=cut

sub Run {
    my ( $Self, %Param ) = @_;

#    $Self->{LogObject}->Log(Priority=>'error', Message=>"Self: \n\t". join(', ', sort keys %$Self));

    my %CommonObject;
    $CommonObject{$_} = $Self->{$_} for grep (/Object$/, keys %$Self);
    $CommonObject{GeneralCatalogObject} = 
    		Kernel::System::GeneralCatalog->new(%CommonObject);
    $CommonObject{ConfigItemObject} =
    		Kernel::System::ITSMConfigItem->new(%CommonObject);

    my $Tracer = Kernel::System::CMDBExplorer->new(%CommonObject, Debug => 1);

    # Get constraints from URL, pass them on
    my %ConstraintParams;
    for my $ParamName ( qw( LinkTypes 
	    		    ObjectTypes
			    IncludeInvalidObjects 
			    TraceIncidentState 
			    CompactTrace
			    EnableClustering
			    MaxTraceDepth )) {
	my $ParamValue = $Self->{ParamObject}->GetParam( Param => $ParamName );
	$ConstraintParams{$ParamName} = $ParamValue if defined $ParamValue;
    }
#    # Set my defaults
#    $ConstraintParams{LinkTypes} = 'ComposedOf,DependsOn,RelevantTo' 
#    				unless $ConstraintParams{LinkTypes};
#    $ConstraintParams{ObjectTypes} = 'Service,ITSMConfigItem' 
#    				unless $ConstraintParams{ObjectTypes};
    $Tracer->SetConstraints( %ConstraintParams ) if %ConstraintParams;

    # Get trace params from URL, pass them on
    my %TraceParams;
    for my $ParamName ( qw( ServiceID ConfigItemID OutputOptions )) {
	my $ParamValue = $Self->{ParamObject}->GetParam( Param => $ParamName );
	$TraceParams{$ParamName} = $ParamValue if defined $ParamValue;
    }
    $TraceParams{OutputFormat} = 'png';	# force
#    $TraceParams{OutputFormat} = 'imgmap';	# force
    my $Content = $Tracer->Trace( %TraceParams );

    # ------------------------------------------------------------ #
    # render image + map
    # ------------------------------------------------------------ #
    if ( ref $Content ) {
	my $Output = $Self->{LayoutObject}->Attachment(
	    Type        => 'inline',
	    Filename    => 'trace.png',
	    ContentType => 'image/png',
	    Content     => $Content->{png},
	); 
	return $Output;
    }

    return '';
} # Run()
#
########################################################################
1;

=back

=head1 TERMS AND CONDITIONS

Copyright (C) 2014- Belnet, http://www.belnet.be
Copyright (C) 2011-2014 Thales Austria GmbH, http://www.thalesgroup.com/

This software comes with ABSOLUTELY NO WARRANTY and WITHOUT ANY SUPPORT. 

For license information, see the enclosed file COPYING-CMDBExplorer
(GNU AFFERO GENERAL PUBLIC LICENSE, version 3). 
If you did not receive this file, see 
http://www.gnu.org/licenses/agpl-3.0.html.


=head1 AUTHOR

cyrille.bollu@belnet.be
dietmar.berg@thalesgroup.com

=cut
