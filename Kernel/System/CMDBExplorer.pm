# --
# Kernel/System/CMDBExplorer.pm - traces links between objects
# Copyright (C) 2014- Belnet, http://www.belnet.be/
# Copyright (C) 2011-2014 Thales Austria GmbH, http://www.thalesgroup.com/
# --
# $Id: CMDBExplorer.pm $
# --
# This software comes with ABSOLUTELY NO WARRANTY and WITHOUT ANY SUPPORT. 
# For license information, see the enclosed file COPYING-CMDBExplorer
# (GNU AFFERO GENERAL PUBLIC LICENSE, version 3). 
# If you did not receive this file, see 
# http://www.gnu.org/licenses/agpl-3.0.html.

# --

package Kernel::System::CMDBExplorer;

=head1 NAME

Kernel::System::CMDBExplorer - trace links between services, config items or FAQs

=head1 SYNOPSIS

This class abstracts a set of ITSM services, config items, FAQs and the links
between them as a "trace" that can be rendered textually (built-in) or 
graphically by C<Kernel::System::CMDBExplorer::GraphVizRenderer>, using the
free graphviz library.

Its purpose is to support visualization of service dependencies either 
directly in the (web-)GUI or from the commandline.

=cut

use strict;
use warnings;

use vars qw($VERSION);
$VERSION = '0.81';

use Kernel::System::Service;
use Kernel::System::ITSMConfigItem;
use Kernel::System::LinkObject;
use Kernel::System::GeneralCatalog;

use Kernel::System::CMDBExplorer::ObjectWrapper;
use Kernel::System::CMDBExplorer::Scope;
use Kernel::System::CMDBExplorer::GraphVizRenderer;

=head1 PUBLIC INTERFACE

=over 4

=cut



###  C o n s t r u c t o r  ############################################
#

=item new()

Creates an object.

    use Kernel::Config;
    use Kernel::System::Encode;
    use Kernel::System::Log;
    use Kernel::System::Main;
    use Kernel::System::Time;
    use Kernel::System::DB;
    use Kernel::System::ITSMConfigItem;
    use Kernel::System::GeneralCatalog;
    use Kernel::System::LinkObject;

    my %CommonObjects = ();
    $CommonObjects{ConfigObject}  = Kernel::Config->new();
    $CommonObjects{EncodeObject}  = Kernel::System::Encode->new(%CommonObjects);
    $CommonObjects{MainObject}    = Kernel::System::Main->new(%CommonObjects);
    $CommonObjects{TimeObject}    = Kernel::System::Time->new(%CommonObjects);
    $CommonObjects{DBObject}      = Kernel::System::DB->new(%CommonObjects);
    $CommonObjects{ConfigItemObject} = Kernel::System::ITSMConfigItem->new(%CommonObjects);
    $CommonObjects{GeneralCatalogObject} = Kernel::System::GeneralCatalog->new(%CommonObjects);
    $CommonObjects{LinkObject}    = Kernel::System::LinkObject->new(%CommonObjects);
    $CommonObjects{ServiceObject} = Kernel::System::Service->new(%CommonObjects);
    
    my $CMDBExplorerObject = Kernel::System::CMDBExplorer->new(
        %CommonObjects,
 	Debug => 0,	# optional, { 0 | 1 }
    );

=cut

# List of OTRS common objs required for this class to function
my @_COMMON_OBJECTS = ( qw( ConfigObject LogObject MainObject 
			    DBObject EncodeObject 
			    ConfigItemObject GeneralCatalogObject 
			    LinkObject ServiceObject ) );

sub new {
    my ( $ClassOrType, %Param ) = @_;

    # allocate new hash for object
    my $Self = {};
    my $Class = ref $ClassOrType || $ClassOrType; 
    bless( $Self, $Class );

    # check needed objects
    for my $Obj ( @_COMMON_OBJECTS ) {
        $Self->{$Obj} = $Param{$Obj} || die "$Class->new(): Got no $Obj!";
    }

    # Get debug setting through config, overriden by parameter
    $Self->{Debug} = $Self->{ConfigObject}->{'CMDBExplorer::Debug'} || 0;
    $Self->{Debug} = $Param{Debug} if defined $Param{Debug};

    return $Self;
} # new()



# Internal accessor method that returns all required OTRS common objects
# (and debug settings) as HASH-ref to facilitate construction of a new 
# object based on an existing one.
sub _GetCommonObjects
{
    my $Self = shift;
    my %CommonObjects;
    $CommonObjects{$_} = $Self->{$_} for ( @_COMMON_OBJECTS, 'Debug' );
    return \%CommonObjects;
} # _GetCommonObjects()

###  M e t h o d s  ####################################################
#

=item GetKnownLinkTypes()

Gets I<all> known link types that may exist between different objects.
For the purpose of link tracing and visualization, the hierarchical 
decomposition of services is modelled as the pseudo link type 
C<ComposedOf>.

Link types are returned as the keys of a HASH ref for usage as lookup 
table, while the values indicate if it is a non-directional link ('=') 
or directed link, with '>' for a "normal" directed link, '!' for I<the> 
dependency link used for incident state propagation, or '+' for the 
pseudo decomposition link.

    my $HashRef = $TraceObject->GetKnownLinkTypes();

=cut
sub GetKnownLinkTypes
{
    my $Self = shift;
    return $Self->{KnownLinkTypes} if $Self->{KnownLinkTypes};	# cache-hit

    my %KnownLinkTypes = ( ComposedOf => '+' );	# pseudo, for svc breakdown
    my %TypeList = $Self->{LinkObject}->TypeList( UserID => 1 );
    $KnownLinkTypes{$_} =
	$TypeList{$_}->{SourceName} eq $TypeList{$_}->{TargetName} ? '=' : '>'
							   for keys %TypeList;
    my $IncidentLinkType = $Self->{ConfigObject}->{'ITSM::Core::IncidentLinkType'};
    $KnownLinkTypes{$IncidentLinkType} = '!' if $KnownLinkTypes{$IncidentLinkType};
    $Self->{KnownLinkTypes} = \%KnownLinkTypes;		# cache it

    return \%KnownLinkTypes;
}

=item GetKnownObjectTypes()

Gets I<all> known "object" types for the purpose of setting constraints
for link tracing and visualization. 
Currently these are C<Service>, C<ITSMConfigItem> and refinements of it
in the form C<ITSMConfigItem::I<Class>>. 
Object types are returned as the keys of a HASHref for use as lookup table.

    my $HashRef = $TraceObject->GetKnownObjectTypes();

=cut
sub GetKnownObjectTypes
{
    my $Self = shift;

    my %KnownObjectTypes = ( 'Service' => 1, 'ITSMConfigItem' => 1, ); # "built-in"

    # add specializations of config items as separate object types
    my $ITSMConfigItemClasses = $Self->{GeneralCatalogObject}->ItemList(
	Class => 'ITSM::ConfigItem::Class',
	Valid => 1, 
    );

    $KnownObjectTypes{"ITSMConfigItem::$_"} = 1 for values %$ITSMConfigItemClasses;

    return \%KnownObjectTypes;
}
    
########################################################################
#

=item SetConstraints()

The extent of a trace can be controlled by filtering ITSM object- 
and link-types as well as by setting one of the other constraints
shown below.

For convenience, lists of identifiers can be passed as ARRAY ref 
or as comma-separated string.  Most constraints are not applied 
to an object which initially starts a trace.
   
    my $ItemCount = $TraceObject->SetConstraints(

        # ALL parameters are optional; 
	#  - default for list parameters is "not set" (i.e. not filtered),
	#  - default for boolean/integer parameters is 0

	# Explicit list of link types to include in the trace
	# possible: all known link types (only the "raw" types, not any
	#           direction-dependent labels that they might have).
	LinkTypes  => [ qw( DependsOn RelevantTo AlternativeTo ComposedOf ) ],
	LinkTypes  => 'DependsOn,RelevantTo,AlternativeTo,ComposedOf',

	# Explicit list of object types to include in the trace
	# possible: 'Service', 'ITSMConfigItem', 'ITSMConfigItem::I<Class>'
	ObjectTypes  => [ qw( Service ITSMConfigItem ) ],
	ObjectTypes  => 'Service,ITSMConfigItem',

	# Flag to include objects in the trace that do not have status "valid";
	# this is also respected for objs that start a trace!
	IncludeInvalidObjects  => 0, 		# 0 | 1

	# Flag to trace only to/from objects with an incident (non-operational)
	TraceIncident  => 0, 			# 0 | 1

	# Number of "hops" to traverse; 0 = unlimited
	MaxTraceDepth => 0,           		# >= 0
    );

If anything goes wrong, the method logs an error and returns C<undef>, 
otherwise it returns 1.

=cut
sub SetConstraints {    
    my ( $Self, %Param ) = @_;

    # check and collect params that we can process
    if ( $Param{LinkTypes} ) {
	# check for correct data type / split string-list
	$Param{LinkTypes} = 
		$Self->_CheckStringListParam($Param{LinkTypes}, 'LinkTypes');
	return unless defined $Param{LinkTypes};	# error, already logged
	# check requested link types against known ones
	my $KnownLinkTypes = $Self->GetKnownLinkTypes;
	my %LinkTypes;
	for my $RequestedLinkType ( @{$Param{LinkTypes}} ) {
	    if ( !exists $KnownLinkTypes->{$RequestedLinkType} ) {
		$Self->{LogObject}->Log(
		    Priority => 'error',
		    Message  => "Unknown link type '$RequestedLinkType' "
		              . "for parameter 'LinkTypes'! "
		    	      . "Known types are: \n\t'" 
			      . join("',\n\t'", sort( keys %$KnownLinkTypes))."'",
		);
		return;
	    } #if
	    $LinkTypes{$RequestedLinkType}++;
	} #for
	# save HASHref for quick lookup
	$Self->{LinkTypes} = \%LinkTypes;
    } #if

    if ( $Param{ObjectTypes} ) {
	# check for correct data type / split string-list
	$Param{ObjectTypes} =
		$Self->_CheckStringListParam($Param{ObjectTypes}, 'ObjectTypes');
	return unless defined $Param{ObjectTypes};	# error, already logged
	# check requested object types against known ones
	my $KnownObjectTypes = $Self->GetKnownObjectTypes;
	my %ObjectTypes;
	for my $RequestedObjectType ( @{$Param{ObjectTypes}} ) {
	    if ( !exists $KnownObjectTypes->{$RequestedObjectType} ) {
		$Self->{LogObject}->Log(
		    Priority => 'error',
		    Message  => "Unknown object type '$RequestedObjectType' "
		     	      . "for parameter 'ObjectTypes'! "
		    	      . "Known types are: \n\t'" 
			      . join("',\n\t'", sort(keys %$KnownObjectTypes))."'",
		);
		return;
	    } #if
	    $ObjectTypes{$RequestedObjectType}++;
	} #for
	# save HASHref for quick lookup
	$Self->{ObjectTypes} = \%ObjectTypes;
    } #if

    for my $Key ( qw( IncludeInvalidObjects 
	    	      TraceIncident )) {
	if ( $Param{$Key} && $Param{$Key} !~ m{^[01]$} ) {
	    $Self->{LogObject}->Log(
		Priority => 'error',
		Message  => "'$Key' must be 0 or 1, not '$Param{$Key}'!",
	    );
	    return;
	} #if
	$Self->{$Key} = ( ($Param{$Key} || 0) != 0 );
    } #for

    if ( $Param{MaxTraceDepth} && $Param{MaxTraceDepth} !~ m{^[0-9]+$} ) {
        $Self->{LogObject}->Log(
            Priority => 'error',
            Message  => "'MaxTraceDepth' must be a non-negative integer, "
	    	      . "not '$Param{MaxTraceDepth}'!",
        );
        return;
    } #if
    $Self->{MaxTraceDepth} = $Param{MaxTraceDepth} || 0;
    return 1;
}

########################################################################
#
# Private method to ensure that the given parameter is either an ARRAYref
# or a comma/blank-separated list of "identifiers" that is split into an 
# ARRAYref using /\s*[,;]\s*/.
# In case of error, a message is logged and the function returns undef.
#
#     $AryRef = $TraceObject->_CheckStringListParam( $ParamToBeChecked, 
#     						     $ParamName );
#
sub _CheckStringListParam {
    my ($Self, $Param, $Name) = @_;
    my $Ref = ref $Param;
    # Check for ARRAY ref
    if ( $Ref && $Ref ne 'ARRAY' ) {
	$Self->{LogObject}->Log(
	    Priority => 'error',
	    Message  => "Parameter '$Name' must be an ARRAY ref "
		       ."or a string-list!",
	);
	return;
    } elsif ( !$Ref )  {
	# Check for valid string, split it
	$Param =~ s/^\s+//;	# trim left
	$Param =~ s/\s+$//;	# trim right
	if ( $Param !~ m/^[-_.,;:# a-z0-9]+$/i ) {
	    $Self->{LogObject}->Log(
		Priority => 'error',
		Message  => "Invalid string for parameter '$Name', "
			   ."must be list of identifiers!",
	    );
	    return;
	} #if
	$Param = [ split(/\s*[,;]\s*/, $Param) ];
    } #elsif
    return $Param;
}

########################################################################
#

=item Trace()

Recursively loads objects and links between them and generates a trace 
which it then renders in one of the supported output formats.

Starting from a single service given by its ID:

    my $Output = $TraceObject->BuildTrace(
    	ServiceID	=> $ServiceID,		# ID > 0
	...
    );

Hierarchical nesting of services is expressed by a pseudo-link C<ComposedOf>.

Starting from a single config item given by its ID:

    my $Output = $TraceObject->BuildTrace(
	ConfigItemID	=> $ConfigItemID,	# ID > 0
	...
    );

At least one of the parameters I<ServiceID> or I<ConfigItemID> must be 
specified. An ID of 0 selects all items of this type.

A I<specific> object with which the trace starts is always included,
irrespective of any constraints that otherwise restrict the inclusion
of objects and links into the graph.

The currently supported output formats are: C<text> (plain), C<png> 
(PNG image), C<imgmap> (PNG image plus HTML imagemap), and C<dot> 
(GraphViz "source"). A string of arbitrary output options can also 
be passed to the renderer.

    my $OutputRef = $TraceObject->Trace(
	...
    );

The output is returned as HASHref, since it can consist of multiple 
parts. For details, see the description of the renderer C<GraphVizRenderer>.

If there is an error, this is logged and the method returns C<''>.

=cut
sub Trace
{
    my ( $Self, %Param ) = @_;

    # check needed stuff
    if ( ! defined $Param{ServiceID} && ! defined $Param{ConfigItemID} ) {
        $Self->{LogObject}->Log(
            Priority => 'error',
            Message  => "Need at least one of the parameters 'ServiceID' "
		       ."or 'ConfigItemID'!",
        );
        return '';
    } #if
    $Self->{ServiceID}    = $Param{ServiceID}    if defined $Param{ServiceID};
    $Self->{ConfigItemID} = $Param{ConfigItemID} if defined $Param{ConfigItemID};
    $Self->{DisplayedCIs} = $Param{DisplayedCIs} if defined $Param{DisplayedCIs};

    # Init
    Kernel::System::CMDBExplorer::ObjectWrapper->Init();	# clear cache
    $Self->GetKnownLinkTypes;	# preload private link type lookup table

    # Start the recursion to build the trace
    $Self->{VisitedObjects} = { };	# loop protection
    my @TraceSteps = ( );	# step includes link + object at "other" end
    for my $Object ( @{$Self->_expandServiceID(    $Self->{ServiceID}    ) },
       		     @{$Self->_expandConfigItemID( $Self->{ConfigItemID} ) },
                     @{$Self->_expandConfigItemID( $Self->{DisplayedCIs} ) } )

    {
	# Prepare the "root" step of the trace (no link, just Object2)
	my $TraceStep = { 
	    Level => 0, 		# recursion level
	    LinkType => '', 		# type of link to object
	    LinkDir => '', 		# 'out' | 'in' | '' (=root)
	    LinkDirType => '', 		# '=' (non-directed)  | '>' | '!' | '+'
	    Object1 => undef, 		# object at "this" end of link (root step: none)
	    Object2 => $Object,		# object at "other" end of link
	    Position => 0,		# relative pos. in top/down dep. chain
	    				#   directed outlink: ++, inlink: --
	    Visited => 0,		# flag if Object2 has been visited before
	};
	push @TraceSteps, $TraceStep;
	my $SubTraceSteps = $Self->_followLinks( $Object, 1, 0 );
	push @TraceSteps, @$SubTraceSteps if scalar @$SubTraceSteps;
    } #for

    # Create output, return it
    my $Renderer = Kernel::System::CMDBExplorer::GraphVizRenderer->new(
	Debug        => $Self->{Debug},
        RootCI       => $Self->{ConfigItemID}[0],
        DisplayedCIs => $Self->{DisplayedCIs},
        DisplayedCIs => $Self->{DisplayedCIs},
        Layout       => $Param{Layout} || 'dot',
    );

    return $Renderer->Render(
        TraceSteps    => \@TraceSteps,
    );
}

########################################################################
#
# Private method to recurse into the trace
sub _followLinks {
    my ( $Self, $Object, $Level, $Pos ) = @_;
    
    # Mark object as visited
    $Self->{VisitedObjects}->{$Object}++;
    
    my $s = $Object->ToString;
    
    # Get links of current object, follow them
    my @TraceSteps = ( );
    my $IncludeInvalidObjects = $Self->{IncludeInvalidObjects};
    my $LinkList = $Object->GetLinkList;
    for my $LinkedClass (keys %$LinkList)
    {
	if ($LinkedClass eq 'Ticket')
	{
	    1;		# DEBUG: x %{$LinkList->{$LinkedClass}}	    
	    next;	# always skipped
	}
	for my $LinkType (keys %{$LinkList->{$LinkedClass}})
	{
	    next unless $Self->_isLinkTypeAllowed($LinkType);	# filtered?
	    my $LinkDirType = $Self->{KnownLinkTypes}->{$LinkType};
	    my $PosDelta = ( $LinkDirType eq '=' ? 0 : 1 );

	    # Follow out-links
	    # For "top-down flow", we only follow _directed_ outlinks if we are
	    # at or "below" the root obj and the link goes "down" or it is
	    # a non-directed link
#	    if ( $PosDelta && ($Pos>=0) || !$PosDelta ) {
	    if ( 1 ) {
		my @TargetIDs = 
			keys %{$LinkList->{$LinkedClass}->{$LinkType}->{Target}};
		for my $ID (@TargetIDs)
		{
		    my $TargetObject = $Object->new(
			Type => $LinkedClass,
			ID => $ID
		    );

                    # Check for dead link
		    if (!$TargetObject)	
		    {
			$Self->{LogObject}->Log(
			    Priority => 'error',
			    Message  => "Link <<$LinkType>> from "
			    	      . $Object->ToString()
			    	      . " to non-existing object with id $ID.",
			);
			next;
		    } 

		    # Filter by attributes of object
		    next unless $TargetObject->IsValid || $IncludeInvalidObjects;
		    next unless $Self->_isObjectTypeAllowed($TargetObject);
		    next unless $Self->_isObjectInciStateAllowed($TargetObject);

		    # Link already traversed (in opposite direction)?
		    my $LinkSignature = $Object.$LinkType.$TargetObject;
		    next if $Self->{VisitedLinks}->{$LinkSignature};

		    # Prepare trace step for target object
		    my $Visited = $Self->{VisitedObjects}->{$TargetObject};
		    push @TraceSteps, { 
			Level => $Level,
			LinkType => $LinkType,
			LinkDir => 'out',
			LinkDirType => '',
			Object1 => $Object,
			Object2 => $TargetObject,
			Position => $Pos,
			Visited => $Visited,
		    };

		    # Check for end of recursion
		    next if $Visited;		# already seen
		    next if $Self->{MaxTraceDepth} && $Level >= $Self->{MaxTraceDepth};

		    # Save link that we just traversed
		    $Self->{VisitedLinks}->{$LinkSignature}++;

		    # Recurse
		    my $SubTraceSteps = $Self->_followLinks( 
			$TargetObject, 
			$Level+1,
			$Pos + $PosDelta,	# "down"
		    );
		    push @TraceSteps, @$SubTraceSteps 
		    	   if scalar(@$SubTraceSteps);
		} #for
	    } #if
		    
	    # Follow in-links
	    # For "top-down flow", we only follow _directed_ inlinks if we are
	    # at or "above" (<0) the root obj and the link goes "up" or it is
	    # a non-directed link
#	    if ( $PosDelta && ( $Pos <= 0 ) || !$PosDelta ) {
	    if ( 1 ) {
		my @SourceIDs = keys %{$LinkList->{$LinkedClass}->{$LinkType}->{Source}};
		for my $ID (@SourceIDs)
		{
		    my $SourceObject = $Object->new(
			Type => $LinkedClass,
			ID => $ID
		    );
		    if (!$SourceObject)		# dead link?
		    {
			$Self->{LogObject}->Log(
			    Priority => 'error',
			    Message  => "Link <<$LinkType>> to "
			    	      . $Object->ToString()
			    	      . " from non-existing object with id $ID.",
			);
			next;		# skip
		    } #if
		    # Filter by attributes of object
		    next unless $SourceObject->IsValid || $IncludeInvalidObjects;
		    next unless $Self->_isObjectTypeAllowed($SourceObject);
		    next unless $Self->_isObjectInciStateAllowed($SourceObject);

		    # Link already traversed (in opposite direction)?
		    my $LinkSignature = $SourceObject.$LinkType.$Object;
		    next if $Self->{VisitedLinks}->{$LinkSignature};

		    # Prepare trace step for source object
		    my $Visited = exists $Self->{VisitedObjects}->{$SourceObject};
		    push @TraceSteps, { 
			Level => $Level,
			LinkType => $LinkType,
			LinkDir => 'in',
			LinkDirType => '',
			Object1 => $Object,
			Object2 => $SourceObject,
			Position => $Pos,
			Visited => $Visited,
		    };

		    # Save link that we just traversed
		    $Self->{VisitedLinks}->{$LinkSignature}++;

		    # Recurse if
		    #   - no loop
		    #   - no trace depth limit or within this limit
		    #   - not top-down only or this was a directed link
		    if ( ! $Visited && ( ! $Self->{MaxTraceDepth} || $Self->{MaxTraceDepth} && $Level < $Self->{MaxTraceDepth} ) )
		    {
			my $SubTraceSteps = $Self->_followLinks( 
			    $SourceObject, 
			    $Level+1,
		       	    $Pos - $PosDelta,	# "up"
			);
			push @TraceSteps, @$SubTraceSteps if scalar(@$SubTraceSteps);
		    }
		} #for
	    } #if
	} #for
    } #for
    return \@TraceSteps;
} 

########################################################################
#
# Private method to render a trace as plaintext
sub _renderAsText {
    my ($Self, $TraceSteps) = @_;
    my @Text;
    for my $Step ( @$TraceSteps ) {
	my $Link = '';
	if ($Step->{LinkType})
	{
	    if ($Step->{LinkDirType} eq '=') {
		$Link = '<-->';		# non-directed
	    } else {
		$Link = $Step->{LinkDir} eq 'in' ? '<-' : '->';
	    }
	    $Link .= "  <<" . $Step->{LinkType} . ">>  ";
	} #if
	my $ClusterInfo = '';
	if (exists $Self->{Object2Scope}->{$Step->{Object2}}) {
	    my $Scope =  $Self->{Object2Scope}->{$Step->{Object2}};
	    $ClusterInfo = ' [' . $Scope->ToString . ']';
	}
	my $Text = ("  " x $Step->{Level})	# indent
		 . $Link			# link to/from Object2
		 . $Step->{Object2}->ToString
		 . $ClusterInfo
		 . ($Step->{Visited} ? ' (already visited)' : '');
	push @Text, $Text;
    } #for
    return { text => join( "\n", @Text, '') };
}

########################################################################
#
# Private method to load initial service object(s), returns ARRAY ref.
# No filtering except that a "wildcard" (ID=0) respects the setting
# of 'IncludeInvalidObjects'.
sub _expandServiceID {
    my ($Self, $ServiceID) = @_;

    return [ ] unless @{ $ServiceID }[0] ne '';

    my @Objects;
    if ($ServiceID > 0) {
	# Single service, try to load it
	my $Object = Kernel::System::CMDBExplorer::ObjectWrapper->new(
	    %{$Self->_GetCommonObjects}, 
	    Type => 'Service', 
	    ID => $Self->{ServiceID}
	);
	push (@Objects, $Object) if $Object;
    } else { 
	# All known services
	my %ServiceList = $Self->{ServiceObject}->ServiceList(
	    Valid  => ! $Self->{IncludeInvalidObjects},
	    UserID => 1,
	);
	for my $ID ( keys %ServiceList ) {
	    my $Object = Kernel::System::CMDBExplorer::ObjectWrapper->new(
		%{$Self->_GetCommonObjects}, 
		Type => 'Service', 
		ID => $ID,
	    );
	    next unless $Object;
	    push (@Objects, $Object);
	} 
	# Sort services by "rank" (top-level services first)
	# This gives _much_ more useful text output
	@Objects = sort { $a->GetRank <=> $b->GetRank } @Objects;
    } #else
    return \@Objects;
}

########################################################################
#
# Private method to load initial config item(s); returns ARRAY ref.
# Explicitly given CIs (by ID) are not filtered, otherwise filtering
# respects IncludeInvalidObjects and ObjectTypes.
sub _expandConfigItemID {
    my ($Self, $ConfigItemID) = @_;

    return [ ] unless @{ $ConfigItemID }[0] ne '';

    my @Objects = ( );

    my $IncludeInvalidObjects = $Self->{IncludeInvalidObjects};

    foreach my $CI ( @{ $ConfigItemID } ) {
        if ($CI != 0) {
            # Single config item, try to load it
	    my $Object = Kernel::System::CMDBExplorer::ObjectWrapper->new(
	        %{$Self->_GetCommonObjects}, 
 	        Type => 'ITSMConfigItem', 
	        ID => $CI,
	    );
            next unless $Object;
            next unless $Object->IsValid || $IncludeInvalidObjects;
            next unless $Self->_isObjectTypeAllowed($Object);
  	    push (@Objects, $Object) if $Object;
        } else { 
	# All config items; load & filter
    	    my $ConfigItemList = $Self->{ConfigItemObject}->ConfigItemSearch();
	    my $IncludeInvalidObjects = $Self->{IncludeInvalidObjects};
	    for my $ID ( @{$ConfigItemList} ) {
	        my $Object = Kernel::System::CMDBExplorer::ObjectWrapper->new(
		    %{$Self->_GetCommonObjects}, 
		    Type => 'ITSMConfigItem', 
		    ID => $ID,
	        );
	        next unless $Object;
	        next unless $Object->IsValid || $IncludeInvalidObjects;
	        next unless $Self->_isObjectTypeAllowed($Object);
	        push (@Objects, $Object);
	    } 
        } #else
    } #foreach

    return \@Objects;
} 

########################################################################
#
# Private method to check if a given link type meets the current constraints
sub _isLinkTypeAllowed {
    my ($Self, $LinkType) = @_;
    return 1 unless exists $Self->{LinkTypes};		# not filtered
    return 1 if $Self->{LinkTypes}->{$LinkType};	
    if ($Self->{Debug}) {
	$Self->{LogObject}->Log(
	    Priority => 'debug',
	    Message  => "Skipping filtered link type '$LinkType'.",
	);
    }
    return 0;
}

########################################################################
#
# Private method to check if a given object meets the current
# type constraints
sub _isObjectTypeAllowed {
    my ($Self, $Object) = @_;
    return 1 unless exists $Self->{ObjectTypes};		# not filtered
    return 1 if $Self->{ObjectTypes}->{$Object->GetType};	# CI w/o class
    return 1 if $Self->{ObjectTypes}->{$Object->GetFullType};
    if ($Self->{Debug}) {
	my $T = $Object->GetFullType;
	$Self->{LogObject}->Log(
	    Priority => 'debug',
	    Message  => "Skipping filtered object type '$T'.",
	);
    }
    return 0;
}

########################################################################
#
# Private method to check if a given object meets the current 
# incident state constraints (when following only "hot" links)
sub _isObjectInciStateAllowed {
    my ($Self, $Object) = @_;
    return 1 unless $Self->{TraceIncident};
    return 1 if $Object->GetCurInciState ne 'operational';
    return 0;
}
1;

=back

=head1 TERMS AND CONDITIONS

Copyright (C) 2014- Belnet, http://www.belnet.be/
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
