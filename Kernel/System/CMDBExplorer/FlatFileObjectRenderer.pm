# --
# Kernel/System/CMDBExplorer/FlatFileObjectRenderer.pm - render CIs in flat file format
# Copyright (C) 2012-2014 Thales Austria GmbH, http://www.thalesgroup.com/
# --
# $Id: CMDBExplorer.pm $
# --
# This software comes with ABSOLUTELY NO WARRANTY and WITHOUT ANY SUPPORT. 
# For license information, see the enclosed file COPYING-CMDBExplorer
# (GNU AFFERO GENERAL PUBLIC LICENSE, version 3). 
# If you did not receive this file, see 
# http://www.gnu.org/licenses/agpl-3.0.html.

# --

package Kernel::System::CMDBExplorer::FlatFileObjectRenderer;

=head1 NAME

Kernel::System::CMDBExplorer::FlatFileObjectRenderer - dump objects in tabular format


=head1 SYNOPSIS

This class encapsulates the tabular ("flat") rendering of objects in a trace
(represented by an array of trace steps) for generating CSV, e.g. for OTRS stats.

B<DISCLAIMER:> Ths code is largely experimental and far from being "finished"!
Use at your own risk.

=cut

use strict;
use warnings;

use vars qw($VERSION);
$VERSION = '0.3';

use Kernel::System::Service;
use Kernel::System::ITSMConfigItem;
use Kernel::System::LinkObject;
use Kernel::System::GeneralCatalog;
use Kernel::System::CustomerUser;

use Kernel::System::CMDBExplorer::ObjectWrapper;


########################################################################
# 

=head1 PUBLIC INTERFACE

=over 4

=cut
#
########################################################################


###  C o n s t r u c t o r  ############################################
#

=item new()

Creates an object

    my $FlatFileObjectRenderer = Kernel::System::CMDBExplorer::FlatFileObjectRenderer->new();

=cut

sub new {
    my ( $Type, %Param ) = @_;

    # allocate new hash for object
    my $Self = {};
    bless( $Self, $Type );

    $Self->{Debug} = $Param{Debug} || 0;
    return $Self;
} # new()
#
########################################################################


###  M e t h o d s  ####################################################
#

=item Render()

Dumps the objects in the graph given by C<$TraceSteps>. 

    $TraceSteps = [ 
	{ 
	    Level       => $Level,          # recursion level
	    LinkType    => $LinkType,       # type of link to object
	    LinkDir     => 'in',            # 'out' | 'in' | '' (=root)
	    LinkDirType => $LinkDirType,    # '>' | '='
	    Object1     => $Object,         # object at "this" end of link
	    Object2     => $TargetObject,   # object at "other" end of link
	    Position    => $RelPosition,    # relative pos. in dep. chain
	    Visited     => $Visited,        # flag if ob has been visited before
	},
	{
	    ...
	}, 
	...
    ];

    # Object-to-scope mapping (see Kernel::System::CMDBExplorer::Scope)
    $Object2Scope = { $Object => $Scope, ... }

    my $Output = $FlatFileObjectRenderer->Render(
        TraceSteps	=> $TraceSteps,
#        OutputFormat 	=> 'imgmap', 	# (optional), default = 'dot'
#        OutputOptions 	=> 'rotate', 	# (optional) (format-specific)
#        ClusterMap 	=> $Object2Scope # (optional) (object-to-scope mapping)
    );
    # $Output->{flat} now contains a two-dimensional array of object and their fields, un-escaped.
=cut

sub Render
{
    my ($Self, %Param) = @_;
    my $TraceSteps   = $Param{TraceSteps} || [ ];
    $Self->{ClusterMap} = $Param{ClusterMap} if exists $Param{ClusterMap};

    # What fields to include in the output
    my @WantedFields = ( '*' );	# default is everything
    @WantedFields = @{$Param{OutputOptions}->{WantedFields}}
		if ref $Param{OutputOptions}->{WantedFields};
    unshift @WantedFields, qw( Class Name Number ConfigItemID );
    $Self->{WantedFields} = [ @WantedFields ];
    $Self->{WantedFieldsLookup}->{$_} = 1 for @WantedFields;

    # Turn trace steps into (unique) nodes and edges
    my %Nodes;
    my %Edges;
    for my $TraceStep ( @$TraceSteps ) {
	# Record all objects as nodes
	my $Object1 = $TraceStep->{Object1};
	my $Object2 = $TraceStep->{Object2};
	my $LinkDir = $TraceStep->{LinkDir};
	my $LinkIsDirected = ($TraceStep->{LinkDirType} ne '=');
	$Nodes{$Object2} = $Object2;	# get unique objs (obj2 is enough)

	# Record all links as edges.
	# OTRS always has 2 entries for each link (from either side), 
	# we need to make sure that each link is drawn just once.
	my $LinkType = $TraceStep->{LinkType};
	if ($LinkType) {	# "root" steps don't have a link!
	    my $Source = $LinkDir eq 'out' ? $Object1 : $Object2;
	    my $Target = $LinkDir eq 'out' ? $Object2 : $Object1;
	    $Edges{$Source.$LinkType.$Target} = { 
		Source      => $Source, 
		LinkType    => $LinkType, 
		LinkDirType => $TraceStep->{LinkDirType}, 
		Target      => $Target,
	    } unless exists $Edges{$Target.$LinkType.$Source};	
	}
    } #for

    # Count hierarchy levels of services for later layout control
    my $MaxSvcLevel = 0;
    for my $Object (values %Nodes)
    {
	next unless $Object->GetType eq 'Service';
	my $Name = $Object->GetName;
	my $Level = scalar (my @x = split /::/, $Name);
	$MaxSvcLevel = $Level if $Level > $MaxSvcLevel;
    } # for
    $Self->{MaxSvcLevel} = $MaxSvcLevel;

    # Render all nodes
    for my $Object (values %Nodes) {
	warn ("Node: ", $Object->ToString, "\n") if $Self->{Debug};
	$Self->_renderObject($Object);
    } #for

    # Produce output
    my $Output = { };
    # Get all field names that we have found
    my %FieldNames;
    for my $Item ( @{$Self->{Items}} ) {
	for my $FieldName ( keys %{$Item} ) {
	    next unless $FieldName;
	    next if $FieldNames{$FieldName};
	    $FieldNames{$FieldName}++;
	}
    }
    # Sort fields according to explicitly given "WantedFields"
    my %FieldMapping;
    my $FieldIndex = 0;
    for my $FieldName ( @{$Self->{WantedFields}} ) {
	next unless $FieldNames{$FieldName};	# found?
	$FieldMapping{$FieldName} = $FieldIndex++;
	delete $FieldNames{$FieldName};
    } #for
    # Add remaining ones
    $FieldMapping{$_} = $FieldIndex++ for sort keys %FieldNames;

    # Get headers into row 0
    my @FieldList = sort { $FieldMapping{$a} <=> $FieldMapping{$b} } keys %FieldMapping;
    my $Data = [ ];
    $Data->[0] = [ @FieldList ];
    # Put data into array, row by row
    for my $Item ( @{$Self->{Items}} ) {
	my $Row = [ ];
	for my $FieldName ( keys %{$Item} ) {
	    $Row->[$FieldMapping{$FieldName}] = $Item->{$FieldName};
	}
	push @{$Data}, $Row;
    }
    $Output->{flat} = $Data;

    # TODO
    #
    return $Output;
} # Render()
#
########################################################################



########################################################################
# Private method that controls the rendering of a single object.
sub _renderObject {
    my ($Self, $Object) = @_;

    my %attrs;

    # Type-specific refinement
    my $Type = $Object->GetFullType;
    if ( $Type eq 'Service' )
    {
	my $Name = $Object->GetName;
	my $Level = scalar (my @x = split /::/, $Name);
	$attrs{URL} = 'index.pl?Action=AgentITSMServiceZoom;ServiceID='.$Object->GetID;
    }
    elsif ( $Type =~ m/^ITSMConfigItem/ ) {
	$attrs{URL} = 'index.pl?Action=AgentITSMConfigItemZoom;ConfigItemID='.$Object->GetID;
	my $CIType = $Type;
	$CIType =~ s/^.*:://;		# leave only specific CI type
	my $Data = $Self->_getCIData($Object);
	$Self->{Items} = [ ] unless $Self->{Items};
	push @{$Self->{Items}}, $Data;
    }

    # Visually mark invalid items
#    $attrs{style} = 'diagonals' unless $Object->IsValid;

    # Visually mark non-operational state
#    my $InciStateColor = $InciStateColors{$Object->GetCurInciState};
#    $attrs{fillcolor} = $InciStateColor if $InciStateColor;

} # _renderObject()


sub _getCIData {
    my ($Self, $Object) = @_;
#    my %Param = ( Prefix => '', PrefixName => '' );
    my $CIVers = $Object->{ITSMConfigItem}->{LastVersion};
    if ( !$CIVers->{XMLDefinition} ) {
	$CIVers = $Object->{ConfigItemObject}->VersionGet(
	    VersionID => $CIVers->{VersionID},  
	    XMLDataGet => 1,
	);
    }

    # Customer user name expansion & cache
    if (! $Self->{CustomerUserObject} ) {
	$Self->{CustomerUserObject} = Kernel::System::CustomerUser->new(%$Object);    
	$Self->{CustomerUserNames} = { };	# login -> fullname
    }

    my %Fields;		# accumulation of return data
    # Get all fields that we have in the "base" record
    my %WantedFieldsLookup = %{$Self->{WantedFieldsLookup}};
    $Fields{$_} = $CIVers->{$_} || '' for keys %WantedFieldsLookup;

#    if ( $CIVers->{XMLDefinition} ) {
#	for my $Item ( @{ $CIVers->{XMLDefinition} } ) {
#
#	    next if !$Item->{Searchable} && !$Item->{Sub};
#
#	    # create key and name
#	    my $Key  = $Param{Prefix} . $Item->{Key};
#	    my $Name = $Param{PrefixName} . $Item->{Name};
#
#	    # add attribute
##	    my $Attribute = $Self->{ConfigItemObject}->XMLStatsAttributeCreate(
##		Key  => $Key,
##		Item => $Item,
##		Name => $Name,
##	    );
#
###	    next if !$Attribute;
##	    next if ref $Attribute ne 'ARRAY';
##	    next if !scalar @{$Attribute};
#
#	    # add attributes to object array
##	    push @{ $Param{ObjectAttributes} }, @{$Attribute};
#
##	    next if !$Item->{Sub};
#	}	
#    }

    if ( $CIVers->{XMLData} ) {
	for my $XMLChunk ( @{$CIVers->{XMLData}} ) {
	    next unless $XMLChunk->{Version};
	    my $TagKey = $XMLChunk->{TagKey};
	    for my $XMLVersion ( @{$XMLChunk->{Version}} ) {
		next unless    $XMLVersion->{TagKey} 
			    && $XMLVersion->{TagKey} =~ m#^\Q$TagKey#;
		for my $Field (keys %{$XMLVersion} ) {
		    next if $Field eq 'TagKey';		# not a field, but a marker
		    next unless    $WantedFieldsLookup{'*'} 
		    		|| $WantedFieldsLookup{$Field};
		    for my $Item ( @{$XMLVersion->{$Field}} ) {
			next unless    $Item->{TagKey} 
				    && $Item->{TagKey} =~ m#^\Q$TagKey#;
			$Fields{$Field} .= "\n" if $Fields{$Field};
			my $Content = $Item->{Content} || '';
			$Content =~ s/\r?\n/\n/mg;	# strip CRs
			$Content =~ s/^\n+//s;		# strip leading LFs
			$Content =~ s/\n+$//s;		# strip trailing LFs
			# TODO: Only apply to "content" that has semantic of a person's name!
			$Content = $Self->_expandLogin($Content) 
					if $Content && $Content !~ m/\s/;
			$Fields{$Field} .= $Content;
		    }
		}
	    }
	}
    }
    return \%Fields;
}


sub _expandLogin {
    my ($Self, $Login) = @_;
    my $Name = $Self->{CustomerUserNames}->{$Login};
    return $Name if $Name;	# already in cache?
    $Name = $Self->{CustomerUserObject}->CustomerName(
	UserLogin => $Login,
    );
    $Self->{CustomerUserNames}->{$Login} = $Name || $Login;
    return $Self->{CustomerUserNames}->{$Login};
}
#
########################################################################



########################################################################
#
# Private method that controls the rendering of a single link as an edge
sub _renderLink {
    my ($Self, $Link) = @_;

    my %attrs;
    $Self->{CsvObject}->add_edge(
	$Link->{Source}->GetKey => $Link->{Target}->GetKey, # from -> to
	label 		        => $Link->{LinkType},
	%attrs,
    );
} # _renderLink()
#
########################################################################



########################################################################
# Function to protect quotes (i.e. convert " to ')
sub __escape($) { 
    my $s = shift;
    $s =~ s/"/'/g;
    return $s;
}
#
########################################################################
1;

=back

=head1 TERMS AND CONDITIONS

Copyright (C) 2012-2014 Thales Austria GmbH, http://www.thalesgroup.com/

This software comes with ABSOLUTELY NO WARRANTY and WITHOUT ANY SUPPORT. 

For license information, see the enclosed file COPYING-CMDBExplorer
(GNU AFFERO GENERAL PUBLIC LICENSE, version 3). 
If you did not receive this file, see 
http://www.gnu.org/licenses/agpl-3.0.html.


=head1 AUTHOR

dietmar.berg@thalesgroup.com

=cut
