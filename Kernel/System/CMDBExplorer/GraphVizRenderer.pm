# --
# Kernel/System/CMDBExplorer/GraphVizRenderer.pm - render trace through graphviz library
# Copyright (C) 2014- Belnet, http://www.belnet.be
# Copyright (C) 2011 Thales Austria GmbH, http://www.thalesgroup.com/
# --
# $Id: CMDBExplorer.pm $
# --
# This software comes with ABSOLUTELY NO WARRANTY and WITHOUT ANY SUPPORT. 
# For license information, see the enclosed file COPYING-CMDBExplorer
# (GNU AFFERO GENERAL PUBLIC LICENSE, version 3). 
# If you did not receive this file, see 
# http://www.gnu.org/licenses/agpl-3.0.html.

# --

package Kernel::System::CMDBExplorer::GraphVizRenderer;

=head1 NAME

Kernel::System::CMDBExplorer::GraphVizRenderer - render trace through graphviz library


=head1 SYNOPSIS

This class encapsulates the graphical rendering of a trace (represented
by an array of trace steps) through the graphivz library from 
http://www.graphviz.org.

=cut

use strict;
use warnings;

use vars qw($VERSION);
$VERSION = '0.6';

use Kernel::Config;
use Kernel::System::Service;
use Kernel::System::ITSMConfigItem;
use Kernel::System::LinkObject;
use Kernel::System::GeneralCatalog;

use Kernel::System::CMDBExplorer::ObjectWrapper;

use GraphViz;


########################################################################
# 
# Mapping of object/link types to visual representation
#
our %InciStateColors =
(
    warning	=> '#FFDD50', # yellow
    incident	=> '#FF505E', # red
);

our %ClusterAttrs = 
(
    color => '#444444',
    penwidth => 3,
);


=head1 PUBLIC INTERFACE

=over 4

=cut

###  C o n s t r u c t o r  ############################################
#

=item new()

Creates an object

    my $GraphVizRenderer = Kernel::System::CMDBExplorer::GraphVizRenderer->new();

=cut
sub new {
    my ( $Type, %Param ) = @_;

    # allocate new hash for object
    my $Self = {};
    bless( $Self, $Type );

    $Self->{Debug}        = $Param{Debug} || 0;
    $Self->{RootCI}       = $Param{RootCI} || 0;
    $Self->{DisplayedCIs} = $Param{DisplayedCIs};
    $Self->{Layout}       = $Param{Layout} || 'dot';

    return $Self;
}

###  M e t h o d s  ####################################################
#

=item Render()

Renders the graph given by C<$TraceSteps> through graphviz with
the built-in mapping from objects & links to graphical elements. 

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

    my $Output = $GraphVizRenderer->Render(
        TraceSteps	=> $TraceSteps,
        OutputFormat 	=> 'imgmap', 	# (optional), default = 'dot'
        OutputOption 	=> 'rotate', 	# (optional) (format-specific)
        ClusterMap 	=> $Object2Scope # (optional) (object-to-scope mapping)
    );
    # $Output->{png} now contains the PNG image of the graph, 
    # $Output->{map} contains the client-side image map for it.
    # If output is requested in 'dot' format, this is returned
    # as $Output->{dot}.

=cut
sub Render
{
    my ($Self, %Param) = @_;
    my $TraceSteps   = $Param{TraceSteps} || [ ];
    my $OutputFormat = $Param{OutputFormat} ||'dot';
    my $OutputOptions = $Param{OutputOptions} ||'';
    $Self->{ClusterMap} = $Param{ClusterMap} if exists $Param{ClusterMap};

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
    }

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
    # Set selected layout's options from SysConfig
    my $LayoutOptions = $Self->{LayoutOptions}->{$Self->{Layout}} || {};

    # Prepare graph
    my $RankDir = ($OutputOptions =~ m/rotate/i ? 1 : 0);
    my $g = GraphViz->new(
        rankdir => $RankDir,
        name    => 'trace',
        layout  => $Self->{Layout},
        %$LayoutOptions,
    );
    $Self->{GraphVizObject} = $g;

    # Render nodes
    for my $Object (values %Nodes) {
	$Self->_renderObject($Object);
    } 

    # Add edges
    for my $Link (values %Edges)
    {
	$Self->_renderLink($Link);
    } 

    # Produce output
    my $Output = { };
    if ($OutputFormat eq 'png') {
	$Output->{png} = $g->as_png;
    } elsif ($OutputFormat eq 'imgmap') {
	$Output->{map} = $g->as_cmapx;
	$Output->{png} = $g->as_png;
    } else {
	$Output->{dot} = $g->as_canon;
    }
    return $Output;
} 

########################################################################
# Private method that controls the rendering of a single object 
# as a GraphViz node.
sub _renderObject {
    my ($Self, $Object) = @_;

    my %attrs;

    # Set node'shape
    $attrs{shape} = $Self->{GraphOptions}->{NodeShapes}->{$Object->GetFullType};

    # Cluster support?
    if ( exists $Self->{ClusterMap}->{$Object} ) {
	my $Scope = $Self->{ClusterMap}->{$Object};
	my $ClusterID = $Scope->GetAllScopeIDsList;
	my $Cluster;
	if (! exists $Self->{ClusterID2Cluster}->{$ClusterID} ) {
	    $Cluster = { %ClusterAttrs };	# new
	    $Self->{ClusterID2Cluster}->{$ClusterID} = $Cluster;
	    $Cluster->{name} = $Scope->ToString; # ClusterID if $Self->{Debug};
	} else {
	    $Cluster = $Self->{ClusterID2Cluster}->{$ClusterID};
	} #else
	$attrs{cluster} = $Cluster;
    } #if

    # Type-specific refinement
    my $Type = $Object->GetFullType;
    if ( $Type eq 'Service' )
    {
	my $Name = $Object->GetName;
	my $Level = scalar (my @x = split /::/, $Name);
	$attrs{URL} = 'index.pl?Action=AgentITSMServiceZoom;ServiceID='.$Object->GetID;
	$attrs{tooltip} = "Service: " . __escape($Object->GetName);
    }
    elsif ( $Type =~ m/^ITSMConfigItem/ )
    {
        # Create URL
        my $url ='index.pl?Action=AgentITSMConfigItemZoom;ConfigItemID=';
        $url .= $Object->GetID;
        $url .= ';DisplayedCIs=' . join(',' , @{ $Self->{DisplayedCIs} });

        # Add or Remove node from URL
        if ( $Object->GetID ~~  @{ $Self->{DisplayedCIs} } ) {
            my $ID = $Object->GetID;
            $url =~ s/,$ID//;
            $url =~ s/$ID,//;
        } else {
            $url .= ',';
            $url .= $Object->GetID;
        }

        # Add Layout to URL
        $url .= ';Layout=' . $Self->{Layout};

        # Add URL attribute
        $attrs{URL} = $url;

        # Add tooltip attribute
	my $CIType = $Type;
	$CIType =~ s/^.*:://;		# leave only specific CI type
	$attrs{tooltip} = "$CIType: " . __escape($Object->GetName);
    }

    # Visually mark invalid items
    $attrs{style} = 'diagonals' unless $Object->IsValid;

    # Set font size
    $attrs{fontsize} = $Self->{GraphOptions}->{NodeFontSize};

    # Set minimum node size
    $attrs{height} = 0.1;
    $attrs{width}  = 0.2;
    $attrs{margin} = "0.03,0.03";

    # Set default filling attributes
    $attrs{style} = 'filled';
    $attrs{fillcolor} = 'white';

    # Visually mark non-operational state
    my $InciStateColor = $InciStateColors{$Object->GetCurInciState};
    $attrs{fillcolor} = $InciStateColor if $InciStateColor;

    # Set shape of currently selected CI
    if ( $Object->GetID eq $Self->{RootCI} ) {
        $attrs{shape} = 'doubleoctagon';
    }

    # Add node to graph
    $Self->{GraphVizObject}->add_node(
	$Object->GetKey, 
	label => __escape($Object->GetShortName), 
	%attrs
    );
}

########################################################################
#
# Private method that controls the rendering of a single link as an edge
sub _renderLink {
    my ($Self, $Link) = @_;

    my %attrs;

    # Set link style and arrow
    $attrs{style} = $Self->{GraphOptions}->{LinkStyles}->{$Link->{LinkType}};
    $attrs{dir} = $Self->{GraphOptions}->{LinkArrows}->{$Link->{LinkType}};

    # Set font size
    $attrs{fontsize} = $Self->{GraphOptions}->{LinkFontSize};

    # Add graphviz edge
    $Self->{GraphVizObject}->add_edge(
	$Link->{Source}->GetKey => $Link->{Target}->GetKey, # from -> to
	label 		        => $Link->{LinkType},
        tooltip => $Link->{LinkType},
	%attrs,
    );
}

########################################################################
#
# Function to protect quotes (i.e. convert " to ')
sub __escape($) { 
    my $s = shift;
    $s =~ s/"/'/g;
    return $s;
}
1;

=back

=head1 TERMS AND CONDITIONS

Copyright (C) 2014- Belnet, http://www.belnet.be
Copyright (C) 2011 Thales Austria GmbH, http://www.thalesgroup.com/

This software comes with ABSOLUTELY NO WARRANTY and WITHOUT ANY SUPPORT. 

For license information, see the enclosed file COPYING-CMDBExplorer
(GNU AFFERO GENERAL PUBLIC LICENSE, version 3). 
If you did not receive this file, see 
http://www.gnu.org/licenses/agpl-3.0.html.

=head1 VERSION

0.6

=head1 AUTHOR

cyrille.bollu@belnet.be
dietmar.berg@thalesgroup.com

=cut
