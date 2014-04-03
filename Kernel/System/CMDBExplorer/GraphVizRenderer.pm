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
    Warning	=> '#FFDD50', # yellow
    Incident	=> '#FF505E', # red
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

    # Get config
    $Self->{ConfigObject} = Kernel::Config->new();
    $Self->{GraphOptions} = $Self->{ConfigObject}->Get("CMDBExplorer::GraphOptions");
    $Self->{LayoutOptions} = $Self->{ConfigObject}->Get("CMDBExplorer::GraphOptions::LayoutOptions");

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
    );

    # $Output->{png} now contains the PNG image of the graph, 
    # $Output->{map} contains the client-side image map for it.

=cut
sub Render
{
    my ($Self, %Param) = @_;
    my $TraceSteps   = $Param{TraceSteps} || [ ];

    # Turn trace steps into (unique) nodes and edges
    my %Nodes;
    my %Edges;
    for my $TraceStep ( @$TraceSteps ) {
	# Record all objects as nodes
	my $Object1 = $TraceStep->{Object1};
	my $Object2 = $TraceStep->{Object2};
	my $LinkDir = $TraceStep->{LinkDir};
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

    # Set selected layout's options from SysConfig
    my $LayoutOptions = $Self->{LayoutOptions}->{$Self->{Layout}} || {};

    # Prepare graph
    my $g = GraphViz->new(
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
    for my $Link (values %Edges) {
	$Self->_renderLink($Link);
    } 

    # Produce output
    my $Output = { };
    $Output->{map} = $g->as_cmapx;
    $Output->{png} = $g->as_png;

    return $Output;
} 

########################################################################
# Private method that controls the rendering of a single object 
# as a GraphViz node.
sub _renderObject {
    my ($Self, $Object) = @_;

    my %attrs;

    # Set default node attributes
    $attrs{shape} = $Self->{GraphOptions}->{NodeShapes}->{$Object->GetFullType};
    $attrs{fontsize} = $Self->{GraphOptions}->{NodeFontSize} || 8;
    $attrs{color} = $Self->{GraphOptions}->{NodeDefaultColor} || 'LightSteelBlue4';
    $attrs{fillcolor} =  $Self->{GraphOptions}->{NodeDefaultFillColor} || 'white';
    $attrs{height} = 0.1;
    $attrs{width}  = 0.2;
    $attrs{margin} = "0.03,0.03";
    $attrs{style} = 'filled';

    # Type-specific refinement
    my $Type = $Object->GetFullType;
    if ( $Type eq 'Service' ) {
	my $Name = $Object->GetName;
	my $Level = scalar (my @x = split /::/, $Name);
	$attrs{URL} = 'index.pl?Action=AgentITSMServiceZoom;ServiceID='.$Object->GetID;
	$attrs{tooltip} = "Service: " . __escape($Object->GetName);
    }
    elsif ( $Type =~ m/^ITSMConfigItem/ ) {
        my $ID = $Object->GetID;

        # URL init
        my $url ='index.pl?Action=AgentITSMConfigItemZoom;ConfigItemID=';
        my $urlDisplayedCIs=';DisplayedCIs=';
        if ( $Self->{DisplayedCIs} ) {
            $urlDisplayedCIs .= join(',' , @{ $Self->{DisplayedCIs} });
        }

        # Tooltip init
	my $Tooltip = $Type;
        $Tooltip =~ s/^.*:://; 
        $Tooltip .= ': ';
        $Tooltip .= __escape($Object->GetName);

        # Change attributes depending on CI
        if ( $ID eq $Self->{RootCI} ) {
            $attrs{fillcolor} = $Self->{GraphOptions}->{RootNodeColor} || 'LightSteelBlue2';
            $url .= $ID;
            $Tooltip .= " (Root CI)";
        } elsif ( $ID ~~  @{ $Self->{DisplayedCIs} } ) {
            # Clicking on the node will remove it from the graph
            $attrs{fillcolor} = $Self->{GraphOptions}->{DisplayedNodeColor} || 'LightSteelBlue1';
            $url .= $Self->{RootCI};
            $urlDisplayedCIs =~ s/,$ID//;
            $urlDisplayedCIs =~ s/$ID,//;
            $Tooltip .= " (Click to remove from graph)";
        } else {
            # Clicking on the node will add it from the graph
            $url .= $ID;
            $urlDisplayedCIs .= ',' . $ID;
            $Tooltip .= " (Click to add to graph)";
        }
        $url .= $urlDisplayedCIs;
        $url .= ';Layout=' . $Self->{Layout};

        # Add URL attribute
        $attrs{URL} = $url;

        # Add tooltip attribute
	$attrs{tooltip} = $Tooltip;
    }

    # Visually mark invalid items
    $attrs{style} = 'diagonals' unless $Object->IsValid;

    # Visually mark non-operational state
    my $InciStateColor = $InciStateColors{$Object->GetCurInciState};
    $attrs{color} = $InciStateColor if $InciStateColor;

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

    # Set default link attributes
    $attrs{style} = $Self->{GraphOptions}->{LinkStyles}->{$Link->{LinkType}} || 'filled';
    $attrs{dir} = $Self->{GraphOptions}->{LinkArrows}->{$Link->{LinkType}} || 'none';
    $attrs{fontsize} = $Self->{GraphOptions}->{LinkFontSize} || 6;
    $attrs{color} =  $Self->{GraphOptions}->{LinkDefaultColor} || 'LightSteelBlue4';
    $attrs{fontcolor} =  $Self->{GraphOptions}->{LinkDefaultFontColor} || 'LightSteelBlue4';
    $attrs{label} = $Link->{LinkType} if ($Self->{GraphOptions}->{DisplayLinksName});

    # Mark links between CI in non-operational state
    if ( $Link->{LinkType} eq 'DependsOn' ) {
        for my $Node ( 'Source', 'Target' ) {
            if ( $Link->{$Node}->GetCurInciState eq 'Incident' ) {
                $attrs{color} = $InciStateColors{Incident};
                last;
            } elsif ( $Link->{$Node}->GetCurInciState eq 'Warning' ) {
                $attrs{color} = $InciStateColors{Warning};
            }
        }
    }

    # Add graphviz edge
    $Self->{GraphVizObject}->add_edge(
	$Link->{Source}->GetKey => $Link->{Target}->GetKey,
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
