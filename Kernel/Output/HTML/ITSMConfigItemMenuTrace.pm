# --
# Kernel/Output/HTML/ITSMConfigItemMenuTrace.pm
# Copyright (C) 2011 Thales Austria GmbH, http://www.thalesgroup.com/
# --
# $Id: ITSMConfigItemMenuTrace.pm $
# --
# This software comes with ABSOLUTELY NO WARRANTY and WITHOUT ANY SUPPORT. 
# For license information, see the enclosed file COPYING-CMDBExplorer
# (GNU AFFERO GENERAL PUBLIC LICENSE, version 3). 
# If you did not receive this file, see 
# http://www.gnu.org/licenses/agpl-3.0.html.
# --

package Kernel::Output::HTML::ITSMConfigItemMenuTrace;

use strict;
use warnings;

use vars qw($VERSION);
$VERSION = qw($Revision: 0.1 $) [1];

sub new {
    my ( $Type, %Param ) = @_;

    # allocate new hash for object
    my $Self = {};
    bless( $Self, $Type );

    # check needed objects
    for my $Object (
        qw(ConfigObject EncodeObject LogObject DBObject LayoutObject ConfigItemObject UserID)
        )
    {
        $Self->{$Object} = $Param{$Object} || die "Got no $Object!";
    }

    return $Self;
}

sub Run {
    my ( $Self, %Param ) = @_;

    # check needed stuff
    if ( !$Param{ConfigItem} ) {
        $Self->{LogObject}->Log( Priority => 'error', Message => 'Need ConfigItem!' );
        return;
    }

    # grant access by default
    my $Access = 1;

    # get groups
    my $GroupsRo
        = $Self->{ConfigObject}->Get('Frontend::Module')->{ $Param{Config}->{Action} }->{GroupRo}
        || [];
    my $GroupsRw
        = $Self->{ConfigObject}->Get('Frontend::Module')->{ $Param{Config}->{Action} }->{Group}
        || [];

    # set access
    my $Access = 1;

    # check permission
    if ( $Param{Config}->{Action} && ( @{$GroupsRo} || @{$GroupsRw} ) ) {

        # set access
        $Access = 0;

        # find read only groups
        ROGROUP:
        for my $RoGroup ( @{$GroupsRo} ) {

            next ROGROUP if !$Self->{LayoutObject}->{"UserIsGroupRo[$RoGroup]"};
            next ROGROUP if $Self->{LayoutObject}->{"UserIsGroupRo[$RoGroup]"} ne 'Yes';

            # set access
            $Access = 1;
            last ROGROUP;
        }

        # find read write groups
        RWGROUP:
        for my $RwGroup ( @{$GroupsRw} ) {

            next RWGROUP if !$Self->{LayoutObject}->{"UserIsGroup[$RwGroup]"};
            next RWGROUP if $Self->{LayoutObject}->{"UserIsGroup[$RwGroup]"} ne 'Yes';

            # set access
            $Access = 1;
            last RWGROUP;
        }
    }

    return $Param{Counter} if !$Access;

    # output menu block
    $Self->{LayoutObject}->Block( Name => 'Menu' );

    # output menu item
    $Self->{LayoutObject}->Block(
        Name => 'MenuItem',
        Data => {
            %Param,
            %{ $Param{ConfigItem} },
            %{ $Param{Config} },
        },
    );
    $Param{Counter}++;

    return $Param{Counter};
}

1;
