# --
# Kernel/Output/HTML/ITSMServiceMenuTrace.pm
# Copyright (C) 2011 Thales Austria GmbH, http://www.thalesgroup.com/
# --
# $Id: ITSMServiceMenuTrace.pm $
# --
# This software comes with ABSOLUTELY NO WARRANTY and WITHOUT ANY SUPPORT. 
# For license information, see the enclosed file COPYING-ITSMTrace
# (GNU AFFERO GENERAL PUBLIC LICENSE, version 3). 
# If you did not receive this file, see 
# http://www.gnu.org/licenses/agpl-3.0.html.
# --

package Kernel::Output::HTML::ITSMServiceMenuTrace;

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
        qw(ConfigObject LogObject DBObject LayoutObject ServiceObject LinkObject UserID)
        )
    {
        $Self->{$Object} = $Param{$Object} || die "Got no $Object!";
    }

    return $Self;
}

sub Run {
    my ( $Self, %Param ) = @_;

    # check needed stuff
    if ( !$Param{Service} ) {
        $Self->{LogObject}->Log( Priority => 'error', Message => 'Need Service!' );
        return;
    }

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

    # check if services can be linked with other objects
    my %PossibleObjects = $Self->{LinkObject}->PossibleObjectsList(
        Object => 'Service',
        UserID => $Self->{UserID},
    );

    # don't show trace menu item if there are no linkable objects
    return if !%PossibleObjects;

    $Self->{LayoutObject}->Block(
        Name => 'MenuItem',
        Data => {
            %Param,
            %{ $Param{Service} },
            %{ $Param{Config} },
        },
    );

    $Param{Counter}++;

    return $Param{Counter};
}

1;
