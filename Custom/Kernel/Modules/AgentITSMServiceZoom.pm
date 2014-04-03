# --
# Kernel/Modules/AgentITSMServiceZoom.pm - the OTRS ITSM Service zoom module
# Copyright (C) 2001-2013 OTRS AG, http://otrs.org/
# --
# $Id: AgentITSMServiceZoom.pm,v 1.10 2013/03/26 14:35:41 ub Exp $
# --
# This software comes with ABSOLUTELY NO WARRANTY. For details, see
# the enclosed file COPYING for license information (AGPL). If you
# did not receive this file, see http://www.gnu.org/licenses/agpl.txt.
# --

package Kernel::Modules::AgentITSMServiceZoom;

use strict;
use warnings;

use Kernel::System::LinkObject;
use Kernel::System::Service;
use Kernel::System::SLA;

###### OTRSCMDBExplorer ######
use Kernel::System::CMDBExplorer;
use MIME::Base64;
###### OTRSCMDBExplorer ######

use vars qw($VERSION);
$VERSION = qw($Revision: 1.10 $) [1];

sub new {
    my ( $Type, %Param ) = @_;

    # allocate new hash for object
    my $Self = {%Param};
    bless( $Self, $Type );

    # check needed objects
    for my $Object (qw(ConfigObject ParamObject DBObject LayoutObject LogObject)) {
        if ( !$Self->{$Object} ) {
            $Self->{LayoutObject}->FatalError( Message => "Got no $Object!" );
        }
    }
    $Self->{LinkObject}    = Kernel::System::LinkObject->new(%Param);
    $Self->{ServiceObject} = Kernel::System::Service->new(%Param);
    $Self->{SLAObject}     = Kernel::System::SLA->new(%Param);

    return $Self;
}

sub Run {
    my ( $Self, %Param ) = @_;

    # get params
    my $ServiceID = $Self->{ParamObject}->GetParam( Param => 'ServiceID' );

    # check needed stuff
    if ( !$ServiceID ) {
        return $Self->{LayoutObject}->ErrorScreen(
            Message => 'No ServiceID is given!',
            Comment => 'Please contact the admin.',
        );
    }

    # get service
    my %Service = $Self->{ServiceObject}->ServiceGet(
        ServiceID     => $ServiceID,
        IncidentState => 1,
        UserID        => $Self->{UserID},
    );
    if ( !$Service{ServiceID} ) {
        return $Self->{LayoutObject}->ErrorScreen(
            Message => "ServiceID $ServiceID not found in database!",
            Comment => 'Please contact the admin.',
        );
    }

    # run config item menu modules
    if ( ref $Self->{ConfigObject}->Get('ITSMService::Frontend::MenuModule') eq 'HASH' ) {
        my %Menus   = %{ $Self->{ConfigObject}->Get('ITSMService::Frontend::MenuModule') };
        my $Counter = 0;
        for my $Menu ( sort keys %Menus ) {

            # load module
            if ( $Self->{MainObject}->Require( $Menus{$Menu}->{Module} ) ) {
                my $Object = $Menus{$Menu}->{Module}->new(
                    %{$Self},
                    ServiceID => $Self->{ServiceID},
                );

                # set classes
                if ( $Menus{$Menu}->{Target} ) {
                    if ( $Menus{$Menu}->{Target} eq 'PopUp' ) {
                        $Menus{$Menu}->{MenuClass} = 'AsPopup';
                    }
                    elsif ( $Menus{$Menu}->{Target} eq 'Back' ) {
                        $Menus{$Menu}->{MenuClass} = 'HistoryBack';
                    }
                }

                # run module
                $Counter = $Object->Run(
                    %Param,
                    Service => \%Service,
                    Counter => $Counter,
                    Config  => $Menus{$Menu},
                );
            }
            else {
                return $Self->{LayoutObject}->FatalError();
            }
        }
    }

    # get sla list
    my %SLAList = $Self->{SLAObject}->SLAList(
        ServiceID => $ServiceID,
        UserID    => $Self->{UserID},
    );
    if (%SLAList) {

        # output row
        $Self->{LayoutObject}->Block(
            Name => 'SLA',
        );

        for my $SLAID ( sort { $SLAList{$a} cmp $SLAList{$b} } keys %SLAList ) {

            # get sla data
            my %SLA = $Self->{SLAObject}->SLAGet(
                SLAID  => $SLAID,
                UserID => $Self->{UserID},
            );

            # output row
            $Self->{LayoutObject}->Block(
                Name => 'SLARow',
                Data => {
                    %SLA,
                },
            );
        }
    }

    # get linked objects
    my $LinkListWithData = $Self->{LinkObject}->LinkListWithData(
        Object => 'Service',
        Key    => $ServiceID,
        State  => 'Valid',
        UserID => $Self->{UserID},
    );

    # get link table view mode
    my $LinkTableViewMode = $Self->{ConfigObject}->Get('LinkObject::ViewMode');

    # create the link table
    my $LinkTableStrg = $Self->{LayoutObject}->LinkObjectTableCreate(
        LinkListWithData => $LinkListWithData,
        ViewMode         => $LinkTableViewMode,
    );

    # output the link table
    if ($LinkTableStrg) {
        $Self->{LayoutObject}->Block(
            Name => 'LinkTable' . $LinkTableViewMode,
            Data => {
                LinkTableStrg => $LinkTableStrg,
            },
        );
    }

    # set incident signal
    my %InciSignals = (
        operational => 'greenled',
        warning     => 'yellowled',
        incident    => 'redled',
    );

    # get create user data
    my %CreateUser = $Self->{UserObject}->GetUserData(
        UserID => $Service{CreateBy},
        Cached => 1,
    );
    for my $Postfix (qw(UserLogin UserFirstname UserLastname)) {
        $Service{ 'Create' . $Postfix } = $CreateUser{$Postfix};
    }

    # get change user data
    my %ChangeUser = $Self->{UserObject}->GetUserData(
        UserID => $Service{ChangeBy},
        Cached => 1,
    );
    for my $Postfix (qw(UserLogin UserFirstname UserLastname)) {
        $Service{ 'Change' . $Postfix } = $ChangeUser{$Postfix};
    }

###### OTRSCMDBExplorer ######
    my $Tracer = Kernel::System::CMDBExplorer->new(%{$Self}, Debug => 1);

    # Get graph parameters from URI
    my %TraceParams;
#    my @DisplayedCIs;
#    if ( $Self->{ParamObject}->GetParam( Param => 'DisplayedCIs' ) ) {
#        @DisplayedCIs = split( ',' ,$Self->{ParamObject}->GetParam( Param => 'DisplayedCIs' ) );
#    } else {
#        @DisplayedCIs = @RootCI;
#    }
#    $TraceParams{DisplayedCIs} =  \@DisplayedCIs;
    $TraceParams{ServiceID} = $ServiceID;
    $TraceParams{Layout} = $Self->{ParamObject}->GetParam( Param => 'Layout' ) || 'dot';
    $TraceParams{IA} = $Self->{ParamObject}->GetParam( Param => 'IA' ) || 0;
    $TraceParams{Depth} = $Self->{ParamObject}->GetParam( Param => 'Depth' ) || 1;

    # Default trace constraints (Show all links up to specified depth)
    my $LinkTypes = '';
    my $MaxTraceDepth = $TraceParams{Depth};

    # Does the agent requested Impact analysis?
    if ( $TraceParams{IA} == 1 ) {
        $LinkTypes = 'DependsOn';
        $MaxTraceDepth = 0;
    }
    # Set Trace constraints
    $Tracer->SetConstraints(
        MaxTraceDepth => $MaxTraceDepth,
        LinkTypes => $LinkTypes,
    );

    # Display Impact analysis menu entry
    if ( $Service{CurInciStateType} ne 'operational' ) {
        if ( ! $TraceParams{IA} ) {
            $Self->{LayoutObject}->Block(
                 Name => 'GraphMenuItem',
                 Data => {
                     Link        => 'Action=AgentITSMServiceZoom;ServiceID=' . $ServiceID . ';Layout=' . $TraceParams{Layout} . ';IA=1',
                     MenuClass   => 'NoPopUp',
                     Name        => 'Impact',
                     Description => 'Show all impacted CIs',
                 },
            );
        } else {
            $Self->{LayoutObject}->Block(
                 Name => 'GraphMenuItem',
                 Data => {
                     Link        => 'Action=AgentITSMServiceZoom;ServiceID=' . $ServiceID . ';Layout=' . $TraceParams{Layout} . ';IA=0',
                     MenuClass   => 'NoPopUp',
                     Name        => 'Regular',
                     Description => 'Regular view',
                 },
            );
        }
    }
    # Display digging menu entries
    $TraceParams{Depth}++;
    $Self->{LayoutObject}->Block(
         Name => 'GraphMenuItem',
         Data => {
             Link        => 'Action=AgentITSMServiceZoom;ServiceID=' . $ServiceID . ';Layout=' . $TraceParams{Layout} . ';IA=' . $TraceParams{IA} . ';Depth=' . $TraceParams{Depth},
             MenuClass   => 'NoPopUp',
             Name        => '+',
             Description => 'Drill down in CMDB',
         },
     );
    $TraceParams{Depth} -= 2;
    $TraceParams{Depth} = 1 unless $TraceParams{Depth};
    $Self->{LayoutObject}->Block(
         Name => 'GraphMenuItem',
         Data => {
             Link        => 'Action=AgentITSMServiceZoom;ServiceID=' . $ServiceID . ';Layout=' . $TraceParams{Layout} . ';IA=' . $TraceParams{IA} . ';Depth=' . $TraceParams{Depth},
             MenuClass   => 'NoPopUp',
             Name        => '-',
             Description => 'Drill up in CMDB',
         },
     );

    # Display graph layout selection field
    my $LayoutStrg = $Self->{LayoutObject}->BuildSelection(
         Name => 'Layout',
         Data => {
             dot   => 'dot',
             neato => 'neato',
             twopi => 'twopi',
             circo => 'circo',
             fdp   => 'fdp',
             sfdp  => 'sfdp',
         },
         SelectedValue => $TraceParams{Layout},
    );
    $Self->{LayoutObject}->Block(
         Name => 'GraphLayout',
         Data => {
             ServiceID 	  => $ServiceID,
             DisplayedCIs => $Self->{ParamObject}->GetParam( Param => 'DisplayedCIs' ) || '',
             IA           => $TraceParams{IA},
             Depth        => $MaxTraceDepth,
             LayoutStrg   => $LayoutStrg,
         },
    );

    # Create the graph
    my $Content = $Tracer->Trace( %TraceParams );

    # Add graph to output
    my $Base64EncodedPNG = encode_base64( $Content->{png} );
    $Self->{LayoutObject}->Block(
         Name => 'LinkGraph',
         Data => {
             Content => $Base64EncodedPNG,
             Map     => $Content->{map},
         },
    );
###### OTRSCMDBExplorer ######

    # store last screen
    $Self->{SessionObject}->UpdateSessionID(
        SessionID => $Self->{SessionID},
        Key       => 'LastScreenView',
        Value     => $Self->{RequestedURL},
    );

    # output header
    my $Output = $Self->{LayoutObject}->Header();
    $Output .= $Self->{LayoutObject}->NavigationBar();

    # generate output
    $Output .= $Self->{LayoutObject}->Output(
        TemplateFile => 'AgentITSMServiceZoom',
        Data         => {
            %Param,
            %Service,
            CurInciSignal => $InciSignals{ $Service{CurInciStateType} },
        },
    );
    $Output .= $Self->{LayoutObject}->Footer();

    return $Output;
}

1;
