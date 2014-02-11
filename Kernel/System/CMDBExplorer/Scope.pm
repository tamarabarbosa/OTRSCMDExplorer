# --
# Kernel/System/ITSMTrace/Scope.pm
# Copyright (C) 2011-2014 Thales Austria GmbH, http://www.thalesgroup.com/
# --
# $Id: Scope.pm $
# --
# This software comes with ABSOLUTELY NO WARRANTY and WITHOUT ANY SUPPORT. 
# For license information, see the enclosed file COPYING-ITSMTrace
# (GNU AFFERO GENERAL PUBLIC LICENSE, version 3). 
# If you did not receive this file, see 
# http://www.gnu.org/licenses/agpl-3.0.html.
# --

package Kernel::System::ITSMTrace::Scope;

=head1 NAME

Kernel::System::ITSMTrace::Scope - object to track for which services an ITSMConfigItem is used

=head1 SYNOPSIS

Scopes are used for clustering ITSM config items in a graphical trace. 
A scope object holds IDs of services from which a CI has an 
I<explicit> directed in-link and service IDs that are I<inherited> 
from other CIs through directed in-links. 

=cut

use strict;
use warnings;

use vars qw($VERSION);
$VERSION = '0.8';

=head1 PUBLIC INTERFACE

=over 4

=cut



########################################################################
#

=item new()

    $ScopeObject = Kernel::System::ITSMTrace::Scope->new();

=cut

sub new
{
    my ($Class, %Param) = @_;

    my $Self = { };
    bless ($Self, $Class);

    $Self->{ExplicitScopeIDs}  = { }; 
    $Self->{InheritedScopeIDs} = { };
    $Self->{Signature} = undef;

    return $Self;
} # new()
#
########################################################################


########################################################################
#

=item AddExplicitScopeID()

Adds an ID to the "explicit" part of the scope unless it is already there. 
If the ID is in the list of "inherited" IDs it is removed from there and
made explicit.

    $ScopeObject->AddExplicitScopeID ( ID => nnn );

Returns 1 if ID was added/moved, 0 if unchanged, undef on error.

=cut

sub AddExplicitScopeID {
    my ( $Self, %Param ) = @_;
    my $ID = $Param{ID};
    return undef unless $ID;
    return 0 if exists $Self->{ExplicitScopeIDs}->{$ID}; # already there
    delete $Self->{InheritedScopeIDs}->{$ID}
   			 if exists $Self->{InheritedScopeIDs}->{$ID};
    $Self->{ExplicitScopeIDs}->{$ID}++;
    undef $Self->{Signature};	# clear cache
    return 1;
} # AddExplicitScopeID()



=item InheritScope()

Merges all scope IDs into the "inherited" part of the current scope object,
unless they are already present as explicit or inherited ID.

   $ScopeObject->InheritScope ( $Scope => ScopeObject2 );

Returns number of IDs added, undef on error.

=cut

sub InheritScope {
    my ( $Self, %Param ) = @_;
    return undef unless ($Param{Scope} && ref $Param{Scope});
    my $Added = 0;
    for my $ID ( $Param{Scope}->GetAllScopeIDs ) {
	next if $Self->{ExplicitScopeIDs}->{$ID};
	next if $Self->{InheritedScopeIDs}->{$ID};
	$Self->{InheritedScopeIDs}->{$ID}++;
	$Added++;
    } #for
    undef $Self->{Signature} if $Added;		# clear cache
    return $Added;
} # InheritScope()
#
########################################################################


########################################################################
#

=item GetExplicitScopeIDs(), GetInheritedScopeIDs(), GetAllScopeIDs()

In array context, returns unsorted list of explicit/inherited/all
scope IDs, otherwise just the respective number of IDs.

=cut

sub GetExplicitScopeIDs { 
    my @IDs = keys %{$_[0]->{ExplicitScopeIDs}};
    return wantarray ? @IDs : scalar(@IDs);
}



sub GetInheritedScopeIDs {
    my @IDs = keys %{$_[0]->{InheritedScopeIDs}};
    return wantarray ? @IDs : scalar(@IDs);
}



sub GetAllScopeIDs {
    my $Self = shift;
    my @IDs= ( keys %{$Self->{ExplicitScopeIDs}}, 
	       keys %{$Self->{InheritedScopeIDs}} );
    return wantarray ? @IDs : scalar(@IDs);
}
#
########################################################################


########################################################################
#

=item GetAllScopeIDsList()

Returns a string based on the sorted concatenation of all scope
IDs, hiding differences between explicit and inherited IDs.
Useful to identify scopes for clustering.

=cut

sub GetAllScopeIDsList {
    my $Self = shift;
    my $Signature = $Self->{Signature};	# load cache
    return $Signature if defined $Signature;
    $Signature = join( '-', sort $Self->GetAllScopeIDs );
    $Self->{Signature} = $Signature;	# (return value)
} # GetAllScopeIDsList()



=item ToString()

Returns a string that represents the contents of the scope object.

=cut

sub ToString {
    my $Self = shift;
    my $S1 = join( ',', sort $Self->GetExplicitScopeIDs  ) || '-';
    my $S2 = join( ',', sort $Self->GetInheritedScopeIDs ) || '-';
    return "$S1;$S2";
} # ToString()
#
#########################################################################
1;

=back

=head1 TERMS AND CONDITIONS

Copyright (C) 2011-2014 Thales Austria GmbH, http://www.thalesgroup.com/

This software comes with ABSOLUTELY NO WARRANTY and WITHOUT ANY SUPPORT. 

For license information, see the enclosed file COPYING-ITSMTrace
(GNU AFFERO GENERAL PUBLIC LICENSE, version 3). 
If you did not receive this file, see 
http://www.gnu.org/licenses/agpl-3.0.html.


=head1 AUTHOR

dietmar.berg@thalesgroup.com

=cut
