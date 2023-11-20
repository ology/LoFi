package PitchConvert;
use Moo;
with('Music::PitchNum');

package main;
use strict;
use warnings;

use Data::Dumper::Compact qw(ddc);
use List::Util qw(none);
use Music::Chord::Note ();
use Music::Scales qw(get_scale_notes);
use Music::VoiceGen ();

use lib map { "$ENV{HOME}/sandbox/$_/lib" } qw(Data-Dataset-ChordProgressions Music-Bassline-Generator MIDI-Util MIDI-Drummer-Tiny Music-Duration Music-Duration-Partition Music-ToRoman);
use Data::Dataset::ChordProgressions ();
use Music::Bassline::Generator ();
use MIDI::Drummer::Tiny ();
use MIDI::Util qw(set_chan_patch midi_format);
use Music::Duration::Partition ();
use Music::ToRoman ();

use constant MIN_CHORD_SIZE => 3;

my $complexity = shift || random_item([2 .. 4]); # 1: least to 4: most
my $key        = shift || 'C';
my $scale_name = shift || 'ionian';
my $octave     = shift || 4;
my $bpm        = shift || random_item([65 .. 75]);
my $parts      = shift || random_parts(key => $key, scale => $scale_name, parts => 4); #'Amv-DMc-Emv-DMc'; # <Note><Major|minor><verse|chorus>-...
my $sections   = shift || 3;

my $chords_patch = shift || 4;
my $bass_patch   = shift || 35;

printf "Complexity: %d, %d BPM, Parts: %s, Chords: %d, Bass: %d\n\n",
    $complexity, $bpm, $parts, $chords_patch, $bass_patch;

my @parts = split /-/, $parts;

my @progressions; # nb: populated by chords() used by bass()

my $motifs = motifs(6, MIN_CHORD_SIZE);

my $d = MIDI::Drummer::Tiny->new(
    file   => "$0.mid",
    bpm    => $bpm,
    bars   => 4 * @parts,
    reverb => 10,
    kick   => 36,
);

my $counter = 0; # global progression increment
for my $section (1 .. $sections) {
    if ($section > 1) {
        $counter = 0;
        $d->sync(
            \&chords2,
            \&bass2,
        );
    }
    $counter = 0;
    $d->sync(
        \&drums,
        \&chords,
        \&bass,
    );
}

$d->write;

sub random_item {
    my ($items) = @_;
    return $items->[ int rand @$items ];
}

sub random_parts {
    my (%args) = @_;
    my $mtr = Music::ToRoman->new(
        scale_note => $args{key},
        scale_name => $args{scale},
    );
    my %map = ( # allowed notes
        ionian  => [0, 2, 3, 4, 5],
        aeolean => [0, 2, 3, 4, 6],
    );
    my @chords = $mtr->get_scale_chords;
    my @scale = get_scale_notes($args{key}, $args{scale});
    my @parts;
    for my $n (1 .. $args{parts}) {
        my $parts_string = '';
        my $index = $map{ $args{scale} }->[ int rand $map{ $args{scale} }->@* ];
        my $pitch = $scale[$index];
        $parts_string .= $pitch;
        $parts_string .= $chords[$index] =~ /m/ ? 'm' : 'M';
        $parts_string .= int rand 2 ? 'v' : 'c';
        push @parts, $parts_string;
    }
    return join '-', @parts;
}

sub motifs {
    my ($mnum, $nnum) = @_;
    my $mdp = Music::Duration::Partition->new(
        size    => 4,
        pool    => [qw/ dhn hn qn en /],
        weights => [    2,  3, 1, 1   ],
    );
    my %seen;
    my @motifs;
    for my $i (1 .. $mnum) {
        my $motif = $mdp->motif;
        redo if @$motif <= 1 || @$motif > $nnum;
        redo if $seen{ join '-', @$motif }++;
        push @motifs, $motif;
    }
    $mdp = Music::Duration::Partition->new(
        size    => 4,
        pool    => [qw/ hn qn tqn /],
        weights => [    1, 2, 2    ],
        groups  => [    1, 1, 3    ],
    );
    for my $i (1 .. $mnum) {
        my $motif = $mdp->motif;
        redo if none { $_ =~ /t/ } @$motif;
        redo if $seen{ join '-', @$motif }++;
        push @motifs, $motif;
    }
    return \@motifs;
}

sub phrase {
        my ($d, $n, $motifs) = @_;
        my $motif = $motifs->[ int rand @$motifs ];
        my %dispatch = (
            voicegen => sub {
                my $conv = PitchConvert->new;
                my @pitches = map { $conv->pitchnum($_) } @$n;
                my $voice = Music::VoiceGen->new(
                    pitches   => \@pitches,
                    intervals => [qw/-3 -2 -1 0 1 2 3/],
                    weightfn  => sub {
                        my ($from, $to, $interval) = @_;
                        $interval == 0 ? 1 : 4;
                    },
                );
                $d->note($motif->[$_], $voice->rand) for 0 .. $#$motif;
            },
            random => sub {
                $d->note($motif->[$_], $n->[ int rand @$n ]) for 0 .. $#$motif;
            },
        );
        my @keys = keys %dispatch;
        my $routine = $dispatch{ $keys[ int rand @keys ] };
        $routine->();
}

sub drums {
    set_chan_patch($d->score, 9, 0);

    my $i = 0;
    for my $n (1 .. $d->bars) {
        for my $m (1 .. $d->beats) {
            $i++;
            $d->note($d->dotted_eighth, $d->closed_hh, $m == 1 || $m == 3 ? $d->kick : '', $m == 2 && $i == 2 ? $d->snare : '');
            if ($m == 4) {
                $d->note($d->sixteenth, $d->kick);
            }
            else {
                $d->rest($d->sixteenth);
            }
        }
        $i = 0 if $i == $d->bars / 2;
    }
}

sub chords {
    set_chan_patch($d->score, 0, $chords_patch);

    my $cn = Music::Chord::Note->new;

    my %data = Data::Dataset::ChordProgressions::as_hash();

    my @accum; # Note accumulator

    @progressions = (); # reset the progression list

    for my $part (@parts) {
        $counter++;
        my ($note, $section, $scale, $pool);
        # Set the pool of possible progressions given scale and section
        if ($part =~ /^([A-G][#b]?)(M|m)(v|c)$/) {
            ($note, $scale, $section) = ($1, $2, $3);
            $scale   = $scale eq 'M' ? 'major' : 'minor';
            $section = $section eq 'v' ? 'verse' : 'chorus';
            $pool    = $data{rock}{$scale}{$section};
        }

        # Set the transposition map
        my %note_map;
        @note_map{ get_scale_notes('C', $scale) } = get_scale_notes($note, $scale);

        # Get a random progression
        my $progression = $pool->[int rand @$pool];

        # Transpose the progression chords from C
        (my $named = $progression->[0]) =~ s/([A-G][#b]?)/$note_map{$1}/g;

        # Keep track of the progressions used
        push @progressions, $named;

        if ($complexity == 1) {
            $named = $progressions[0];
        }
        elsif ($complexity == 2 && ($counter == 1 || $counter == 3)) {
            $named = $progressions[0];
        }
        elsif ($complexity == 2 && $counter == 4) {
            $named = $progressions[1];
        }
        elsif ($complexity == 3 && $counter == 3) {
            $named = $progressions[2];
        }
        elsif ($complexity == 3 && $counter == 4) {
            $named = $progressions[1];
        }
        if ($complexity < 4) {
            $progressions[ $counter - 1 ] = $named;
        }

        print "$named\n";

        my @chords = split /-/, $named;

        # Add each chord to the score
            for my $chord (@chords) {
                $chord =~ s/^(.+)\//$1/ if $chord =~ /\//;
                $chord =~ s/sus2/add9/;
                $chord =~ s/6sus4/sus4/;
                my @notes = $cn->chord_with_octave($chord, $octave);
                @notes = midi_format(@notes);
                push @accum, \@notes;
            }
    }

    my $k = 0;
#    print "\t", join ', ', map { "[@$_]" } @accum;
    for my $n (@accum) {
        $k++;
        if ($k % 2 == 0 || $k % 4 == 0) {
            phrase($d, $n, $motifs);
        }
        else {
            $d->note($d->whole, @$n);
        }
    }
    print "\n";
}

sub bass {
    set_chan_patch($d->score, 1, $bass_patch);

    my $mdp = Music::Duration::Partition->new(
        size    => 4,
        pool    => [qw/ dhn hn qn /],
        weights => [    4,  4, 1   ],
    );
    my @motifs = map { $mdp->motif } 1 .. $complexity;
    unshift @motifs, [ 'wn' ];

    my $bassline = Music::Bassline::Generator->new(
        octave    => 2,
        guitar    => 1,
        verbose   => 0,
#        scale     => sub { $_[0] =~ /^[A-G][#b]?m/ ? 'pminor' : 'pentatonic' },
        tonic     => 1,
        positions => { major => [0,3,4], minor => [0,3,4] },
        wrap      => 'C3',
#        chord_notes => 0,
    );

    for my $p (@progressions) {
        my @chords = split /-/, $p;

        my $i = 0;

        for my $n (0 .. $#chords) {
            my $chord = $chords[$n];
            $chord =~ s/sus2/add9/;
            $chord =~ s/6sus4/sus4/;
            my $next = $chords[ $n + 1 ];
            if ($next) {
                $chord =~ s/sus2/add9/;
                $chord =~ s/6sus4/sus4/;
            }

            my $m = $motifs[ int rand @motifs ];

            my $notes = $bassline->generate($chord, scalar(@$m), $next);

            $mdp->add_to_score($d->score, $m, $notes);

            $i++;
        }
    }
}

sub chords2 {
    set_chan_patch($d->score, 0, $chords_patch);

    my $cn = Music::Chord::Note->new;

    my %data = Data::Dataset::ChordProgressions::as_hash();

    my @accum; # Note accumulator
    my $i = 0;

    @progressions = (); # reset the progression list

    for my $part (@parts) {
        $i++;

        my ($note, $section, $scale, $pool);
        # Set the pool of possible progressions given scale and section
        if ($part =~ /^([A-G][#b]?)(M|m)(v|c)$/) {
            ($note, $scale, $section) = ($1, $2, $3);
            $scale   = $scale eq 'M' ? 'major' : 'minor';
            $section = $section eq 'v' ? 'verse' : 'chorus';
            $pool    = $data{rock}{$scale}{$section};
        }

        # Set the transposition map
        my %note_map;
        @note_map{ get_scale_notes('C', $scale) } = get_scale_notes($note, $scale);

        # Get a random progression
        my $progression = $pool->[int rand @$pool];

        # Transpose the progression chords from C
        (my $named = $progression->[0]) =~ s/([A-G][#b]?)/$note_map{$1}/g;

        # Keep track of the progressions used
        push @progressions, $named;

        print "$named\n" if $i <= 2;

        my @chords = split /-/, $named;

        # Add each chord to the score
        for my $chord (@chords) {
            $chord =~ s/^(.+)\//$1/ if $chord =~ /\//;
            $chord =~ s/sus2/add9/;
            $chord =~ s/6sus4/sus4/;
            my @notes = $cn->chord_with_octave($chord, $octave);
            @notes = midi_format(@notes);
            push @accum, \@notes;
        }
    }

    my $k = 0;
#    print "\t", join ', ', map { "[@$_]" } @accum;
    for my $n (@accum[0 .. $#accum / 2]) {
        $k++;
        if ($k % 4 == 0) {
            phrase($d, $n, $motifs);
        }
        else {
            $d->note($d->whole, @$n);
        }
    }
    print "\n";
}

sub bass2 {
    set_chan_patch($d->score, 1, $bass_patch);

    my $mdp = Music::Duration::Partition->new(
        size    => 4,
        pool    => [qw/ dhn hn qn en /],
        weights => [    3,  3, 2, 1   ],
    );
    my $motif1 = $mdp->motif;
    my $motif2 = $mdp->motif;

    my $bassline = Music::Bassline::Generator->new(
        octave    => 2,
        guitar    => 1,
        verbose   => 0,
        tonic     => 1,
        positions => { major => [0,2,3,4,5], minor => [0,2,3,4,5,6] },
        wrap      => 'C3',
    );

    for my $p (@progressions[ 0 .. $#progressions / 2 ]) {
        my @chords = split /-/, $p;

        my $i = 0;

        for my $n (0 .. $#chords) {
            my $chord = $chords[$n];
            $chord =~ s/sus2/add9/;
            $chord =~ s/6sus4/sus4/;
            my $next = $chords[ $n + 1 ];
            if ($next) {
                $chord =~ s/sus2/add9/;
                $chord =~ s/6sus4/sus4/;
            }

            my $m = $i % 2 == 0 ? $motif2 : $motif1;

            my $notes = $bassline->generate($chord, scalar(@$m), $next);

            $mdp->add_to_score($d->score, $m, $notes);

            $i++;
        }
    }
}
