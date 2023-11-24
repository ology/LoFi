package PitchConvert;
use Moo;
with('Music::PitchNum');

package main;
use strict;
use warnings;

use Getopt::Long qw(GetOptions);
use Data::Dumper::Compact qw(ddc);
use List::Util qw(any none);
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

my %opts = (
    complexity   => random_item([2 .. 4]), # 1: least to 4: most
    key          => 'C',
    scale_name   => 'ionian',
    octave       => 4,
    chords_patch => 4,
    bass_patch   => 35,
    melody_patch => 4,
    bpm          => random_item([65 .. 75]),
    parts        => undef, # Ex: 'Amv-DMc-Emv-DMc' - <Note><Major|minor><verse|chorus>-...
    sections     => 3,
    zones        => 4, # section parts
);
GetOptions( \%opts,
    'complexity=i',
    'key=s',
    'scale_name=s',
    'octave=i',
    'chords_patch=i',
    'bass_patch=i',
    'melody_patch=i',
    'bpm=i',
    'parts=s',
    'sections=i',
    'zones=i',
) or die "Can't GetOptions";

$opts{parts} ||= random_parts(
    key   => $opts{key},
    scale => $opts{scale_name},
    parts => $opts{zones},
);
my @parts = split /-/, $opts{parts};

printf "Complexity: %d, %d BPM, Parts: %s, Chords: %d, Bass: %d\n\n",
    $opts{complexity}, $opts{bpm}, $opts{parts}, $opts{chords_patch}, $opts{bass_patch};

my @progressions; # nb: populated by chords() used by bass()

my $d = MIDI::Drummer::Tiny->new(
    file   => "$0.mid",
    bpm    => $opts{bpm},
    bars   => 4 * @parts,
    reverb => 10,
    kick   => 36,
);

my $counter = 0; # global progression increment
for my $section (1 .. $opts{sections}) {
    if ($section > 1) {
        $counter = 0;
        $d->sync(
            \&chords2,
            \&bass2,
            \&melody2,
        );
    }
    $counter = 0;
    $d->sync(
        \&drums,
        \&chords,
        \&bass,
        \&melody,
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
    my ($num, $size, $pool, $weights, $groups) = @_;
    die 'No pool given' unless $pool;
    $weights ||= [ (1) x @$pool ];
    $groups  ||= [ (1) x @$pool ];
    my $mdp = Music::Duration::Partition->new(
        size    => $size,
        pool    => $pool,
        weights => $weights,
        groups  => $groups,
    );
    my %seen;
    my @motifs;
    my $triplets = any { $_ =~ /t/ } @$pool ? 1 : 0;
    for my $i (1 .. $num) {
        my $motif = $mdp->motif;
        redo if $triplets && none { $_ =~ /t/ } @$motif;
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
#            @pitches = (@pitches, map { $_ + 12 } @pitches);
            my $voice = Music::VoiceGen->new(
                pitches   => \@pitches,
                intervals => [qw/-4 -3 -2 -1 1 2 3 4/],
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
            $d->note(
                $d->dotted_eighth,
                $d->closed_hh,
                $m == 1 || $m == 3 ? $d->kick : '',
                $m == 2 && $i == 2 ? $d->snare : ''
            );
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
    set_chan_patch($d->score, 0, $opts{chords_patch});

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

        if ($opts{complexity} == 1) {
            $named = $progressions[0];
        }
        elsif ($opts{complexity} == 2 && ($counter == 1 || $counter == 3)) {
            $named = $progressions[0];
        }
        elsif ($opts{complexity} == 2 && $counter == 4) {
            $named = $progressions[1];
        }
        elsif ($opts{complexity} == 3 && $counter == 3) {
            $named = $progressions[2];
        }
        elsif ($opts{complexity} == 3 && $counter == 4) {
            $named = $progressions[1];
        }
        if ($opts{complexity} < 4) {
            $progressions[ $counter - 1 ] = $named;
        }

        print "Chords: $named\n";

        my @chords = split /-/, $named;

        add_chord($cn, \@chords, \@accum);
    }

#    print "\t", join ', ', map { "[@$_]" } @accum;
    for my $n (@accum) {
        $d->note($d->whole, @$n);
    }
    print "\n";
}

sub bass {
    set_chan_patch($d->score, 1, $opts{bass_patch});

    my $motifs = motifs(
        $opts{complexity},
        4,
        [qw/ wn dhn hn qn /],
        [    2, 4,  4, 1   ],
    );

    my $bassline = Music::Bassline::Generator->new(
        verbose   => 0,
        octave    => 2,
        guitar    => 1,
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

            my $m = $motifs->[ int rand @$motifs ];

            my $notes = $bassline->generate($chord, scalar(@$m), $next);

            for my $i (0 .. $#$m) {
                $d->note($m->[$i], $notes->[$i]);
            }

            $i++;
        }
    }
}

sub chords2 {
    set_chan_patch($d->score, 0, $opts{chords_patch});

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

        print "Chords2: $named\n" if $i <= 2;

        my @chords = split /-/, $named;

        add_chord($cn, \@chords, \@accum);
    }

#    print "\t", join ', ', map { "[@$_]" } @accum;
    for my $n (@accum[0 .. $#accum / 2]) {
        $d->note($d->whole, @$n);
    }
    print "\n";
}

# Add each chord to the score
sub add_chord {
    my ($cn, $chords, $accum) = @_;
    for my $chord (@$chords) {
        $chord =~ s/^(.+)\/.+$/$1/ if $chord =~ /\//;
        $chord =~ s/sus2/add9/;
        $chord =~ s/6sus4/sus4/;
        my @notes = $cn->chord_with_octave($chord, $opts{octave});
        @notes = midi_format(@notes);
        push @$accum, \@notes;
    }
}

sub bass2 {
    set_chan_patch($d->score, 1, $opts{bass_patch});

    my $motifs = motifs(
        2,
        4,
        [qw/ dhn hn qn en /],
        [    3,  3, 2, 1   ],
    );

    my $bassline = Music::Bassline::Generator->new(
        verbose   => 0,
        octave    => 2,
        guitar    => 1,
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

            my $m = $i % 2 == 0 ? $motifs->[1] : $motifs->[0];

            my $notes = $bassline->generate($chord, scalar(@$m), $next);

            for my $i (0 .. $#$m) {
                $d->note($m->[$i], $notes->[$i]);
            }

            $i++;
        }
    }
}

sub melody {
    set_chan_patch($d->score, 2, $opts{melody_patch});

    my $straight_motifs = motifs(
        4,
        4,
        [qw/ hn dqn qn en /],
        [    3, 2,  2, 2   ],
    );
    my $triplet_motifs = motifs(
        4,                 # number of motifs
        4,                 # number of quarter-notes
        [qw/ hn qn tqn /], # pool
        [    1, 2, 2    ], # weights
        [    1, 1, 3    ], # groups
    );
    my @motifs = (@$straight_motifs, @$triplet_motifs);

    my $cn = Music::Chord::Note->new;

    my %data = Data::Dataset::ChordProgressions::as_hash();

    my @accum; # Note accumulator

    for my $progression (@progressions) {
#        print "Melody: $progression\n";
        my @chords = split /-/, $progression;
        add_chord($cn, \@chords, \@accum);
    }

    my $k = 0;
#    print "\t", join ', ', map { "[@$_]" } @accum;
    for my $n (@accum) {
        $k++;
        if ($k % 2 == 0 || $k % 4 == 0) {
            phrase($d, $n, \@motifs);
        }
        else {
            my ($dura, $notes);
#            if (int rand 2) {
                $dura = $d->whole;
                $notes = 1;
#            }
#            else {
#                $dura = $d->half;
#                $notes = 2;
#            }
            $d->note($dura, $n->[ int rand @$n ]) for 1 .. $notes;
        }
    }
#    print "\n";
}

sub melody2 {
}
