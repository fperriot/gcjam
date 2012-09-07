use strict;

my %memo;
my @base;
my $diamond;

sub super {
    my $i = shift;

    if (exists $memo{$i}) {
        return $memo{$i};
    }

    my @parent = @{$base[$i]};
    my @super = @parent;
    my %count = ();
    $count{$_}++ for @parent;

    foreach (@parent) {
        foreach (@{super($_)}) {
            die ($diamond = 1) if ++$count{$_} > 1;
            push @super, $_;
        }
    }

    #print "super($i) = @super\n";

    return ($memo{$i} = \@super);
}

my $caseno = 1;

<>;
while (<>) {
    print "Case #$caseno: ";
    ++$caseno;

    my $n = int $_;

    @base = ();

    foreach my $i (1..$n) {
        $_ = <>;
        my @b = split;
        shift @b;
        @{$base[$i]} = @b;
    }

    %memo = ();
    $diamond = 0;

    foreach (1..$n) {
        last unless eval { super($_) };
    }

    print $diamond ? "Yes\n" : "No\n";
}

