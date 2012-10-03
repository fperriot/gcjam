use strict;

my $caseno = 0;

my %leet = (a => '4', b => '8', e => '3', g => '9', i => '1', o => '0',
            s => '5', t => '7');

foreach ('a' .. 'z') {
    $leet{$_} = $_ unless exists $leet{$_};
}

<>;
while (<>) {
    my $k = int $_;

    ++$caseno;

    next unless $k = 2;

    my $s = <>;
    chomp $s;

    my @pairs;

    while ($s =~ /^../) {
        push @pairs, $&;
        $s = substr $s, 1;
    }

    #print "@pairs\n";

    my %dict;

    foreach (@pairs) {
        my ($a, $b) = (substr($_, 0, 1), substr($_, 1, 1));
        $dict{$a . $b}++;
        $dict{$leet{$a} . $b}++;
        $dict{$a . $leet{$b}}++;
        $dict{$leet{$a} . $leet{$b}}++;
    }

    my @uniqs = sort { $a cmp $b } keys %dict;
    my @strs;
    my %inner;

SEED:
    while (scalar @uniqs) {
        my $seed = shift @uniqs;
        my $first = substr $seed, 0, 1;
        my $last = substr $seed, -1, 1;

        print "Seed = $seed\n";

        for (;;) {

            if ($first eq $last && $inner{$first}) {
                for (my $i = 0; $i < scalar @strs; $i++) {
                    my $idx = index $strs[$i], $first;
                    next if $idx < 0;
                    print substr($strs[$i], 0, $idx), ">", $seed, "<",
                          substr($strs[$i], $idx+1), "\n";
                    substr($strs[$i], $idx, 1) = $seed;
                    next SEED;
                }
            }

            my ($l, $r);
            for (my $i = 0; $i < scalar @uniqs; $i++) {
                my $u = $uniqs[$i];
                if ($u =~ /$first$/) {
                    $l = $u;
                    splice @uniqs, $i, 1;
                    last;
                }
                elsif ($u =~ /^$last/) {
                    $r = $u;
                    splice @uniqs, $i, 1;
                    last;
                }
            }
            if (defined $l) {
                $inner{$first}++;
                $first = substr $l, 0, 1;
                $seed = $first . $seed;
                print ">$seed\n";
            }
            elsif (defined $r) {
                $inner{$last}++;
                $last = substr $r, 1, 1;
                $seed .= $last;
                print "$seed<\n";
            }
            else { last };
        }

        push @strs, $seed;
    }

    my $len = 0;
    $len += length $_ for @strs;

    print "Case #$caseno: $len\n";
}

