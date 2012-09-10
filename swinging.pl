use strict;
use List::Util qw(min max);

my $caseno = 1;

<>;
while (<>) {
    my $n = int $_;
    my (@d, @l, @s);

    foreach (1..$n) {
        $_ = <>;
        my ($d, $l) = split;
        push @d, int $d;
        push @l, int $l;
    }

    my $D = int <>;
    my $y = 0;

    $s[0] = $d[0];
    my $j = 1;

    foreach my $i (0..$n-1) {

        if ($d[$i] + $s[$i] >= $D) {
            $y = 1;
            last;
        }

        while ($j < $n) {
            last unless $d[$i] + $s[$i] >= $d[$j];
            $s[$j] = min($l[$j], $d[$j] - $d[$i]);
            ++$j;
        }
    }

    print "Case #$caseno: ", ($y ? "YES" : "NO"), "\n";
    ++$caseno;
}

