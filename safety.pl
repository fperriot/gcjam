use List::Util qw/sum min/;
use strict;

our $caseno = 0;

<>;
while (<>) {
    our ($n, @pts) = split;

    $caseno++;
    print "Case #$caseno:";

    my $t = sum @pts;

    for (my $i = 0; $i < $n; ++$i) {
        my $v = $pts[$i];
        my @opp = sort { $a <=> $b } (@pts[0..$i-1], @pts[$i+1..$n-1]);

        my @sigma = (0, @opp);
        $sigma[$_] += $sigma[$_-1] for 1..$n-1;

        my @ms;

        for (my $j = 0; $j < $n-1; ++$j) {

            my $x = 1 - ($j * $opp[$j] - $sigma[$j]) / $t;
            my $y = $opp[$j];
            #print "\n(j,x,y)=($j, $x, $y)\n";
            my $slope = - $t / ($j + 1);

            my $isect = ($y - $slope * $x - $v) / ($t - $slope);

            push @ms, $isect;

            #print "[$isect]\n";
        }

        my $m = min @ms;

        $m = 0 if $m < 0;

        printf " %.6f", $m * 100.0;
        #print " " . ($m * 100.0);
    }

    print "\n";
}

