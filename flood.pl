use strict;
use List::Util qw/sum min/;

<>;
while (<>) {
    #print;
    my ($n, @pts) = split;

    print "Case #", $.-1, ":";

    my $t = sum @pts;
    my $tinit = $t;
    my @fld = @pts;

    while ($t > 0) {
        my $min = min @fld;
        my $tied = scalar grep { $_ == $min } @fld;
        #print "$tied tied for last at $min\n";
        last unless $t >= $tied;
        foreach (@fld) { if ($_ == $min) { $_++ } }
        $t -= $tied;
    }

    #print "$t points left to give\n";

    my $min = min @fld;
    my $tied = scalar grep { $_ == $min } @fld;

    #print "$tied tied for last at $min\n";

    for (my $i = 0; $i < $n; ++$i) {

        my $need = $fld[$i] - $pts[$i];

        if ($fld[$i] == $min) {
            $need += $t / $tied;
        }

        my $m = $need / $tinit;

        printf " %f", $m * 100.0;
    }

    print "\n";
}

