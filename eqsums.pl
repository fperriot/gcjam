use strict;
use List::Util qw/sum/;

sub listdiff {
    my @a = @{$_[0]};
    my @b = @{$_[1]};
    my %has;
    map { $has{$_}++ } @b;
    grep { not $has{$_} } @a;
}

my $caseno = 1;

<>;
while (<>) {
    print "Case #$caseno:\n";
    ++$caseno;

    my ($n, @x) = split;
    my %subset;

    map { $_ = int $_ } @x; # for speed and size

LOOP:
    foreach my $i (0..$n-1) {
    foreach my $j ($i+1..$n-1) {
    foreach my $k ($j+1..$n-1) {
    foreach my $p ($k+1..$n-1) {
    foreach my $q ($p+1..$n-1) {
    foreach my $r ($q+1..$n-1) {
        my @subset = @x[$i, $j, $k, $p, $q, $r];
        my $sum = sum @subset;
        if (exists $subset{$sum}) {
            my @prev = @{$subset{$sum}};
            my @u = listdiff \@prev, \@subset;
            my @v = listdiff \@subset, \@prev;
            print "@u\n@v\n";
            #print "Found in $iter iterations\n";
            last LOOP;
        } else {
            $subset{$sum} = \@subset;
        }
    } } } } } }
}

