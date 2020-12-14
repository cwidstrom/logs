#!/usr/bin/perl -w

use strict;

use FindBin;
use lib ($FindBin::RealBin);
use Utils;

# This script outputs Coverity analysis capabilities with respect to a taxonomy
# definition.  It is an example of how to fetch and process Coverity analysis
# capabilities, and join them to a taxonomy definition.  You can use this script
# as a starting point for your own needs.

## command line and options

my $usage = <<USAGE;
Usage: $0 --taxonomy <taxonomy_definition_json_file> --capabilities <coverity_analysis_capabilities_json_file> [--language <only_for_lang>]
Note: you probably want to run this tool against a *fully-resolved* taxonomy, such as is produced in build/issue-type-taxonomies/fully-resolved/
USAGE

sub printUsageAndDie {
    die "@_\n\n$usage";
}

## configuration variables
my $verbose = $ENV{'VERBOSE'} || 0;
my $baseDir = $FindBin::RealBin;
#my $workDir = File::Spec->catdir($baseDir, 'tmp');
my $capabilitiesFile = '';
my $taxonomyFile = '';
my @languageFilter = ();

## process command line
while (my $arg = shift @ARGV) {
    if ($arg eq '--taxonomy') {
        $taxonomyFile = shift(@ARGV);
    } elsif ($arg eq '--capabilities') {
        $capabilitiesFile = shift(@ARGV);
    } elsif ($arg eq '--language') {
        push @languageFilter, shift(@ARGV);
    }
}

## validate command line
printUsageAndDie() unless $taxonomyFile && $capabilitiesFile;
die "Can't find ${taxonomyFile}" unless -f $taxonomyFile;
die "Can't find ${capabilitiesFile}" unless -f $capabilitiesFile;


## get started

my $taxny = Utils::objectFromJsonFile($taxonomyFile);
my $capabilities = Utils::objectFromJsonFile($capabilitiesFile);

my @languages = ();
Utils::joinCapabilitiesToTaxonomy($taxny, $capabilities, \@languages, $verbose);

# Override computed languages with command line
if ($#languageFilter >= 0) {
    @languages = @languageFilter;
}

sub htmlOutput($@) {
    my ($taxny, @languages) = @_;

    # deal with either a fully resolved taxonomy or an unresolved one
    if (exists($taxny->{'taxonomy'})) {
        $taxny = $taxny->{'taxonomy'};
    }

    my %lang2taxa;
    my %lang2count;

    my @taxa = @{$taxny->{'taxa'}};
    
    ## output a table; each row is a taxon, each column is a language; each cell contains the check+subcategories that cover that taxon for that language
    ## consider this code an example of traversing the output of joinCapabilitiesToTaxonomy() that you can hack to taste 
    print <<HEADER;
<!DOCTYPE html>
<html>
  <head>
    <title>Coverity for $taxny->{'name'}->{'en'}</title>
  </head>
  <body>
    <table border=1>
      <tr>
        <th>taxon</th>
HEADER

    for my $lang (@languages) {
        print "        <th>${lang}</th>\n";
        $lang2count{$lang} = 0;
        $lang2taxa{$lang} = [];
    }
    print "      </tr>\n";

    for my $taxon (@taxa) {
        print "      <tr>\n";
        print ' ' x 8;
        print "<td>$taxon->{'id'} $taxon->{'name'}->{'en'}</td>\n";
        for my $lang (@languages) {
            # remove duplicates
            my %checkerAndSubcategory;
            for my $cap (@{$taxon->{'coverity-analysis-capabilities-by-language'}->{$lang}}) {
                $checkerAndSubcategory{"$cap->{'checkerName'} $cap->{'subcategory'}"} = 1;
            }
            my @css = sort { $a cmp $b } keys(%checkerAndSubcategory);

            # list checker + subcategory pairs
            print "        <td><ul>\n";
            for my $cs (@css) {
                print ' ' x 10;
                print "<li>$cs</li>\n";
            }
            print "        </ul></td>\n";

            if (@css) {
                $lang2count{$lang} += 1;
                push(@{$lang2taxa{$lang}}, $taxon->{'id'});
            }
        }
        print "      </tr>\n";
    }
    print "</table>\n";
    

    # another table; this one lists the taxa we support for each language and gives the count of taxa for each language
    print "<table border=1><tr><th>language</th><th>count of Taxa</th><th>Taxa</th></tr>\n";
    for my $lang (@languages) {
        print "  <tr><td>$lang</td><td>$lang2count{$lang}</td><td>" . join(',', @{$lang2taxa{$lang}}) . "</td></tr>\n";
    }
    print "</table>\n";

    # a hack: output the language to taxa mapping as JSON; the computation of lang2taxa could be factored out to a helper instead of interleaved with table construction above
    Utils::objectToJsonFile(\%lang2taxa, 'caps.json', '');

    # dump taxny
    Utils::objectToJsonFile($taxny, 'joined.json', '');
    

    # another table; this one lists issue types and the taxa that each one intersects with
    my %it2lang2taxa;
    my %it2taxa;

    for my $taxon (@taxa) {
        my $tid = $taxon->{'id'};
        for my $lang (@languages) {
            for my $cap (@{$taxon->{'coverity-analysis-capabilities-by-language'}->{$lang}}) {
                my $type = $cap->{'type'};
                my $subtype = $cap->{'subtype'} || '';
                my $it = $type . ':' . $subtype;
                $it2taxa{$it} = {} unless exists($it2taxa{$it});
                $it2taxa{$it}->{$tid} = 1;

                $it2lang2taxa{$it} = {} unless exists($it2lang2taxa{$it});
                $it2lang2taxa{$it}->{$lang} = {} unless exists($it2lang2taxa{$it}->{$lang});
                $it2lang2taxa{$it}->{$lang}->{$tid} = 1;
            }
        }
    }
    print "<table border=1><tr><th>issue type</th><th>Taxa</th>" 
        . join("</th><th>", @languages) 
        . "</th></tr>\n";
    my @its = sort { $a cmp $b } keys(%it2taxa);
    for my $it (@its) {
        print "<tr>\n<td>$it</td>\n<td>" 
            . join(",", sort { $a cmp $b } keys(%{$it2taxa{$it}})) 
            . "</td>\n<td>";
        for my $lang (@languages) {
            my @tids = exists($it2lang2taxa{$it}->{$lang}) ? keys(%{$it2lang2taxa{$it}->{$lang}}) : ();
            print "</td>\n<td>" . join(",", sort { $a cmp $b } @tids);
        }
        print "</td>\n</tr>"
    }
    print "</table>\n";

    
    print <<FOOTER;
  </body>
</html>
FOOTER
}

htmlOutput($taxny, @languages);

