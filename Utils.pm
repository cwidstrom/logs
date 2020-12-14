package Utils;
use strict;
use warnings;
use Exporter qw(import);
use JSON::PP;
use Data::Dumper;


##############################################################################
## Takes a file name.  Returns a perl representation of the JSON in the file.
## Dies on error.
sub objectFromJsonFile($) {
    my $infile = shift;
    open(my $in, '<', $infile) or die "open($infile): $!";
    local $/;
    my $jsonText = <$in>;
    close($in) or warn "close($infile): $!";

    my $jsonParser = JSON::PP->new
        ->relaxed()            # allow trailing comma, '#'-comments
        ->allow_singlequote()  # allow 'key' : 'value'
        ->allow_barekey()      # allow key : "value"
        ;
    return $jsonParser->decode($jsonText);
}

##############################################################################
## Takes an object (presumably a hash ref), a file name, and a comparator for
## keys.  Output JSON for the object to the file with JSON object keys sorted
## according to the comparator.
sub objectToJsonFile($$$) {
    my $ref = shift;
    my $outfile = shift;
    my $jsonKeyToSortOrder = shift || '';

    my $jsonEncoder = JSON::PP->new
        ->indent()
        ->indent_length(4)
        ->space_before(0)->space_after(1);  # output as "key": "value"

    # note: The default settings of the JSON::PP encoder/decoder seem to
    # round-trip UTF-8 encoded files just fine (see
    # ../tests/test-canonicalize-taxonomy/utf-8.json).

    # allow control over key order for JSON objects
    if ($jsonKeyToSortOrder) {
        my %m = %$jsonKeyToSortOrder;
        my $keysComparatorForJsonOutput = sub {
            my $a = $JSON::PP::a;
            my $b = $JSON::PP::b;
            return ($m{$a} || $a) cmp ($m{$b} || $b);
        };
        $jsonEncoder = $jsonEncoder->sort_by($keysComparatorForJsonOutput);
    }

    my $outString = $jsonEncoder->encode($ref);
    
    open(my $out, '>', $outfile) or die "open($outfile): $!";
    print $out ($outString);
    close($out) or die "close($outfile): $!";
}

#################################################################################
# Takes a taxonomy and an output file, cannonicalizes the taxonomy, and writes
# it to the output file.  The taxonomy can either be an object (previously read
# from objectFromJsonFile and manipulated) or an input JSON file.
sub canonicalizeTaxonomy($$) {
    my $infileOrTaxny = shift;
    my $outfile = shift;

    ##
    ## helpers
    ##

    # A sort helper to make sure that we sort taxon IDs such that
    #   id1 < id2 < id2.2 < id2.10 < id10
    sub id2k($) {
        my $key = shift;
        $key =~ s/(\d+)/sprintf("%09u", $1)/eg;
        return $key;
    }

    # A key ordering for JSON objects that we'll use to order the keys in the JSON
    # objects we output.  It has to work for every level: top level, taxonomy level,
    # and taxon level.
    my %keyOrder = (

        # top level
        'taxonomy-type' => '_ba',
        'format-version' => '_bb',
        'taxonomy' => '_bc',

        # taxonomy level
        'name' => '_ca',
        'depends-on' => '_cb',
        'description' => '_cc',
        'taxon-order' => '_cd',
        'abbreviation' => '_ce',
        'extra' => '_xa',
        'taxa' => '_ya',

        # taxon level
        'id' => '_aa',
        # name uses value from above
        # description uses value from above
        # abbreviation uses value from above
        'child-taxa' => '_dc',
        # includes?
        'issue-types' => '_de',
        # extra uses value from above

        # sort 'en' to the top when sorting locales
        'en' => '_',
        );

    ##
    ## main body of canonicalizeTaxonomy
    ##

    my $taxny;
    if ('HASH' eq ref($infileOrTaxny)) {
        $taxny = $infileOrTaxny;
    } else {
        $taxny = Utils::objectFromJsonFile($infileOrTaxny);
    }

    # make sure the JSON has the structure we expect; we tolerate either raw
    # taxonomy definitions (with a 'taxonomy' object at top level or a resolved
    # definition where top level just has list under 'taxa'.
    my $warnString = "WARNING: skipping canonicalizeTaxonomy($infileOrTaxny):";
    if ('HASH' ne ref($taxny)) {
        warn "${warnString} expected a JSON object at top level, but got " . ref($taxny);
        return;
    }

    my $taxaRef;  # a pointer we can write through (see below) to update the list of taxa
    my $taxonomyRef; # a pointer to write missing values in taxonomy
    if (exists($taxny->{'taxa'})) {
        $taxaRef = \$taxny->{'taxa'};
        $taxonomyRef = \$taxny;
    } 
    elsif ( exists($taxny->{'taxonomy'}) 
            && 'HASH' eq ref($taxny->{'taxonomy'})
            && exists($taxny->{'taxonomy'}->{'taxa'}) )
    {
        $taxaRef = \$taxny->{'taxonomy'}->{'taxa'};
        $taxonomyRef = \$taxny->{'taxonomy'}
    } 
    else {
        warn "${warnString} couldn't find the 'taxa' list.";
        return;
    }
    if ('ARRAY' ne ref($$taxaRef)) {
        warn "${warnString} expected a list for 'taxa' but got " . ref($$taxaRef);
        return;
    } 

    # Replace null value for taxonomy description with an empty map
    unless (exists($$taxonomyRef->{'description'})) {
        $$taxonomyRef->{'description'} = {};
    }

    # Replace null value for taxonomy abbreviation with an empty map
    unless (exists($$taxonomyRef->{'abbreviation'})) {
         $$taxonomyRef->{'abbreviation'} = {};
    }
    # Replace null value for taxonomy extra with an empty map
    unless (exists($$taxonomyRef->{'extra'})) {
        $$taxonomyRef->{'extra'} = {};
    }
    # Replace null value for taxonomy depends on with an empty list
    unless (exists($$taxonomyRef->{'depends-on'})) {
        $$taxonomyRef->{'depends-on'} = [];
    }

    # sort taxa by id
    my @taxa = @$$taxaRef;
    my @sortedTaxa = sort { id2k($a->{'id'}) cmp id2k($b->{'id'}) } @taxa;

    # sort issue-type mapping (alphabetically) and child-taxa (by id)
    for my $taxon (@sortedTaxa) {
        if (exists($taxon->{'issue-types'})) {
            my @sits = sort {$a cmp $b} @{$taxon->{'issue-types'}};
            $taxon->{'issue-types'} = \@sits;
        }
        if (exists($taxon->{'child-taxa'})) {
            my @sct = sort { id2k($a) cmp id2k($b) } @{$taxon->{'child-taxa'}};
            $taxon->{'child-taxa'} = \@sct;
        }
    }

    # put the sorted list of taxa back
    $$taxaRef = \@sortedTaxa;

    Utils::objectToJsonFile($taxny, $outfile, \%keyOrder);
}


#################################################################################
# The joinCapabilitiesToTaxonomy function exists to facilitate documentation of
# Coverity analysis capabilities with respect to a taxonomy.

#
sub typeSubtypeLanguageToSingleLine($$$) {
    my ($type, $subtype, $language) = @_;
    $subtype = ':' . $subtype if $subtype;
    $language = '|' . $language if $language;
    return "${type}${subtype}${language}";
}
sub typeSubtypeLanguageFromSingleLine($) {
    my ($singleLine) = @_;
    if ($singleLine =~ m/^([^:|]+)(?::([^|]*))?(?:\|(.*))?$/) {
        return ($1, $2 || '', $3 || '');
    }
}

# get the lowercased, canonical form of the language for matching with taxonomies
sub getCodeLanguage($) {
    my ($cap) = @_;
    return $cap->{'code-language'} || lc($cap->{'language'} || 'unknown');
}

# Given a type, subtype, and language, provide a list of keys that could match
# it in a taxon's 'issue-types' list.
sub typeSubtypeLanguageToTaxonKeys($$$) {
    my ($type, $subtype, $language) = @_;
    return ( typeSubtypeLanguageToSingleLine($type, $subtype, $language),
             typeSubtypeLanguageToSingleLine($type, '', $language),
             typeSubtypeLanguageToSingleLine($type, $subtype, ''),
             typeSubtypeLanguageToSingleLine($type, '', '') );
}

# Debug function copy/pasted from:
# https://stackoverflow.com/questions/2431032/how-do-i-print-a-hash-structure-in-perl
sub printStruct {
    my ($struct,$structName,$pre)=@_;
    print "-----------------\n" unless (defined($pre));
    if (!ref($struct)){ # $struct is a scalar.
    print "$structName=$struct\n";
    } elsif (ref($struct) eq "ARRAY") { # Struct is an array reference
    return ("ARRAY(".scalar(@$struct).")") if (@$struct>100);
    for(my$i=0;$i<@$struct;$i++) {
        if (ref($struct->[$i]) eq "HASH") {
        printStruct($struct->[$i],$structName."->[$i]",$pre." ");
        } elsif (ref($struct->[$i]) eq "ARRAY") { # contents of struct is array ref
        print "$structName->"."[$i]=()\n" if (@{$struct->[$i]}==0);
        my $string = printStruct($struct->[$i],$structName."->[$i]",$pre." ");
        print "$structName->"."[$i]=$string\n" if ($string);
        } else { # contents of struct is a scalar, just print it.
        print "$structName->"."[$i]=$struct->[$i]\n";
        }
    }
    return();
    } else { # $struct is a hash reference or a scalar
    foreach (sort keys %{$struct}) {
        if (ref($struct->{$_}) eq "HASH") {
        printStruct($struct->{$_},$structName."->{$_}",$pre." ");
        } elsif (ref($struct->{$_}) eq "ARRAY") { # contents of struct is array ref
        my $string = printStruct($struct->{$_},$structName."->{$_}",$pre." ");
        print "$structName->"."{$_}=$string\n" if ($string);
        } else { # contents of struct is a scalar, just print it.
        print "$structName->"."{$_}=$struct->{$_}\n";
        }
    }
    return();
    } 
    print "------------------\n" unless (defined($pre));
    return();
}

# build a look-up table for resolving issue-types entries in taxonomy definitions
# return the table and a list of code-languages
sub capabilitiesToLookupTable {
    my $capabilities = shift;
    my $verbose = shift || 0;
    my %it2entry;
    my %codeLanguages;
    for my $cap (@$capabilities) {
        my $type = $cap->{'type'};
        die "capabilities seem to be malformed (or from a version of cov-analzye that's too old)" unless defined($type);
        my $subtype = $cap->{'subtype'} || '';
        my $language = getCodeLanguage($cap);

        $codeLanguages{$language} = 1;

        my %done; # avoid duplicates, for example when subtype is empty

        # taxonomy definitions can leave out subtype, language, or both
        for my $k (typeSubtypeLanguageToTaxonKeys($type, $subtype, $language)) {
            next if $done{$k};
            $done{$k} = 1;
            print "$k\n" if $verbose >= 2;
            $it2entry{$k} = [] unless exists $it2entry{$k};
            push(@{$it2entry{$k}}, $cap);
        }
    }
    my @codeLanguages = sort { $a cmp $b } (keys(%codeLanguages));
    return (\%it2entry, \@codeLanguages);
}

sub joinCapabilitiesToTaxonomy($$$$) {
    my ($taxny, $capabilities, $languages, $verbose) = @_;

    my ($it2entry, $codeLanguages) = capabilitiesToLookupTable($capabilities, $verbose);
    push (@$languages, @$codeLanguages);

    ## deal with either a fully resolved taxonomy or an unresolved one
    if (exists($taxny->{'taxonomy'})) {
        $taxny = $taxny->{'taxonomy'};
    }
    ## resolve issue-types entries; update the taxonomy in place
    my @taxa = @{$taxny->{'taxa'}};
    for my $taxon (@taxa) {
        if (exists($taxon->{'issue-types'})) {
            my @its = @{$taxon->{'issue-types'}};
            my @tcaps = ();
            my %lang2caps;
            for my $it (@its) {
                if (exists($it2entry->{$it})) {
                    my $caps = $it2entry->{$it};
                    push(@tcaps, @$caps);
                    for my $cap (@$caps) {
                        my $lang = getCodeLanguage($cap);
                        $lang2caps{$lang} = [] unless exists $lang2caps{$lang};
                        push (@{$lang2caps{$lang}}, $cap);
                    }
                } else {
                    if ($verbose) {
                        my $taxonId = $taxon->{'id'};
                        my $taxonName = $taxon->{'name'}->{'en'};
                        print "*** Couldn't resolve mapping '${it}' to an analysis capability for taxon ${taxonId} ${taxonName}\n";
                    }
                }
            }
            $taxon->{'coverity-analysis-capabilities'} = \@tcaps;
            $taxon->{'coverity-analysis-capabilities-by-language'} = \%lang2caps;
        }
    }
}




#################################################################################
# The functions checkJsonValuesIsomorphic, checkJsonObjectsIsomorphic,
# checkJsonListsIsomorphic are mutually recrsive and traverse two JSON
# structures to check if they are isomorphic: strings are equal; lists have the
# same contents; objects have the same keys and equal objects for the same key.
# they all return empty string if they are isomorphic; otherwise return a
# non-empty error message explaining why they are not.  Their first two
# arguments are the values from JSON deserialization; their last is context
# information to include in a potential error message.

sub categorizeJsonSubstructure($) {
    my $v = shift;
    my $rv = ref($v);
    return 'list' if ('ARRAY' eq $rv);
    return 'object' if ('HASH' eq $rv);
    return 'scalar' if ('' eq $rv);
    die "($v)($rv)";
}

# Check that two JSON values (strings, lists, objects) are isomorphic.
sub checkJsonValuesIsomorphic($$$) {
    my $v1 = shift;
    my $v2 = shift;
    my $context = shift || '';
    my $v1cat = categorizeJsonSubstructure($v1);
    my $v2cat = categorizeJsonSubstructure($v2);

    if ($v1cat eq $v2cat) {
        if ('list' eq $v1cat) {
            return checkJsonListsIsomorphic($v1, $v2, $context);
        } elsif ('object' eq $v1cat) {
            return checkJsonObjectsIsomorphic($v1, $v2, $context);
        } elsif ('scalar' eq $v1cat) {
            if ($v1 eq $v2) {
                return '';
            } else {
                return "${context}: scalar values not equal: '${v1}' != '${v2}'\n";
            }
        } else {
            die $v1cat;
        }
    } else {
        return "${context}: incompatible structure (${v1cat}, ${v2cat})\n";
    }
}

# Check that two JSON objects (passed in as hash references) are isomorphic.
sub checkJsonObjectsIsomorphic($$$) {
    my $o1 = shift;
    my $o2 = shift;
    my $context = shift || '';

    my %keycount;
    my $k;
    my $res = '';

    for $k (keys(%$o1)) {
        $keycount{$k} += 1;
    }
    for $k (keys(%$o2)) {
        $keycount{$k} += 1;
    }
    my @keys = sort { $a cmp $b } (keys(%keycount));

    for $k (@keys) {
        if (2 == $keycount{$k}) {
            $res .= checkJsonValuesIsomorphic($o1->{$k}, $o2->{$k}, "${context}/${k}");
        } else {
            $res .= "${context}: only "
                . (exists($o1->{$k}) ? 'first' : 'second')
                . " has key ${k}\n";
        }
    }
    return $res;
}

# Check that two JSON lists (passed as list references) are isomorphic.  Hack:
# This function will sort by 'id' field if one exists; otherwise it'll sort by
# value.  This function doesn't make sense on other kinds of structures.
sub checkJsonListsIsomorphic($$$) {
    my $l1 = shift;
    my $l2 = shift;
    my $context = shift || '';

    my $sz1 = 1 + $#{$l1};
    my $sz2 = 1 + $#{$l2};
    if ($sz1 != $sz2) {
        return "${context} lists are not the same size: $sz1 != $sz2";
    }

    my @sl1;
    my @sl2;
    if ($sz1 > 0 && 'HASH' eq ref($l1->[0]) && exists($l1->[0]->{'id'})) {
        @sl1 = sort { ($a->{'id'}) cmp ($b->{'id'}) } @$l1;
        @sl2 = sort { ($a->{'id'}) cmp ($b->{'id'}) } @$l2;
    } elsif ($sz1 > 0 && '' eq ref($l1->[0])) {
        @sl1 = sort { $a cmp $b } @$l1;
        @sl2 = sort { $a cmp $b } @$l2;
    } else {
        @sl1 = @$l1;
        @sl2 = @$l2;
    }

    my $res = '';
    for (my $i=0; $i<$sz1; ++$i) {
        $res .= checkJsonValuesIsomorphic($sl1[$i], $sl2[$i], $context . "[" . $i . "]");
    }
    return $res;
}

# check that two JSON files are isomorphic: they have the same structure (same
# keys for objects, same values for each key, same values in each list)
sub checkJsonFilesIsomorphic($$) {
    my $file1 = shift;
    my $file2 = shift;
    return checkJsonValuesIsomorphic( objectFromJsonFile($file1),
                                      objectFromJsonFile($file2),
                                      "(${file1}, ${file2})" );
}



#################################################################################
# Explain the exit code from a 'waitpid' or 'system' call.  This code is more or
# less verbatim from the perl manual for 'system'.
sub explainDeath($) {
    my $exitCode = shift;
    if ($exitCode == -1) {
        return "failed to execute: $!";
    } elsif ($exitCode & 127) {
        return sprintf("died with signal %d, %s coredump",
                       ($exitCode & 127),  ($exitCode & 128) ? 'with' : 'without');
    } else {
        return sprintf("exited with value %d", $exitCode >> 8);
    }
}

###############################################################################
### license for runWithTimeout function -- from http://trend-prof.tigris.org/
#
#Copyright (c) 2005-2008, Regents of the University of California.
#All rights reserved.
#
#Redistribution and use in source and binary forms, with or without
#modification, are permitted provided that the following conditions are
#met:
#
#    Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
#
#    Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in
#    the documentation and/or other materials provided with the
#    distribution.
#
#    Neither the name of the University of California, Berkeley nor the
#    names of its contributors may be used to endorse or promote
#    products derived from this software without specific prior written
#    permission.
#
#THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
### end license for runWithTimeout function
#
# Run @cmd in a separate process, but kill it and all its children after
# $timeout seconds.  Undefined or timeout <= 0 menas no timeout.  On success,
# return empty string; otherwise return an error message explaining what went
# wrong
sub runWithTimeout($@) {
    my ($timeout, @cmd) = @_;

    # setup alarm handler
    $timeout = 0 if !defined($timeout) || $timeout < 0;
    my $alarmMsg = "TIMEOUT after $timeout seconds\n";
    $SIG{ALRM} = sub { die $alarmMsg; };

    my $exitCode;
    my $childPid;
    eval {
        alarm($timeout);
        $childPid = fork();
        die "fork failed\n" unless defined($childPid);
        if (0 == $childPid) {
            # child: set this process to be in a new group and exec
            setpgrp(0,0) or die "setpgrp failed\n";
            exec(@cmd) or die("exec '", join(" ", @cmd), "' failed\n");
        } else {
            #parent
            waitpid($childPid,0);
            $exitCode = $?;
        }
    };
    # we're done with the alarm one way or another
    alarm(0);
    $SIG{ALRM} = sub {};

    # kill the whole process group; wait for a child
    if (defined($childPid)) {
        kill(-9, $childPid);
        waitpid($childPid,0);
    }

    # figure out our return value
    if (!defined($exitCode) && defined($@)) {
        return $@;
    } else {
        return "waitpid returned undefined result" unless defined($exitCode);
        return "" if 0 == $exitCode;
        return explainDeath($exitCode);
    }
}

##############################################################################
sub cleanDiffOrDie {
    my $f1 = shift;
    my $f2 = shift;
    my $opts = shift || '';

    my $diffCmd = "diff $opts '$f1' '$f2'";
    my $res = system($diffCmd);
    if (0 != $res) {
        #my $diff = `$diffCmd`;
        $res = explainDeath($res);
        #die "Differences detected with '$diffCmd' (exited with $res):\n$diff\n";
        die "Differences detected with '$diffCmd' (exited with $res)\n";
    }
}

##############################################################################
# Takes a path; does a "rm -rf" on it, then recreates it.  Die unless a
# directory exists at the end (technically the remove could fail and that's
# fine).
sub removeAndRecreateDir($) {
    my $dir = shift;
    File::Path::remove_tree($dir);
    File::Path::make_path($dir);
    die unless -d $dir;
}

##############################################################################
# SAT-34942: Checks that every entry in the "taxon-order" field matches to an "id".
sub checkTaxonOrderIsValid {
    my $infile = shift;

    my $taxny;
    $taxny = Utils::objectFromJsonFile($infile);

    # Locate the 'taxon-order' list if it exists
    my $taxonOrder;
    my $warnString = "WARNING: skipping checkTaxonOrderIsValid($infile):";
    if (exists($taxny->{'taxonomy'}->{'taxon-order'})) {
        $taxonOrder = $taxny->{'taxonomy'}->{'taxon-order'};
    }
    else {
        return;
    }
    if ('ARRAY' ne ref($taxonOrder)) {
        warn "${warnString} expected a list for 'taxon-order' but got " . ref($taxonOrder);
        return;
    }

    # Locate the 'taxa' list
    my $taxa;
    if (exists($taxny->{'taxonomy'}->{'taxa'})) {
        $taxa = $taxny->{'taxonomy'}->{'taxa'};
    }
    if ('ARRAY' ne ref($taxa)) {
        warn "${warnString} expected a list for 'taxa' but got " . ref($taxa);
        return;
    }

    # Create a hash of all 'id' from 'taxa'
    my %ids;
    foreach my $taxon (@$taxa) {
        my $id = $taxon->{'id'};
        $ids{$id} = 1;
    }

    # Check that each 'id' appears in the taxon-order
    # This ensures that there are no missing ids in the taxon-order
    my %revhash;
    $revhash{$_}++ for (@$taxonOrder);
    my @missing;
    foreach my $id (keys %ids) {
        push @missing, $id if (!exists($revhash{$id}));
    }
    die "There are 'id' entries that do not match to a 'taxon-order' in $infile.\n'id' with missing 'taxon-order' entry:\n".Dumper(\@missing)
        if (scalar @missing > 0);

    # Check that there are no duplicate entries in 'taxon-order'.
    # This ensures that there are no duplicate ids in the taxon-order
    my %seen;
    my @duplicates = grep {$seen{$_}++} @$taxonOrder;
    die "There are duplicate entries in 'taxon-order' in $infile.\nDuplicate entries:\n".Dumper(\@duplicates)
        if (scalar @duplicates > 0);

    # Check that each entry in 'taxon-order' exists as an 'id' in 'taxa'
    my @taxonEntryWithInexistentID;
    foreach my $entry (@$taxonOrder) {
        if(!exists($ids{$entry})) {
            push @taxonEntryWithInexistentID, $entry;
        }
    }
    my $num = scalar @taxonEntryWithInexistentID;
    if ($num != 0){
        # Every entry in 'taxon-order' should exist as an 'id' in 'taxa'
        die "There are $num entry(s) in the 'taxon-order' that do not match to any 'id' in $infile\n".Dumper(\@taxonEntryWithInexistentID);
    }
    
}

##############################################################################
# perl wants this "1;" at the end.  Don't ask.
1;
