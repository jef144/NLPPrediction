#!/usr/bin/perl 
#Script to  pseudo randomly flag an input line as Test, Train, or skipped,  then write 
#to .trn and .tst datasets
#Accpets a list on the command line  of large text files ending in .txt, generate .trn and .tst in the current directory
#Typical invocation is is to position youself in the folder with the large corpus files,  then:
#    segmentTrnTst.pl `ls *.txt`


use strict;
use Data::Dumper;

#Set seed for  reproducibility

my $seed = 42;
srand($seed);

#Set the percentage of the input for the train and test scenarios
my $threshTrain = .1;
my  $threshTest = .1 ;


print "Removing all files in the trn and tst  directory..\n";
my $rc = `rm  trn/*`;
print $rc;    
$rc = `rm  tst/*`;
print $rc;    



#Iterate the list of input files
print Dumper @ARGV;

for my $input ( @ARGV) {
        my ($basename, $extension) = $input =~ /(.*)\.(\S+)$/;
        
	print "Opening input file $input with extension $extension...\n";
        if ($extension != 'txt') {
            print "Invalid extension $extension, skipping $input\n";
            next;
        } 
      

        
        $basename = 'en_US'; 
        open IN, "<$input"  or die "Could not open $input for read access ...\n";
        open TRN, ">>trn/$basename.trn"  or die "Could not open trn/$basename.trn for output ...\n";
        open TST, ">>tst/$basename.tst"  or die "Could not open tst/$basename.tst for output ...\n";

        while (<IN>) {

             	#First decide if this is train, test, or skipped line
                my $rand = rand();
 		my $target;   
 
                if ($rand <= $threshTrain) {
			$target = 'TRAIN';
		}  elsif ($rand <= $threshTrain + $threshTest)   {
			$target = 'TEST';
		} else    {
			$target = 'SKIP';
		} 
 
                #Slice up the input line into sentences.  First clear the line feed, then put a sentence delimter at the end... 
                #Sentence delims are .?!
       		chomp;
      	  	$_ = $_ . ".";               
 		#Split  on periods, question marks, exclamation points
        	my (@sentences) =    /(.*?)[.?!]/g;
                for my $sentence (@sentences) {
		# print "     $sentence\n"; 
		#print "$target" . ($target eq 'TRAIN') .  " \n";
        		print TRN  "$sentence\n" if $target eq 'TRAIN';
        		print TST  "$sentence\n" if $target eq 'TEST';
	   }	
	}

}
