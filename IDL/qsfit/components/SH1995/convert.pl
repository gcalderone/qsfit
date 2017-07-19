#!/usr/bin/perl

$pre = "ss";
$a = 0;
while (<>) {
	chomp;

	if ($a == 1) {
		$a = 0;
		print "\n";
	} 
	else {
		if (/DENS/) {$a=1;}
		else {
			if (/_/)  {
				print "\n$_";
			}
			else {
				if (/BNS/)  {
					print "\n$_";
				}
				else {
					s/ \d\d /    /g;
					s/  \d /    /g;				
					s/ \d\d-/   -/g;
					s/  \d-/   -/g;
					print;
				}
			}
		}
	}
}
