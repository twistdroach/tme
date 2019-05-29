#! /usr/local/bin/perl -w

# $Id: m68k-opmap-make.pl,v 1.4 2003/06/27 20:42:31 fredette Exp $

# m68k-opmap-make.pl - compiles the complete decoding of all legal
# first-instruction-word values into the opcode map used by the C
# decoder:

#
# Copyright (c) 2002, 2003 Matt Fredette
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. All advertising materials mentioning features or use of this software
#    must display the following acknowledgement:
#      This product includes software developed by Matt Fredette.
# 4. The name of the author may not be used to endorse or promote products
#    derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
# INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
# STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#

# globals:
$0 =~ /^(.*\/)?([^\/]+)$/; $PROG = $2;
$debug = 0;

# to silence -w:
undef($value);

# emit our header:
print <<"EOF;";
/* generated automatically by $PROG, do not edit! */

/* includes: */
#include "m68k-impl.h"
EOF;

# we begin with no submaps and no opcode maps:
$submap_next = 0;
$opcode_map_next = 0;
%opcode_maps = ();

# assuming an ILP32 machine, various sizeofs:
$sizeof_opcode = 12;
$sizeof_submap = 64 * 24;

# loop over standard input:
for ($line = 1; defined($_ = <STDIN>); $line++) {
    chomp;

    # break the line into tokens:
    @tokens = split(' ', $_);

    # if this is the beginning of a new CPU:
    if ($tokens[0] eq "cpu-begin") {
	$cpu_name = $tokens[1];

	# initialize for this CPU:
	print STDERR "$PROG: initializing for $cpu_name...";

	# initialize the full map:
	undef(@map_line);
	for ($pattern = 65536; $pattern-- > 0;) {
	    $map_op0[$pattern] = "U";
	    $map_op1[$pattern] = "U";
	    $map_eax_size[$pattern] = "U";
	    $map_eax_cycles[$pattern] = "U";
	    $map_imm_operand[$pattern] = "U";
	    $map_imm_size[$pattern] = "U";
	    $map_eay_size[$pattern] = "U";
	    $map_eay_cycles[$pattern] = "U";
	}

	# initialize the special operations:
	undef(%specop);

	# we're done initializing and we're now reading patterns:
	$patterns = 0;
	print STDERR " done\n$PROG: reading $cpu_name patterns...";
    }

    # if this is a special-operation line:
    elsif ($tokens[0] eq "specop") {
	shift(@tokens);
	$specop = shift(@tokens);
	foreach (@tokens) {
	    $specop{$_} = $specop;
	}
    }

    # if this is a pattern:
    elsif ($tokens[0] =~ /^[01]/) {
    
	# the first token is the pattern.  die if this pattern has already
	# appeared:
	$pattern = oct("0b".shift(@tokens));
	die "stdin:$line: duplicate pattern of line $map_line[$pattern]\n"
	    if (defined($map_line[$pattern]));
	$map_line[$pattern] = $line;

	# fill this map entry:
	foreach $token (@tokens) {
	    ($what, $value) = split(/=/, $token, 2);
	    eval("\$map_".$what."[$pattern] = \$value;");
	}
	die "stdin:$line: no function given\n"
	    if (!defined($map_func[$pattern]));
	$patterns++;
    }

    # if this is the end of a CPU:
    elsif ($tokens[0] eq "cpu-end") {
	$cpu_name = $tokens[1];

	# note how many patterns we read:
	print STDERR " read $patterns $cpu_name patterns\n";
    
	# sanity-check the information read in, and force all unused
	# full map entries to illegal:
	print STDERR "$PROG: finding unused $cpu_name patterns...";
	$unused = 0;
	for ($pattern = 65536; $pattern-- > 0;) {

	    # if this is an unused map entry:
	    if (!defined($map_line[$pattern])) {
		$map_func[$pattern] = "illegal";
		$unused++;
		next;
	    }

	    # since the overwhelming majority of instructions use at
	    # most one effective address path (the eax path), only the
	    # size of the eax path operand is stored in the opcode
	    # maps.  any instruction that uses the eay path must do so
	    # with the same size as the eax path:
	    if ($map_eay_size[$pattern] ne "U") {

		# if this instruction isn't using the eax path, switch
		# to using the eax path instead.  the only instruction
		# allowed to do this is move.S.  temporarily rewrite
		# this function to a special function "movenonmemtomem":
		if ($map_eax_size[$pattern] eq "U") {
		    die "$PROG: pattern ".sprintf("%b", $pattern)." ($map_func[$pattern]) uses the eay path\n"
			if ($map_func[$pattern] !~ /^(move)(\d+)$/);
		    $map_func[$pattern] = $1."nonmemtomem".$2;
		    $map_eax_size[$pattern] = $map_eay_size[$pattern];
		    $map_eax_cycles[$pattern] = $map_eay_cycles[$pattern];
		    $map_op0[$pattern] =~ s/^memy/memx/;
		    $map_op1[$pattern] =~ s/^memy/memx/;
		}

		# otherwise, this instruction is using both ea paths.
		# the only instruction allowed to do this is move.S.
		# temporarily rewrite this function to a special
		# function "movememtomem":
		else {
		    die "$PROG: pattern ".sprintf("%b", $pattern)." ($map_func[$pattern]) uses both ea paths\n"
			if ($map_func[$pattern] !~ /^(move)(\d+)$/);
		    $map_func[$pattern] = $1."memtomem".$2;
		    die "$PROG: pattern ".sprintf("%b", $pattern)." ($map_func[$pattern]) uses both ea paths at different sizes\n"
			if ($map_eay_size[$pattern] ne $map_eax_size[$pattern]);
		    die "$PROG: pattern ".sprintf("%b", $pattern)." ($map_func[$pattern]) doesn't use eay write-only\n"
			if ($map_eay_cycles[$pattern] ne "wo");
		}
	    }
	}
	print STDERR " found $unused unused $cpu_name patterns\n";

	# loop over the root patterns to come up with the root map
	# entry, the submap key and the functions list for each:
	print STDERR "$PROG: making $cpu_name root map entries and submap keys...";
	for ($root = 0; $root < 1024; $root++) {

	    # get counts of the different map entry attribute values.
	    # the most popular attribute values will get stored in the
	    # root map entry, while the others will get set in submap
	    # entries:
	    #
	    undef(%count_func);
	    undef(%count_op0);
	    undef(%count_op1);
	    undef(%count_eax_size);
	    undef(%count_imm);
	    for ($sub = 0, $pattern = $root << 6; $sub < 64 ; $sub++, $pattern++) {

		# the decoder specially cancels all attributes other
		# than "function" when the function is "illegal", so
		# don't count any of its attribute values.
		#
		# otherwise, function, specop, and EA cycles go
		# together, and immediate operand and immediate size
		# go together:
		#
		$func = $map_func[$pattern];
		if ($func ne "illegal") {
		    $specop = "un" if (!defined($specop = $specop{$func}));
		    $func .= ".$specop";
		    if ($map_eax_size[$pattern] eq "U") {
			# a pattern that doesn't use the EA path can have
			# any value for its opcode map entry cycles:
			die "$PROG: pattern ".sprintf("%b", $pattern)." ($func) doesn't use the EA path but needs $map_eax_cycles[$pattern] cycles?\n"
			    if ($map_eax_cycles[$pattern] ne "U");
			$func .= ".any";
		    }
		    elsif ($map_eax_size[$pattern] eq "UNSIZED") {
			# a pattern that uses the raw effective address
			# must have UNDEF for its opcode map entry cycles:
			die "$PROG: pattern ".sprintf("%b", $pattern)." ($func) takes the raw EA path but needs $map_eax_cycles[$pattern] cycles?\n"
			    if ($map_eax_cycles[$pattern] ne "U");
			$func .= ".un";
		    }
		    else {
			# otherwise, this pattern must have its specified
			# value for its opcode map entry cycles:
			die "$PROG: pattern ".sprintf("%b", $pattern)." ($func) uses the EA path at size $map_eax_size[$pattern] but needs $map_eax_cycles[$pattern] cycles?\n"
			    if ($map_eax_cycles[$pattern] eq "U"
				|| $map_eax_cycles[$pattern] eq "un");
			$func .= ".".$map_eax_cycles[$pattern];
		    }
		    $map_func[$pattern] = $func;
		    $count_op0{$map_op0[$pattern]}++;
		    $count_op1{$map_op1[$pattern]}++;
		    $count_eax_size{$map_eax_size[$pattern]}++;
		    $count_imm{$map_imm_operand[$pattern].".".$map_imm_size[$pattern]}++;
		}
		$count_func{$func}++;
	    }

	    # a pattern that doesn't use the EA path can use the same
	    # opcode map entry as a pattern that has the same function
	    # but does use the EA path, because the former pattern
	    # will have UNDEF for its eax size, which makes the cycles
	    # member of the opcode map entry a don't care:
	    #
	    @funcs = (keys(%count_func));
	    %funcs_rewrite = ();
	    foreach $func (@funcs) {
		@parts = split(/\./, $func);
		if (@parts == 3
		    && $parts[2] ne "any") {
		    $func_rewrite = $parts[0].".".$parts[1].".any";
		    if (defined($count_func{$func_rewrite})) {
			$count_func{$func} += delete($count_func{$func_rewrite});
			$funcs_rewrite{$func_rewrite} = $func;
		    }
		}
	    }
	    for ($sub = 0, $pattern = $root << 6; $sub < 64 ; $sub++, $pattern++) {
		$map_func[$pattern] = $func
		    if (defined($func = $funcs_rewrite{$map_func[$pattern]}));
	    }

	    # sort the attributes:
	    @funcs = (sort { ($count_func{$b} == $count_func{$a}
			      ? $a cmp $b
			      : $count_func{$b} <=> $count_func{$a}) } keys(%count_func));
	    @op0s = (sort { ($count_op0{$b} == $count_op0{$a}
			     ? $a cmp $b
			     : $count_op0{$b} <=> $count_op0{$a}) } keys(%count_op0));
	    @op1s = (sort { ($count_op1{$b} == $count_op1{$a}
			     ? $a cmp $b
			     : $count_op1{$b} <=> $count_op1{$a}) } keys(%count_op1));
	    @eax_sizes = (sort { ($count_eax_size{$b} == $count_eax_size{$a}
				  ? $a cmp $b
				  : $count_eax_size{$b} <=> $count_eax_size{$a}) } keys(%count_eax_size));
	    @imms = (sort { ($count_imm{$b} == $count_imm{$a}
			     ? $a cmp $b
			     : $count_imm{$b} <=> $count_imm{$a}) } keys(%count_imm));
	    
	    # make sure the attributes have at least undef in them.  for
	    # all-illegal roots they would otherwise be empty:
	    push(@op0s, "U"); $count_op0{"U"}++;
	    push(@op1s, "U"); $count_op1{"U"}++;
	    push(@eax_sizes, "U"); $count_eax_size{"U"}++;
	    push(@imms, "U.U"); $count_imm{"U.U"}++;
	    
	    # display the attributes:
	    if (1 && $debug) {
		print STDERR "root $root:";
		print STDERR "\n  funcs"; foreach (@funcs) { print STDERR " $_ ($count_func{$_})"}
		print STDERR "\n  op0s"; foreach (@op0s) { print STDERR " $_ ($count_op0{$_})"}
		print STDERR "\n  op1s"; foreach (@op1s) { print STDERR " $_ ($count_op1{$_})"}
		print STDERR "\n  eax_sizes"; foreach (@eax_sizes) { print STDERR " $_ ($count_eax_size{$_})"}
		print STDERR "\n  imms"; foreach (@imms) { print STDERR " $_ ($count_imm{$_})"}
		print STDERR "\n";
	    }

	    # form the submap key.  this is a very long string
	    # describing what a submap must do with attributes in
	    # order to be associated with this root.  this string is
	    # very long because it lists all attributes for the 64
	    # subs under this root:
	    #
	    # this string really ends up being a regular expression,
	    # and since the decoder specially cancels all attributes
	    # when the function is "illegal", illegal can really take
	    # any attributes, so wildcards are used:
	    #
	    @key = ();
	    for ($sub = 0, $pattern = $root << 6; $sub < 64 ; $sub++, $pattern++) {
		if ($map_func[$pattern] eq "illegal") {
		    push(@key, '\S+');
		    push(@key, '\S+');
		    push(@key, '\S+');
		    push(@key, '\S+');
		}
		else {

		    # the op0 and op1 attributes can be anything for
		    # patterns that don't use them ("U" for
		    # undefined), because even if they are defined in
		    # the submap or root, the instruction decoder does
		    # nothing except pass them to the instruction
		    # functions, which don't care.  so they can be
		    # anything in the submap key:
		    $_ = $map_op0[$pattern]; push(@key, ($_ eq "U" ? '\S+' : $_ eq $op0s[0] ? "U" : $_));
		    $_ = $map_op1[$pattern]; push(@key, ($_ eq "U" ? '\S+' : $_ eq $op1s[0] ? "U" : $_));

		    # however, the same is not true for the EA size
		    # and immediate operand attributes.  if a pattern
		    # doesn't use these attributes, they *must* be
		    # undefined in the submap, so that any defined
		    # attribute in the root is ignored - otherwise the
		    # instruction decoder will do EA work and fetch
		    # immediates when it isn't supposed to.  this
		    # means that we have to introduce a
		    # use-the-root-attribute value for those patterns
		    # that *must* use the root attribute value.  we
		    # use "X" to stand for this value:
		    $_ = $map_eax_size[$pattern]; push(@key, ($_ eq $eax_sizes[0] ? "X" : $_));
		    $_ = $map_imm_operand[$pattern].".".$map_imm_size[$pattern]; push(@key, ($_ eq $imms[0] ? "X" : $_));
		}
	    }
	    $key = join(" ", @key);
	    $root_key[$root] = $key;
	    $root_funcs[$root] = join(",", @funcs);
	    $root_op0[$root] = $op0s[0];
	    $root_op1[$root] = $op1s[0];
	    $root_eax_size[$root] = $eax_sizes[0];
	    $imms[0] =~ /^(.*)\.(.*)$/;
	    $root_imm_operand[$root] = $1;
	    $root_imm_size[$root] = $2;
	    undef($root_submap[$root]);
	    print STDERR "  key $key\n" if (0 && $debug);
	}
	print STDERR " done\n";

	# create submaps and opcode maps for the roots:
	$submaps_new = $submap_next;
	print STDERR "$PROG: creating $cpu_name submaps...";
	for ($root = 0; $root < 1024; $root++) {
    
	    # get the function array and key for this root:
	    @funcs = split(/,/, $root_funcs[$root]);
	    $key = $root_key[$root];
	    print STDERR "  root $root key $key\n" if (0 && $debug);
    
	    # calculate the additional memory cost of creating a new
	    # submap for this root:
	    $submap_best_cost = ($sizeof_submap
				 # our opcode map:
				 + (@funcs * $sizeof_opcode));
    
	    # check all of the existing submaps for a key match:
	    undef($submap_best);
	    $key =~ s/\./\\\./g;
	    for ($submap = 0; $submap < $submap_next; $submap++) {
		next unless ($submap_key[$submap] =~ /^$key$/);
	
		# get the opcode map indices array, and the size of
		# the opcode map currently required by this submap:
		@opcode_map_indices = split(/,/, $submap_opcode_map_indices[$submap]);
		$opcode_map_size = $submap_opcode_map_size[$submap];
	
		# loop over the submap entries, seeing how the submap's opcode
		# map indices would have to change to accomodate this root:
		undef(@index_to_func);
		undef(%func_to_new_index);
		for ($sub = 0, $pattern = $root << 6; $sub < 64 ; $sub++, $pattern++) {
		    $opcode_map_index = $opcode_map_indices[$sub];
		    $func = $index_to_func[$opcode_map_index];
		    if (!defined($func)) {
			$index_to_func[$opcode_map_index] = $map_func[$pattern];
		    }
		    elsif ($func ne $map_func[$pattern]) {
			if (!defined($_ = $func_to_new_index{$map_func[$pattern]})) {
			    $_ = $func_to_new_index{$func} = $opcode_map_size++;
			    $index_to_func[$_] = $func;
			}
			$opcode_map_indices[$sub] = $_;
		    }
		}

		# a submap for an earlier CPU is off-limits unless we
		# can use it without growing the opcode map it
		# requires, since both the submap and the earlier
		# CPU's opcode maps have already been generated:
		next if ($submap < $submaps_new
			 && $opcode_map_size > $submap_opcode_map_size[$submap]);
	
		# calculate the additional memory cost of reusing this
		# submap for this root:
		$submap_cost = 0;

		# if the exact opcode map we would need doesn't
		# already exist, we would have to create it:
		$opcode_map_key = join(" ", @index_to_func);
		$submap_cost += ($sizeof_opcode
				 * $opcode_map_size)
		    if (!defined($opcode_map{$opcode_map_key}));

		# if we would grow the size of the opcode map required
		# by this submap by N entries, that's N more opcodes
		# for *each* root already using this submap:
		$submap_cost += ($sizeof_opcode
				 * ($submap_refcnt[$submap]
				    * ($opcode_map_size
				       - $submap_opcode_map_size[$submap])));

		# if using this submap for this root is still better
		# than any alternative, remember it and how we may
		# want to change it:
		if ($submap_cost < $submap_best_cost) {
		    $submap_best = $submap;
		    $submap_best_cost = $submap_cost;
		    @submap_best_opcode_map_indices = @opcode_map_indices;
		    $submap_best_opcode_map_size = $opcode_map_size;
		}
	    }
    
	    # if there is a best existing submap, make changes to it:
	    if (defined($submap_best)) {
		if ($submap_opcode_map_size[$submap_best] != $submap_best_opcode_map_size) {
		    $submap_opcode_map_indices[$submap_best] = join(",", @submap_best_opcode_map_indices);
		    $submap_opcode_map_size[$submap_best] = $submap_best_opcode_map_size;
		    print STDERR "  submap $submap_best now funcs ".join(",", @funcs).", ($opcode_map_size) indices ".join(",", @opcode_map_indices)."\n"
			if ($debug);
		}
	    }
    
	    # otherwise, create a new submap:
	    else {
	
		# allocate a new submap number:
		$submap_best = $submap_next++;
	
		# create the hash of function name to opcode map index:
		undef(%func_to_index);
		$opcode_map_size = 0;
		foreach $func (@funcs) {
		    $func_to_index{$func} = $opcode_map_size++;
		}
	
		# now make the list of opcode map indices:
		@opcode_map_indices = ();
		for ($sub = 0, $pattern = $root << 6; $sub < 64 ; $sub++, $pattern++) {
		    die "internal error - func $map_func[$pattern] missing from ".join(" ", @funcs)."\n"
			if (!defined($func_to_index{$map_func[$pattern]}));
		    push(@opcode_map_indices, $func_to_index{$map_func[$pattern]});
		}
	
		# officially add this new submap:
		$submap_opcode_map_indices[$submap_best] = join(",", @opcode_map_indices);
		$submap_opcode_map_size[$submap_best] = $opcode_map_size;
		$submap_key[$submap_best] = $root_key[$root];
		print STDERR "  new submap $submap_best, funcs ".join(",", @funcs).", ($opcode_map_size) indices ".join(",", @opcode_map_indices)."\n"
		    if ($debug);
	    }
    
	    # note how reference counts have changed:
	    $root_submap[$root] = $submap_best;
	    $submap_refcnt[$submap_best]++;
	    print STDERR "  refcnt on submap $submap_best now $submap_refcnt[$submap_best]\n" if ($debug);	    
	}

	# write out any newly created submaps:
	$submaps = 0;
	for ($submap = $submaps_new; $submap < $submap_next; $submap++) {
	    next if ($submap_refcnt[$submap] == 0);
	    $submaps++;

	    print <<"EOF;";

/* create submap $submap: */
static struct _tme_m68k_decoder_submap *
_tme_m68k_create_submap$submap(struct tme_m68k *ic)
{
  struct _tme_m68k_decoder_submap *submap, *submap_entry;
  int entry_i;

  /* allocate and initialize the submap: */
  submap = tme_new(struct _tme_m68k_decoder_submap, 64);
  submap_entry = submap;
  for (entry_i = 0; entry_i < 64; entry_i++) {
      submap_entry->_tme_m68k_decoder_submap_gen._tme_m68k_decoder_gen_operand0 = NULL;
      submap_entry->_tme_m68k_decoder_submap_gen._tme_m68k_decoder_gen_operand1 = NULL;
      submap_entry->_tme_m68k_decoder_submap_gen._tme_m68k_decoder_gen_eax_size = TME_M68K_SIZE_UNDEF;
      submap_entry->_tme_m68k_decoder_submap_gen._tme_m68k_decoder_gen_imm_operand = TME_M68K_OPNUM_UNDEF;
      submap_entry++;
  }

  /* fill the submap: */
  submap_entry = submap;
EOF;

	    # get the submap key and opcode map indices and split them up:
	    @key = split(' ', $submap_key[$submap]);
	    @opcode_map_indices = split(/,/, $submap_opcode_map_indices[$submap]);

	    # emit the code to initialize each submap entry:
	    for ($sub = 0; $sub < 64 ; $sub++) {
		print "\n  /* submap $submap sub $sub */\n";

		# emit op0:
		$op0 = &operand_final(shift(@key));
		print "  submap_entry->_tme_m68k_decoder_submap_gen._tme_m68k_decoder_gen_operand0 = $op0;\n"
		    if (defined($op0));

		# emit op1:
		$op1 = &operand_final(shift(@key));
		print "  submap_entry->_tme_m68k_decoder_submap_gen._tme_m68k_decoder_gen_operand1 = $op1;\n"
		    if (defined($op1));

		# emit eax_size:
		$eax_size = shift(@key);
		$eax_size = "SUBMAP_X" if ($eax_size eq "X");
		print "  submap_entry->_tme_m68k_decoder_submap_gen._tme_m68k_decoder_gen_eax_size = TME_M68K_SIZE_$eax_size;\n"
		    if ($eax_size ne "U" && $eax_size ne '\S+');

		# emit imm_size and imm_operand:
		$_ = shift(@key);
		if ($_ eq "X") {
		    print "  submap_entry->_tme_m68k_decoder_submap_gen._tme_m68k_decoder_gen_imm_operand = TME_M68K_OPNUM_SUBMAP_X;\n";
		}
		elsif ($_ !~ /^U/ && $_ ne '\S+') {
		    ($imm_operand, $imm_size) = (/^(\S+)\.(\S+)/);
		    $imm_size = "16U8" if ($imm_size eq "8");
		    print "  submap_entry->_tme_m68k_decoder_submap_gen._tme_m68k_decoder_gen_imm_operand = $imm_operand;\n";
		    print "  submap_entry->_tme_m68k_decoder_submap_gen._tme_m68k_decoder_gen_imm_size = TME_M68K_SIZE_$imm_size;\n";
		}
		
		# emit the opcode map index:
		print "  (submap_entry++)->_tme_m68k_decoder_submap_opcode_map_index = $opcode_map_indices[$sub];\n";
	    }

	    # finish the function:
	    print <<"EOF;";

  /* done: */
  return(submap);
}
EOF;
	}

	# for each root, according to the submap used, either use an
	# existing opcode map or create a new opcode map:
	$opcode_maps = 0;
	for ($root = 0; $root < 1024; $root++) {

	    # create the opcode map key:
	    $submap = $root_submap[$root];
	    @opcode_map_indices = split(/,/, $submap_opcode_map_indices[$submap]);
	    $opcode_map_size = $submap_opcode_map_size[$submap];
	    undef(@index_to_func);
	    for ($sub = 0, $pattern = $root << 6; $sub < 64 ; $sub++, $pattern++) {
		$opcode_map_index = $opcode_map_indices[$sub];
		$func = $index_to_func[$opcode_map_index];
		if (!defined($func)) {
		    $index_to_func[$opcode_map_index] = $map_func[$pattern];
		}
		elsif ($func ne $map_func[$pattern]) {
		    die "$PROG internal error: opcode map indices broken (root $root, submap $submap, sub $sub index $opcode_map_index needs to be $map_func[$pattern], but it's already $func)\n";
		}
	    }
	    $opcode_map_key = join(" ", @index_to_func);

	    # if the exact opcode map we need doesn't already exist,
	    # create it:
	    if (!defined($opcode_map = $opcode_maps{$opcode_map_key})) {
		$opcode_map = $opcode_map_next++;
		$opcode_maps{$opcode_map_key} = $opcode_map;
		$opcode_maps++;
    
		print "\n";
		print "/* opcode map for $cpu_name root $root (submap $submap): */\n";
		print "static const struct _tme_m68k_opcode _tme_m68k_opcode_map${opcode_map}[$opcode_map_size] = {\n";
		for ($map_i = 0; $map_i < $opcode_map_size; $map_i++) {
		    @parts = split(/\./, $index_to_func[$map_i]);
		    ($func, $specop, $cycles) = @parts;
		    if ($func =~ /^(\S+)(nonmemtomem)(\d+)/
			|| $func =~ /^(\S+)(memtomem)(\d+)/) {
			$func = $1.$3;
			$specop = $1.$2;
		    }
		    $specop = "undef" 
			if (!defined($specop)
			    || $specop eq "un");
		    $specop =~ tr/a-z/A-Z/;
		    $specop = "TME_M68K_SPECOP_$specop";
		    if (!defined($cycles)
			|| $cycles eq "un"
			|| $cycles eq "any") {
			$cycles = "TME_BUS_CYCLE_UNDEF";
		    }
		    elsif ($cycles eq "ro") {
			$cycles = "TME_BUS_CYCLE_READ";
		    }
		    elsif ($cycles eq "wo") {
			$cycles = "TME_BUS_CYCLE_WRITE";
		    }
		    elsif ($cycles eq "rw") {
			$cycles = "TME_BUS_CYCLE_READ|TME_BUS_CYCLE_WRITE";
		    }
		    else {
			die "$PROG error: bad cycles $cycles\n";
		    }
		    print "  { tme_m68k_$func, $specop, $cycles },\n";
		}
		print "};\n";
	    }
	    $root_opcode_map[$root] = $opcode_map;
	}
	print STDERR " created $submaps new submaps, $opcode_maps new opcode maps\n";

	# write out the function that creates the root map:
	print <<"EOF;";

/* initializes the ${cpu_name} decoder map: */
void
_tme_${cpu_name}_decoder_map_init(struct tme_m68k *ic)
{
  struct _tme_m68k_decoder_root *root, *root_entry;
  int entry_i;
EOF;

	print "\n";
	print "  /* the submaps used by this CPU: */\n";
	undef(@submap_done);
	for ($root = 0; $root < 1024; $root++) {
	    $submap = $root_submap[$root];
	    if (!$submap_done[$submap]) {
		print "  struct _tme_m68k_decoder_submap *submap$submap = _tme_m68k_create_submap$submap(ic);\n";
		$submap_done[$submap] = 1;
	    }
	}

	print <<'EOF;';

  /* allocate and initialize the root map: */
  root = tme_new(struct _tme_m68k_decoder_root, 1024);
  ic->_tme_m68k_decoder_root = root;
  root_entry = root;
  for (entry_i = 0; entry_i < 1024; entry_i++) {
      root_entry->_tme_m68k_decoder_root_gen._tme_m68k_decoder_gen_operand0 = NULL;
      root_entry->_tme_m68k_decoder_root_gen._tme_m68k_decoder_gen_operand1 = NULL;
      root_entry->_tme_m68k_decoder_root_gen._tme_m68k_decoder_gen_eax_size = TME_M68K_SIZE_UNDEF;
      root_entry->_tme_m68k_decoder_root_gen._tme_m68k_decoder_gen_imm_operand = TME_M68K_OPNUM_UNDEF;
      root_entry++;
  }
  root_entry = root;
EOF;

	print "\n";
	print "  /* fill the root map: */\n";
	for ($root = 0; $root < 1024; $root++) {
	    print "\n  /* root $root (base opcode ".sprintf("0x%04x", ($root << 6))."): */\n";
    
	    # emit op0:
	    $op0 = &operand_final($root_op0[$root]);
	    print "  root_entry->_tme_m68k_decoder_root_gen._tme_m68k_decoder_gen_operand0 = $op0;\n"
		if (defined($op0));
	    
	    # emit op1:
	    $op1 = &operand_final($root_op1[$root]);
	    print "  root_entry->_tme_m68k_decoder_root_gen._tme_m68k_decoder_gen_operand1 = $op1;\n"
		if (defined($op1));
	    
	    # emit eax_size:
	    $eax_size = $root_eax_size[$root];
	    print "  root_entry->_tme_m68k_decoder_root_gen._tme_m68k_decoder_gen_eax_size = TME_M68K_SIZE_$eax_size;\n"
		if ($eax_size ne "U");
	    
	    # emit imm_size and imm_operand:
	    $imm_operand = $root_imm_operand[$root];
	    if ($imm_operand ne "U" && $imm_operand ne '\S+') {
		$imm_size = $root_imm_size[$root];
		$imm_size = "16U8" if ($imm_size eq "8");
		print "  root_entry->_tme_m68k_decoder_root_gen._tme_m68k_decoder_gen_imm_operand = $imm_operand;\n";
		print "  root_entry->_tme_m68k_decoder_root_gen._tme_m68k_decoder_gen_imm_size = TME_M68K_SIZE_$imm_size;\n";
	    }
	    
	    # emit the submap and opcode map:
	    print "  root_entry->_tme_m68k_decoder_root_submap = submap$root_submap[$root];\n";
	    print "  (root_entry++)->_tme_m68k_decoder_root_opcode_map = _tme_m68k_opcode_map$root_opcode_map[$root];\n";
	}

	# close the function:
	print "\n}\n";
    }

    # anything else is an error:
    else {
	print STDERR "stdin:$line $PROG error: don't know how to handle: ".join(" ", @tokens)."\n";
	exit(1);
    }
}

# done:
exit(0);

# this subroutine returns the final C version of an operand:
sub operand_final {
    local ($op) = @_;

    if ($op eq "U" || $op eq '\S+') {
	undef($op);
    }
    elsif ($op =~ /^\%([ad][0-7])\.(\d+)/) {
	($op, $op_size) = ($1, $2);
	$op =~ tr/a-z/A-Z/;
	if ($op_size == 16) {
	    $op .= " << 1";
	}
	elsif ($op_size == 8) {
	    $op .= " << 2";
	}
	$op = "&ic->tme_m68k_ireg_uint${op_size}(TME_M68K_IREG_${op})";
    }
    elsif ($op eq "eax.32") {
	$op = "&ic->_tme_m68k_ea_address";
    }
    elsif ($op =~ /^imm(\d+)\.(\d+)/) {
	$op = "(tme_uint${2}_t *) &_tme_m68k_imm${2}[${1}]";
    }
    elsif ($op =~ /^mem[xy]\.(\d+)/) {
	$op = "&ic->tme_m68k_ireg_memx${1}";
    }
    else {
	die "$PROG fatal: unknown operand $op\n";
    }
    $op;
}	
