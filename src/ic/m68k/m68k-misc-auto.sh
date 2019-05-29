#! /bin/sh

# $Id: m68k-misc-auto.sh,v 1.7 2003/05/10 15:19:00 fredette Exp $

# ic/m68k/m68k-misc-auto.sh - automatically generates C code 
# for miscellaneous m68k emulation support:

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

header=false

for option
do
    case $option in
    --header) header=true ;;
    esac
done

PROG=`basename $0`
cat <<EOF
/* automatically generated by $PROG, do not edit! */
EOF

# we need our own inclusion protection, since the instruction word
# fetch macros need to be multiply included:
if $header; then
    echo ""
    echo "#ifndef _IC_M68K_MISC_H"
    echo "#define _IC_M68K_MISC_H"
fi

# emit the register mapping macros:
if $header; then

    echo ""
    echo "/* the register mapping: */"
    echo "#define TME_M68K_IREG_UNDEF		(-1)"
    ireg32_next=0
    
    # NB: these are in a deliberate order, matching the order of
    # registers in instruction encodings:
    for regtype in d a; do
	capregtype=`echo ${regtype} | tr a-z A-Z`
	for regnum in 0 1 2 3 4 5 6 7; do
	    echo "#define TME_M68K_IREG_${capregtype}${regnum}		(${ireg32_next})"
	    echo "#define tme_m68k_ireg_${regtype}${regnum}		tme_m68k_ireg_uint32(TME_M68K_IREG_${capregtype}${regnum})"
	    ireg32_next=`expr ${ireg32_next} + 1`
	done
    done

    # the current and next program counter:
    echo "#define TME_M68K_IREG_PC		(${ireg32_next})"
    echo "#define tme_m68k_ireg_pc		tme_m68k_ireg_uint32(TME_M68K_IREG_PC)"
    ireg32_next=`expr ${ireg32_next} + 1`
    echo "#define TME_M68K_IREG_PC_NEXT		(${ireg32_next})"
    echo "#define tme_m68k_ireg_pc_next		tme_m68k_ireg_uint32(TME_M68K_IREG_PC_NEXT)"
    ireg32_next=`expr ${ireg32_next} + 1`
    
    # the status register and ccr:
    echo "#define tme_m68k_ireg_sr		tme_m68k_ireg_uint16(${ireg32_next} << 1)"
    echo "#define tme_m68k_ireg_ccr		tme_m68k_ireg_uint8(${ireg32_next} << 2)"
    ireg32_next=`expr ${ireg32_next} + 1`

    # the shadow status register and format/offset word:
    echo "#define TME_M68K_IREG_SHADOW_SR	(${ireg32_next} << 1)"
    echo "#define tme_m68k_ireg_shadow_sr	tme_m68k_ireg_uint16(TME_M68K_IREG_SHADOW_SR)"
    echo "#define TME_M68K_IREG_FORMAT_OFFSET	((${ireg32_next} << 1) + 1)"
    echo "#define tme_m68k_ireg_format_offset	tme_m68k_ireg_uint16(TME_M68K_IREG_FORMAT_OFFSET)"
    ireg32_next=`expr ${ireg32_next} + 1`

    # the memory buffers:
    for mem_which in x y z; do
	cap_mem_which=`echo ${mem_which} | tr a-z A-Z`
	echo "#define TME_M68K_IREG_MEM${cap_mem_which}32		(${ireg32_next})"
	echo "#define tme_m68k_ireg_mem${mem_which}32		tme_m68k_ireg_uint32(TME_M68K_IREG_MEM${cap_mem_which}32)"
	echo "#define TME_M68K_IREG_MEM${cap_mem_which}16		(${ireg32_next} << 1)"
	echo "#define tme_m68k_ireg_mem${mem_which}16		tme_m68k_ireg_uint16(TME_M68K_IREG_MEM${cap_mem_which}16)"
	echo "#define TME_M68K_IREG_MEM${cap_mem_which}8		(${ireg32_next} << 2)"
	echo "#define tme_m68k_ireg_mem${mem_which}8		tme_m68k_ireg_uint8(TME_M68K_IREG_MEM${cap_mem_which}8)"
	ireg32_next=`expr ${ireg32_next} + 1`
    done

    # the control registers:
    for reg in usp isp msp sfc dfc vbr; do
	capreg=`echo $reg | tr a-z A-Z`
	echo "#define TME_M68K_IREG_${capreg}		(${ireg32_next})"
	echo "#define tme_m68k_ireg_${reg}		tme_m68k_ireg_uint32(TME_M68K_IREG_${capreg})"
	ireg32_next=`expr ${ireg32_next} + 1`
    done

    echo "#define TME_M68K_IREG32_COUNT		(${ireg32_next})"
fi

# emit the flags->conditions mapping.  note that the nesting of the
# flag variables is deliberate, to make this array indexable with the
# condition code register:
if $header; then :; else
    echo ""
    echo "/* the flags->conditions mapping: */"
    echo "const tme_uint16_t _tme_m68k_conditions[32] = {"
    for xflag in 0 1; do
	for nflag in 0 1; do
	    for zflag in 0 1; do
		for vflag in 0 1; do
		    for cflag in 0 1; do
		    
			# the True condition:
			echo -n "TME_BIT(TME_M68K_C_T)"
			
			# the High condition:
			if test $cflag != 1 && test $zflag != 1; then
			    echo -n " | TME_BIT(TME_M68K_C_HI)"
			fi
			
			# the Low or Same condition:
			if test $cflag = 1 || test $zflag = 1; then
			    echo -n " | TME_BIT(TME_M68K_C_LS)"
			fi
			
			# the Carry Clear and Carry Set conditions:
			if test $cflag != 1; then
			    echo -n " | TME_BIT(TME_M68K_C_CC)"
			else
			    echo -n " | TME_BIT(TME_M68K_C_CS)"
			fi
			
			# the Not Equal and Equal conditions:
			if test $zflag != 1; then
			    echo -n " | TME_BIT(TME_M68K_C_NE)"
			else
			    echo -n " | TME_BIT(TME_M68K_C_EQ)"
			fi
			
			# the Overflow Clear and Overflow Set conditions:
			if test $vflag != 1; then
			    echo -n " | TME_BIT(TME_M68K_C_VC)"
			else
			    echo -n " | TME_BIT(TME_M68K_C_VS)"
			fi
			
			# the Plus and Minus conditions:
			if test $nflag != 1; then
			    echo -n " | TME_BIT(TME_M68K_C_PL)"
			else
			    echo -n " | TME_BIT(TME_M68K_C_MI)"
			fi
			
			# the Greater or Equal condition:
			if (test $nflag = 1 && test $vflag = 1) || \
			   (test $nflag != 1 && test $vflag != 1); then
			    echo -n " | TME_BIT(TME_M68K_C_GE)"
			fi
			
			# the Less Than condition:
			if (test $nflag = 1 && test $vflag != 1) || \
			   (test $nflag != 1 && test $vflag = 1); then
			    echo -n " | TME_BIT(TME_M68K_C_LT)"
			fi

			# the Greater Than condition:
			if (test $nflag = 1 && test $vflag = 1 && test $zflag != 1) || \
			   (test $nflag != 1 && test $vflag != 1 && test $zflag != 1); then
			    echo -n " | TME_BIT(TME_M68K_C_GT)"
			fi
		    
			# the Less Than or Equal condition:
			if test $zflag = 1 || \
			   (test $nflag = 1 && test $vflag != 1) || \
			   (test $nflag != 1 && test $vflag = 1); then
			    echo -n " | TME_BIT(TME_M68K_C_LE)"
			fi

			echo ","
		    done
		done
	    done
	done
    done
    echo "};"
fi

# emit the instruction word fetch macros:
if $header; then

    echo ""
    echo "#endif /* _IC_M68K_MISC_H */"

    # permute for the fast vs. slow executors:
    for executor in fast slow; do

	echo ""
	echo -n "#if"
	if test $executor = slow; then echo -n "n"; fi
	echo "def _TME_M68K_EXECUTE_FAST"
	echo ""
	echo "/* these macros are for the ${executor} executor: */"

	# permute for any-alignment vs. strict-alignment:
	for alignment in any strict; do

	    # permute for the two different sizes we need to handle:
	    for size in 16 32; do

		# permute for big-endian vs. little-endian:
		for endian in little big; do

		    # permute for signed or unsigned:
		    for capsign in U S; do
			if test $capsign = U; then sign=u ; un=un ; else sign= ; un= ; fi

			# the slow executor has only one possible
			# version of each macro, no matter what the
			# endianness or alignment or atomic requirements
			# of the host, since the tme_m68k_fetch${size}
			# functions take care of all of that:
			if test $executor = slow; then
			    if test $endian = big && test $alignment = any; then
				echo ""
				echo "/* on all hosts, this fetches a ${size}-bit ${un}signed value for the slow executor: */"
				echo "#undef _TME_M68K_EXECUTE_FETCH_${capsign}${size}"
				echo "#define _TME_M68K_EXECUTE_FETCH_${capsign}${size}(v) \\"
				echo "  (v) = (tme_${sign}int${size}_t) tme_m68k_fetch${size}(ic, linear_pc); \\"
				if test ${size} = 16; then
				    echo "  insn_fetch_sizes <<= 1; \\"
				else
				    echo "  insn_fetch_sizes = (insn_fetch_sizes << 1) | 1; \\"
				fi
				echo "  linear_pc += sizeof(tme_${sign}int${size}_t)"
			    fi
			    continue
			fi

			# assume we'll be universal:
			macro_comment="ll hosts"
			macro_test=

			# if this is a 16-bit fetch:
			if test $size = 16; then

			    # we don't need a strict alignment version, since
			    # we're guaranteed to be reading from emulator
			    # addresses that are 16-bit aligned - for the fast
			    # executor, tme_m68k_go_slow guarantees that 
			    # emulator_load is 16-bit aligned:
			    if test $alignment = strict; then continue; fi

			    # we also don't need a little-endian version, since
			    # the fast macros we emit will always use tme_betoh_u16:
			    if test $endian = little; then continue; fi

			# if this is a 32-bit fetch:
			else

			    # while the emulator address we'll be reading
			    # from is 16-bit aligned as explained above,
			    # we still need a strict-alignment version of
			    # the 32-bit fetcher on hosts that require 32-bit
			    # values to be more aligned than 16-bit values.
			    #
			    # on a host that does not have this further
			    # requirement, we also don't need a little-endian
			    # version, since the fast macros we emit will always
			    # use tme_betoh_u32:
			    macro_comment=" host with ${alignment} alignment"
			    if test $alignment = strict; then
				macro_comment=" ${endian}-endian${macro_comment}"
				if test ${endian} = little; then macro_test="!"; fi
				macro_test="(ALIGNOF_INT${size}_T > ALIGNOF_INT16_T) && ${macro_test}defined(WORDS_BIGENDIAN)"
			    else
				macro_test="ALIGNOF_INT${size}_T <= ALIGNOF_INT16_T"
				if test ${endian} = little; then continue; fi
			    fi
			fi
				
			# open the macro:
			echo ""
			echo "/* on a${macro_comment}, "
			echo "   this loads a ${size}-bit ${un}signed value for the ${executor} instruction executor: */"
			if test "x${macro_test}" != x; then echo "#if ${macro_test}"; fi
			echo "#undef _TME_M68K_EXECUTE_FETCH_${capsign}${size}"
			echo "#define _TME_M68K_EXECUTE_FETCH_${capsign}${size}(v) \\"

			# assume we'll be converting:
			conv="tme_betoh_u${size}"

			# prepare the buffer to read out of:
			if test ${executor} = fast; then

			    # if we're doing a 32-bit read and the
			    # emulator address is not 32-bit aligned,
			    # on a strict-alignment host we will have
			    # to do a sequence of two 16-bit memory
			    # reads:
			    buffer="emulator_load"
			    misaligned="((unsigned long) emulator_load) & (sizeof(tme_uint32_t) - 1)"
			    update="emulator_load += sizeof(tme_${sign}int${size}_t)"

			    # if we can't do the fast read, bail:
			    echo "  if ((emulator_load + (sizeof(tme_uint${size}_t) - 1)) > emulator_load_last) \\"
			    echo "    goto _tme_m68k_fast_fetch_failed; \\"
			fi

			single="(v) = (tme_${sign}int${size}_t) ${conv}(*((tme_uint${size}_t *) ${buffer}));"

			# if this is a 16-bit read, the host can always 
			# do a simple assignment.  
			#
		        # we need the rdlock if we're on an architecture
			# where an aligned access may not be atomic:
			if test $size = 16; then
			    echo "  tme_memory_aligned_rdlock(tlb->tme_m68k_tlb_bus_rwlock); \\"
			    echo "  ${single} \\"
			    echo "  tme_memory_aligned_unlock(tlb->tme_m68k_tlb_bus_rwlock); \\"

			# if this is a 32-bit read on an any-alignment host,
			# do a simple assignment.
			#
			# we need the rdlock if this is an aligned access and
			# we're on an architecture where an aligned access may
			# not be atomic, or if this is an unaligned access and
			# we're on an architecture where an unaligned access
			# may not be atomic:
			elif test $alignment = any; then
			    echo "  if (${misaligned}) { \\"
			    echo "    tme_memory_unaligned_rdlock(tlb->tme_m68k_tlb_bus_rwlock); \\"
			    echo "    ${single} \\"
			    echo "    tme_memory_unaligned_unlock(tlb->tme_m68k_tlb_bus_rwlock); \\"
			    echo "  } \\"
			    echo "  else { \\"
			    echo "    tme_memory_aligned_rdlock(tlb->tme_m68k_tlb_bus_rwlock); \\"
			    echo "    ${single} \\"
			    echo "    tme_memory_aligned_unlock(tlb->tme_m68k_tlb_bus_rwlock); \\"
			    echo "  } \\"

			# otherwise, this is a 32-bit read on a strict-alignment
			# host.  
			#
			# if doing a sequence access is not costlier than deciding 
			# whether to do a sequence or aligned access and then doing
			# the chosen access, or if the buffer is misaligned, 
			# acquire the rdlock for a sequence and do the sequence
			# of reads, else acquire the rdlock for an aligned access
			# and do the single read:
			else
			    echo "  if (TME_SEQUENCE_ACCESS_NOT_COSTLIER || ${misaligned}) { \\";
			    if test $endian = little; then
				word_lo=0 ; word_hi=1 ; 
			    else
				word_lo=1 ; word_hi=0 ; conv=
			    fi
			    echo "    tme_memory_sequence_rdlock(tlb->tme_m68k_bus_tlb_rwlock); \\"
			    echo "    (v) = (tme_${sign}int${size}_t) \\"
			    echo "      ${conv}((((tme_uint32_t) ((tme_uint16_t *) ${buffer})[${word_hi}]) << 16) | \\"
			    echo "              ((tme_uint32_t) ((tme_uint16_t *) ${buffer})[${word_lo}])); \\"
			    echo "    tme_memory_sequence_unlock(tlb->tme_m68k_bus_tlb_rwlock); \\"
			    echo "  } else { \\"
			    echo "    tme_memory_aligned_rdlock(tlb->tme_m68k_tlb_bus_rwlock); \\"
			    echo "    ${single} \\"
			    echo "    tme_memory_aligned_unlock(tlb->tme_m68k_tlb_bus_rwlock); \\"
			    echo "  } \\"
			fi

			# remember if we did a 16-bit or 32-bit fetch, and update:
			if test ${size} = 16; then
			    echo "  insn_fetch_sizes <<= 1; \\"
			else
			    echo "  insn_fetch_sizes = (insn_fetch_sizes << 1) | 1; \\"
			fi
			echo "  ${update}"

			# close the conditional:
			if test "x${macro_test}" != x; then echo "#endif /* ${macro_test} */"; fi
		    done
		done
	    done
	done

	echo ""
	echo -n "#endif /* "
	if test $executor = slow; then echo -n "!"; fi
	echo "_TME_M68K_EXECUTE_FAST */"
    done
fi

# done:
exit 0
