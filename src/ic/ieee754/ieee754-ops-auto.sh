#! /bin/sh

# $Id: ieee754-ops-auto.sh,v 1.2 2005/03/23 11:47:37 fredette Exp $

# ic/ieee754/ieee754-misc-auto.sh - automatically generates C code 
# for IEEE 754 emulation operations:

#
# Copyright (c) 2004 Matt Fredette
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
#include <tme/common.h>
_TME_RCSID("\$Id: ieee754-ops-auto.sh,v 1.2 2005/03/23 11:47:37 fredette Exp $");

EOF
if $header; then 
:
else
    cat <<EOF
#include "softfloat-tme.h"
#include <math.h>
EOF
fi

# the precision information helper script:
#
ieee754_precision_sh=`echo $0 | sed -e "s/$PROG/ieee754-precision.sh/"`

# the different compliance levels:
#
levels="strict partial unknown"

# permute for the different compliance levels:
#
for level in ${levels}; do

    # permute for functions or for a set:
    #
    for what in funcs set; do

	# if we're generating headers:
	#
	if $header; then

	    # if we're doing the strict-level functions, start the
	    # operations struct type:
	    #
	    if test "${level}-${what}" = strict-funcs; then
		echo "/* the IEEE 754 operations: */"
		echo "struct tme_ieee754_ops {"
		echo ""
		echo "  /* the version of this structure: */"
		echo "  tme_uint32_t tme_ieee754_ops_version;"
	      

	    # otherwise, if we're doing a set, just declare the set
	    # and continue:
	    #
	    elif test ${what} = set; then
		echo ""
		echo "/* the ${level} compliance operations: */"
		echo "extern _tme_const struct tme_ieee754_ops tme_ieee754_ops_${level};"
		continue
	    fi

	# otherwise, if we're doing a set:
	#
	elif test ${what} = set; then

	    # start the operations set for this level:
	    #
	    echo ""
	    echo "/* the ${level} compliance operations: */"
	    echo "_tme_const struct tme_ieee754_ops tme_ieee754_ops_${level} = {"
	    echo ""
	    echo "  /* the version of this structure: */"
	    echo "  TME_X_VERSION(0, 0),"
	fi

	# permute for the different precisions:
	#
	for precision in single double extended80 quad; do

	    # get information about this precision:
	    #
	    eval `sh ${ieee754_precision_sh} ${precision}`

	    # generate the operations:
	    #
	    for name in add sub mul div \
			rem sqrt abs neg move \
			rint \
			cos acos cosh \
			sin asin sinh \
			tan atan tanh atanh \
			exp expm1 log10 log log1p \
			getexp getman scale \
			pow \
			from_single from_double from_extended80 from_quad \
			to_int32 to_int64 \
			; do

		# get the characteristics of this operation that are the
		# same at all compliance levels:
		#
		monadic=true
		case "${name}" in
		add | sub | mul | div | rem | pow | scale)
		    monadic=false
		    ;;
		esac
		dst_type="struct tme_float"
		case "${name}" in
		to_int32) dst_type="tme_int32_t" ;;
		esac

		# we don't need a function to convert from the same
		# precision to this precision:
		#
		from_precision=
		case "${name}" in 
		from_*)
		    from_precision=`echo ${name} | sed -e 's/^from_//'`
		    if test ${from_precision} = ${precision}; then
			continue
		    fi
		    ;;
		esac

		# if we're generating headers:
		#
		if $header; then

		    # only emit this header information if we're
		    # doing the strict level functions:
		    #
		    if test "${level}-${what}" = strict-funcs; then
			echo ""
			echo "  /* this does a ${precision}-precision "`echo ${name} | tr _ -`": */"
			echo -n "  void (*tme_ieee754_ops_${precision}_${name}) _TME_P((struct tme_ieee754_ctl *, "
			if $monadic; then :; else
			    echo -n "_tme_const struct tme_float *, "
			fi
			echo "_tme_const struct tme_float *, ${dst_type} *));"
		    fi
			
		    continue
		fi

		# start with no function for this set:
		#
		func_set='  NULL,'

		# permute for the stricter compliance levels:
		#
		for level_stricter in ${levels}; do

		    # form this function name:
		    #
		    func="_tme_ieee754_${level_stricter}_${precision}_${name}"

		    # the function type is normally determined by
		    # whether or not a softfloat function, libm
		    # function, or C operator is given.  it can be
		    # overridden:
		    # 
		    type=
		    func_softfloat=
		    func_libm=
		    func_libm_has_f=true
		    op_builtin=

		    # any preprocessor condition controlling whether or not
		    # the function can be emitted is normally determined by
		    # the function type.  it can be overridden:
		    #
		    cond=

		    # assume that we will use the operands as passed
		    # in.  the precisions will later default to the
		    # precision of the result, if they are not
		    # overridden:
		    #
		    op0=src0
		    op0_precision=
		    if $monadic; then op1= ; else op1=src1 ; fi
		    op1_precision=

		    # miscellaneous attributes:
		    #
		    src0_buffer=
		    src0_precision=
		    src1_buffer=
		    src1_precision=
		    check_nan=
		    check_inf_src0=
		    enter_softfloat=
		    enter_native=

		    # characterize this operation at this compliance level:
		    #
		    case "${level_stricter}-${name}" in
		    strict-add | strict-sub | strict-mul | strict-div | strict-rem | strict-sqrt) 
			func_softfloat="${name}"
			;;
		    *-add) op_builtin='+' ;;
		    *-sub) op_builtin='-' ;;
		    *-mul) op_builtin='*' ;;
		    *-div) op_builtin='/' ;;
		    *-sqrt) func_libm=sqrt ;;
		    partial-abs | unknown-abs) func_libm=fabs ;;
		    strict-neg)	op0=0 ; func_softfloat=sub ; op1=src0 ;;
		    partial-neg | unknown-neg) op0=0 ; op_builtin='-'; op1=src0 ;;
		    strict-move) func_softfloat=add ; op1=0 ;;
		    *-move) type="${level_stricter}-move" ; src0_buffer=false ;;
		    strict-rint) func_softfloat=round_to_int ;;
		    partial-pow | unknown-pow) func_libm="${name}" ;;
		    partial-log | unknown-log) func_libm="${name}" ;;
		    partial-exp | unknown-exp) func_libm="${name}" ;;
		    partial-log10 | unknown-log10) func_libm="${name}" ;;
		    partial-scale | unknown-scale) func_libm=scalbn ;;
		    strict-getexp | strict-getman) type="${level_stricter}-${name}" ; check_nan=true ; check_inf_src0=return-nan ;;
		    strict-from_*) func_softfloat="OP0_PRECISION_SF_to_${precision_sf}" ; op0_precision="${from_precision}" ;;
		    strict-to_int32) type="${level_stricter}-${name}" ; func_softfloat="${precision_sf}_${name}" ;;
		    esac

		    # finish typing this function:
		    #
		    if test "x${type}" = x; then
			if test "x${func_softfloat}" != x; then
			    type=softfloat
			elif test "x${func_libm}" != x; then
			    type=libm
			elif test "x${op_builtin}" != x; then
			    type=builtin
			else
			    type=none
			fi
		    fi

		    # finish the preprocessor condition controlling
		    # whether or not the function can be emitted:
		    #
		    if test "x${cond}" = x; then
			case "${precision}-${level_stricter}-${type}" in

			# a function with a none type can't be emitted:
			#
			*-*-none) cond=0 ;;

			# extended80 or quad precision softfloat
			# functions are conditional on a 64-bit
			# integral type:
			#
			extended80-*-softfloat | quad-*-softfloat) cond="defined(TME_HAVE_INT64_T)" ;;

			# partial compliance functions are conditional
			# on the builtin type being an exact match for
			# the IEEE 754 type:
			#
			*-partial-*) cond="(TME_FLOAT_FORMAT_IEEE754_${capprecision}_BUILTIN == TME_FLOAT_FORMAT_IEEE754_${capprecision})" ;;

			# assume that anything else can be unconditionally emitted:
			#
			*) cond=1 ;;
			esac
		    fi

		    # finish the operands:
		    #
		    for _opn in 0 1; do
			eval "opn=\$op${_opn}"
			if test "x${opn}" = x; then
			    continue
			fi
			eval "opn_precision=\$op${_opn}_precision"
			if test "x${opn_precision}" = x; then
			    opn_precision="${precision}"
			fi
			eval `sh ${ieee754_precision_sh} ${opn_precision} opn_`
			case "${opn}" in
			src[01])
			    eval "${opn}_precision=\$opn_precision"
			    if test "x${func_softfloat}" != x || test "x${func_libm}${op_builtin}" = x; then
				eval "srcn_buffer=\$${opn}_buffer"
				if test "x${srcn_buffer}" = x; then
				    eval "${opn}_buffer=true"
				fi
				opn="tme_ieee754_${opn_precision}_value_get(${opn}, &${opn}_buffer)"
				if test "x${func_softfloat}" != x; then
				    opn="((const ${opn_precision_sf} *) ${opn})"
				fi
				opn="(*${opn})"
			    else
				opn="tme_ieee754_${opn_precision}_value_builtin_get(${opn})"
			    fi
			    ;;
			[0-9])
			    if test "x${func_softfloat}" != x; then
				opn="int32_to_${opn_precision_sf}(${opn})"
			    fi
			    ;;
			esac
			eval "op${_opn}=\$opn"
			eval "op${_opn}_precision=\$opn_precision"
			eval "op${_opn}_precision_sf=\$opn_precision_sf"
			eval "op${_opn}_integral=\$opn_integral"
		    done

		    # finish the miscellaneous attributes:
		    #
		    if test "x${src0_buffer}" = x; then
			src0_buffer=false
		    fi
		    if test "x${src1_buffer}" = x; then
			src1_buffer=false
		    fi
		    if test "x${check_nan}" = x; then
			if test "${level_stricter}" = partial; then
			    check_nan=true
			else
			    check_nan=false
			fi
		    fi
		    if test "x${enter_softfloat}" = x; then
			if test "x${func_softfloat}" != x; then
			    enter_softfloat=true
			else
			    enter_softfloat=false
			fi
		    fi
		    if test "x${enter_native}" = x; then
			if test "${level_stricter}" = partial; then
			    enter_native=true
			else
			    enter_native=false
			fi
		    fi

		    # if we're making functions, and we're at the
		    # right level to emit this function, and this
		    # function can be emitted:
		    #
		    if test ${what} = funcs && test ${level_stricter} = ${level} && test "${cond}" != 0; then

			# start any conditional:
			#
			if test "${cond}" != 1; then
			    echo ""
			    echo "#if ${cond}"
			fi
			    
			# start the function:
			#
			echo ""
			echo "/* this does a ${level} compliance ${precision}-precision "`echo ${name} | tr _ -`": */"
			echo "static void"
			echo -n "${func}(struct tme_ieee754_ctl *ieee754_ctl, const struct tme_float *src0, "
			if $monadic; then :; else
			    echo -n "const struct tme_float *src1, "
			fi
			echo "${dst_type} *dst)"
			echo "{"

			# emit locals:
			#
			if ${src0_buffer}; then
			    echo "  ${op0_integral} src0_buffer;"
			fi
			if ${src1_buffer}; then
			    echo "  ${op1_integral} src1_buffer;"
			fi
			echo "  int exceptions;"

			# check the operand(s):
			#
			if ${check_nan}; then
			    echo ""
			    echo "  /* check for a NaN operand: */"
			    if $monadic; then nanf=monadic; src1= ; else nanf=dyadic; src1=", src1"; fi
			    echo "  if (__tme_predict_false(tme_ieee754_${src0_precision}_check_nan_${nanf}(ieee754_ctl, src0${src1}, dst))) {"
			    echo "    return;"
			    echo "  }"
			fi
			if test "x${check_inf_src0}" != x; then
			    echo ""
			    echo "  /* if the operand is an infinity: */"
			    echo "  if (tme_ieee754_${precision}_is_inf(src0)) {"
			    echo ""
			    case "${check_inf_src0}" in
			    return-nan)
				echo "    /* return a NaN: */"
				echo "    dst->tme_float_format = TME_FLOAT_FORMAT_IEEE754_${capprecision};"
				echo "    dst->tme_float_value_ieee754_${precision} = ieee754_ctl->tme_ieee754_ctl_default_nan_${precision};"
				echo "    return;"
				;;
			    esac
			    echo "  }"
			fi

			# enter the operation mode:
			#
			if ${enter_softfloat}; then
			    echo ""
			    echo "  /* enter softfloat operation: */"
			    echo "  tme_mutex_lock(&tme_ieee754_global_mutex);"
			    echo "  tme_ieee754_global_ctl = ieee754_ctl;"
			    echo "  tme_ieee754_global_exceptions = 0;"
			    echo "  ieee754_ctl->tme_ieee754_ctl_lock_unlock = tme_ieee754_unlock_softfloat;"
			fi
			if ${enter_native}; then
			    echo ""
			    echo "  /* enter native floating-point operation: */"
			    echo "  tme_float_enter(ieee754_ctl->tme_ieee754_ctl_rounding_mode, tme_ieee754_exception_float, ieee754_ctl);"
			    echo "  ieee754_ctl->tme_ieee754_ctl_lock_unlock = tme_float_leave;"
			fi

			# assume that this operation raises no exceptions:
			#
			echo ""
			echo "  /* assume that this operation raises no exceptions: */"
			echo "  exceptions = 0;"

			# the operation:
			#
			echo ""
			echo "  /* the operation: */"
			case "${type}" in

			# a move operation:
			#
			*-move)
			    echo "  *dst = *src0;"
			    ;;

			# a getexp operation:
			#
			strict-getexp)
			    echo ""
			    echo "  /* if the operand is a zero, return a zero: */"
			    echo "  if (tme_ieee754_${precision}_is_zero(src0)) {"
			    echo "    tme_ieee754_${precision}_value_builtin_set(dst, TME_FLOAT_FORMAT_IEEE754_${capprecision}_BUILTIN, 0);"
			    echo "  }"
			    echo ""
			    echo "  /* otherwise, return the unbiased exponent: */"
			    echo "  else {"
			    echo "    tme_ieee754_${precision}_value_builtin_set(dst, TME_FLOAT_FORMAT_IEEE754_${capprecision}_BUILTIN, TME_FIELD_MASK_EXTRACTU(${op0}${sexp}, ${mask_exp}) - ${exp_bias});"
			    echo "  }"
			    ;;

			# a getman operation:
			#
			strict-getman)
			    echo ""
			    echo "  /* if the operand is a zero, return it: */"
			    echo "  if (tme_ieee754_${precision}_is_zero(src0)) {"
			    echo "    *dst = *src0;"
			    echo "  }"
			    echo ""
			    echo "  /* otherwise, return the operand, with its exponent set to biased zero: */"
			    echo "  else {"
			    echo "    tme_ieee754_${precision}_value_set(dst, ${op0});"
			    echo "    TME_FIELD_MASK_DEPOSITU(dst->tme_float_value_ieee754_${precision}${sexp}, ${mask_exp}, ${exp_bias});"
			    echo "  }"
			    ;;

			# a strict to-integer conversion operation:
			#
			strict-to_int32 | strict-to_int64)
			    echo "  *dst = ${precision_sf}_${name}(${op0});"
			    ;;

			# a softfloat operation:
			#
			softfloat)
			    echo "  _tme_ieee754_${precision}_value_set(dst, ${precision_sf},"
			    func_softfloat_raw="${func_softfloat}"
			    func_softfloat=`echo ${func_softfloat} | sed -e "s/OP0_PRECISION_SF/${op0_precision_sf}/g"`
			    func_softfloat=`echo ${func_softfloat} | sed -e "s/OP1_PRECISION_SF/${op0_precision_sf}/g"`
			    if test "${func_softfloat}" = "${func_softfloat_raw}"; then
				func_softfloat="${precision_sf}_${func_softfloat}"
			    fi
			    echo -n "    ${func_softfloat}(${op0}"
			    if test "x${op1}" != x; then
				echo ","
				echo -n "                ${op1}"
			    fi
			    echo "));"
			    ;;

			# a libm operation:
			#
			libm)
			    if test "x${op1}" = x; then ops="${op0}"; else ops="${op0}, ${op1}"; fi
			    # if there is a float variant of this libm function:
			    #
			    if ${func_libm_has_f}; then
				echo "#if (TME_FLOAT_FORMAT_IEEE754_${capprecision}_BUILTIN == TME_FLOAT_FORMAT_FLOAT)"
				echo "  tme_ieee754_${precision}_value_builtin_set(dst, TME_FLOAT_FORMAT_FLOAT, ${func_libm}f(${ops}));"
				echo "#else  /* (TME_FLOAT_FORMAT_IEEE754_${capprecision}_BUILTIN != TME_FLOAT_FORMAT_FLOAT) */"
			    fi
			    echo "  tme_ieee754_${precision}_value_builtin_set(dst, TME_FLOAT_FORMAT_DOUBLE, ${func_libm}(${ops}));"
			    if ${func_libm_has_f}; then
				echo "#endif /* (TME_FLOAT_FORMAT_IEEE754_${capprecision}_BUILTIN != TME_FLOAT_FORMAT_FLOAT) */"
			    fi
			    ;;

			# a builtin operation:
			#
			builtin)
			    echo "  tme_ieee754_${precision}_value_builtin_set(dst, TME_FLOAT_FORMAT_IEEE754_${capprecision}_BUILTIN, ${op0} ${op_builtin} ${op1});"
			    ;;

			*)
			    echo "$PROG internal error: don't know how to generate a ${type} type operation" 1>&2
			    exit 1
			    ;;
			esac

			# leave the operation mode:
			#
			if ${enter_native}; then
			    echo ""
			    echo "  /* leave native floating-point operation: */"
			    echo "  exceptions |= tme_float_leave();"
			fi
			if ${enter_softfloat}; then
			    echo ""
			    echo "  /* leave softfloat operation: */"
			    echo "  tme_ieee754_global_ctl = NULL;"
			    echo "  exceptions |= tme_ieee754_global_exceptions;"
			    echo "  tme_mutex_unlock(&tme_ieee754_global_mutex);"
			fi
			echo "  ieee754_ctl->tme_ieee754_ctl_lock_unlock = NULL;"

			# signal any exceptions:
			#
			echo ""
			echo "  /* signal any exceptions: */"
			echo "  if (exceptions != 0) {"
			echo "    (*ieee754_ctl->tme_ieee754_ctl_exception)(ieee754_ctl, exceptions);"
			echo "  }"

			# end the function:
			#
			echo "}"

			# close any conditional:
			#
			if test "${cond}" != 1; then
			    echo ""
			    echo "#endif /* ${cond} */"
			fi
		    fi

		    # update the function for this set:
		    #
		    case "${cond}" in
		    0) ;;
		    1) func_set="  ${func}," ;;
		    *) func_set="#if (${cond})@  ${func},@#else  /* !(${cond}) */@${func_set}@#endif /* !(${cond}) */" ;;
		    esac

		    # stop now if we just did this level:
		    #
		    if test ${level_stricter} = ${level}; then
			break
		    fi
		done
		    
		# if we're making a set:
		#
		if test ${what} = set; then
		    echo ""
		    echo "  /* this does a ${level} compliance ${precision}-precision ${name}: */"
		    echo "${func_set}" | tr '@' '\n'
		fi
			
	    done
	done

	# if we're generating headers:
	#
	if $header; then

	    # if we're doing the strict-level functions, close the
	    # operations struct type:
	    #
	    if test "${level}-${what}" = strict-funcs; then
		echo "};"
	    fi

	# otherwise, if we're doing a set:
	#
	elif test ${what} = set; then

	    # close the operations set for this level:
	    #
	    echo "};"
	fi

    done

done

# if we're not generating headers:
#
if $header; then :; else

    echo ""
    echo "/* this looks up an operations structure: */"
    echo "const struct tme_ieee754_ops *"
    echo "tme_ieee754_ops_lookup(const char *compliance)"
    echo "{"
    echo ""
    for level in ${levels}; do
	echo "  if (TME_ARG_IS(compliance, \"${level}\")) { "
	echo "    return (&tme_ieee754_ops_${level});"
	echo "  }"
    done
    echo "  return (NULL);"
    echo "}"

    echo ""
    echo "/* this is a compliance options string: */"
    echo -n "const char * const tme_ieee754_compliance_options = \"{ ";
    sep=
    for level in ${levels}; do
	echo -n "${sep}${level}"
	sep=' | '
    done
    echo " }\";"
fi

# done:
#
exit 0;
