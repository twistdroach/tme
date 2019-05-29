/* $Id: sparc-impl.h,v 1.5 2007/03/29 01:13:33 fredette Exp $ */

/* ic/sparc/sparc-impl.h - implementation header file for SPARC emulation: */

/*
 * Copyright (c) 2005 Matt Fredette
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by Matt Fredette.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef _IC_SPARC_IMPL_H
#define _IC_SPARC_IMPL_H

#include <tme/common.h>
_TME_RCSID("$Id: sparc-impl.h,v 1.5 2007/03/29 01:13:33 fredette Exp $");

/* includes: */
#include <tme/ic/sparc.h>
#include <tme/ic/ieee754.h>
#include <tme/generic/ic.h>
#include <setjmp.h>

/* macros: */

/* generic registers: */
#define tme_sparc_ireg_uint64(x)	tme_sparc_ic.tme_ic_ireg_uint64(x)
#define tme_sparc_ireg_int64(x)		tme_sparc_ic.tme_ic_ireg_int64(x)
#define tme_sparc_ireg_uint32(x)	tme_sparc_ic.tme_ic_ireg_uint32(x)
#define tme_sparc_ireg_int32(x)		tme_sparc_ic.tme_ic_ireg_int32(x)
#define tme_sparc_ireg_uint8(x)		tme_sparc_ic.tme_ic_ireg_uint8(x)

/* format 3 instruction fields: */
#define TME_SPARC_FORMAT3_MASK_RS2	(0x1f <<  0)
#define TME_SPARC_FORMAT3_MASK_RS1	(0x1f << 14)
#define TME_SPARC_FORMAT3_MASK_RD	(0x1f << 25)

/* traps: */
#define TME_SPARC_TRAP_none				(0x00)
#define TME_SPARC_TRAP_reset				(0x100)
#define TME_SPARC_TRAP_instruction_access_MMU_miss	(0x3C)
#define TME_SPARC_TRAP_instruction_access_error		(0x21)
#define TME_SPARC_TRAP_r_register_access_error		(0x20)
#define TME_SPARC_TRAP_instruction_access_exception	(0x01)
#define TME_SPARC_TRAP_privileged_instruction		(0x03)
#define TME_SPARC_TRAP_illegal_instruction		(0x02)
#define TME_SPARC_TRAP_fp_disabled			(0x04)
#define TME_SPARC_TRAP_cp_disabled			(0x24)
#define TME_SPARC_TRAP_unimplemented_FLUSH		(0x25)
#define TME_SPARC_TRAP_watchpoint_detected		(0x0B)
#define TME_SPARC_TRAP_window_overflow			(0x05)
#define TME_SPARC_TRAP_window_underflow			(0x06)
#define TME_SPARC_TRAP_mem_address_not_aligned		(0x07)
#define TME_SPARC_TRAP_fp_exception			(0x08)
#define TME_SPARC_TRAP_cp_exception			(0x28)
#define TME_SPARC_TRAP_data_access_error		(0x29)
#define TME_SPARC_TRAP_data_access_MMU_miss		(0x2C)
#define TME_SPARC_TRAP_data_access_exception		(0x09)
#define TME_SPARC_TRAP_tag_overflow			(0x0A)
#define TME_SPARC_TRAP_division_by_zero			(0x2A)
#define TME_SPARC_TRAP_trap_instruction(x)		(0x80 + (x))
#define TME_SPARC_TRAP_interrupt_level(il)		(0x10 + (il))

/* SPARC FPU FSR fields: */
#define TME_SPARC_FSR_RND		(0xc0000000)
#define  TME_SPARC_FSR_RND_RN		 (0x00000000)
#define  TME_SPARC_FSR_RND_RZ		 (0x40000000)
#define  TME_SPARC_FSR_RND_RP		 (0x80000000)
#define  TME_SPARC_FSR_RND_RM		 (0xc0000000)
#define TME_SPARC_FSR_TEM		(0x0f800000)
#define TME_SPARC_FSR_NS		TME_BIT(22)
#define TME_SPARC_FSR_VER		(0x000e0000)
#define  TME_SPARC_FSR_VER_missing	 (0x000e0000)
#define TME_SPARC_FSR_FTT		(0x0001c000)
#define  TME_SPARC_FSR_FTT_none			(0x00000000)
#define  TME_SPARC_FSR_FTT_IEEE754_exception	(0x00004000)
#define  TME_SPARC_FSR_FTT_unfinished_FPop	(0x00008000)
#define  TME_SPARC_FSR_FTT_unimplemented_FPop	(0x0000c000)
#define  TME_SPARC_FSR_FTT_sequence_error	(0x00010000)
#define  TME_SPARC_FSR_FTT_hardware_error	(0x00014000)
#define  TME_SPARC_FSR_FTT_invalid_fp_register	(0x00018000)
#define TME_SPARC_FSR_QNE		TME_BIT(13)
#define TME_SPARC_FSR_FCC		(0x00000c00)
#define  TME_SPARC_FSR_FCC_EQ		 (0x00000000)
#define  TME_SPARC_FSR_FCC_LT		 (0x00000400)
#define  TME_SPARC_FSR_FCC_GT		 (0x00000800)
#define  TME_SPARC_FSR_FCC_UN		 (0x00000c00)
#define TME_SPARC_FSR_AEXC		(0x000003e0)
#define TME_SPARC_FSR_CEXC		(0x0000001f)
#define  TME_SPARC_FSR_CEXC_NVC		TME_BIT(4)
#define  TME_SPARC_FSR_CEXC_OFC		TME_BIT(3)
#define  TME_SPARC_FSR_CEXC_UFC		TME_BIT(2)
#define  TME_SPARC_FSR_CEXC_DZC		TME_BIT(1)
#define  TME_SPARC_FSR_CEXC_NXC		TME_BIT(0)

/* sparc32 PSR fields: */
#define TME_SPARC32_PSR_IMPL	(0xf0000000)
#define TME_SPARC32_PSR_VER	(0x0f000000)
#define TME_SPARC32_PSR_ICC_N	TME_BIT(23)
#define TME_SPARC32_PSR_ICC_Z	TME_BIT(22)
#define TME_SPARC32_PSR_ICC_V	TME_BIT(21)
#define TME_SPARC32_PSR_ICC_C	TME_BIT(20)
#define TME_SPARC32_PSR_ICC	(TME_SPARC32_PSR_ICC_N | TME_SPARC32_PSR_ICC_Z | TME_SPARC32_PSR_ICC_V | TME_SPARC32_PSR_ICC_C)
#define TME_SPARC32_PSR_EC	TME_BIT(13)
#define TME_SPARC32_PSR_EF	TME_BIT(12)
#define TME_SPARC32_PSR_PIL	(0x00000f00)
#define TME_SPARC32_PSR_S	TME_BIT(7)
#define TME_SPARC32_PSR_PS	TME_BIT(6)
#define TME_SPARC32_PSR_ET	TME_BIT(5)
#define TME_SPARC32_PSR_CWP	(0x0000001f)

/* sparc64 PSTATE flags: */
#define TME_SPARC64_PSTATE_CLE	TME_BIT(9)
#define TME_SPARC64_PSTATE_PRIV	TME_BIT(2)
#define TME_SPARC64_PSTATE_PEF	TME_BIT(4)

/* sparc64 CCR flags: */
#define TME_SPARC64_CCR_XCC_N	TME_BIT(7)
#define TME_SPARC64_CCR_XCC_Z	TME_BIT(6)
#define TME_SPARC64_CCR_XCC_V	TME_BIT(5)
#define TME_SPARC64_CCR_XCC_C	TME_BIT(4)
#define TME_SPARC64_CCR_ICC_N	TME_BIT(3)
#define TME_SPARC64_CCR_ICC_Z	TME_BIT(2)
#define TME_SPARC64_CCR_ICC_V	TME_BIT(1)
#define TME_SPARC64_CCR_ICC_C	TME_BIT(0)
#define TME_SPARC64_CCR_ICC	(TME_SPARC64_CCR_ICC_N | TME_SPARC64_CCR_ICC_Z | TME_SPARC64_CCR_ICC_V | TME_SPARC64_CCR_ICC_C)

/* idle types and idle type state: */
#define TME_SPARC_IDLE_TYPE_NULL		(0)
#define TME_SPARC_IDLE_TYPE_NETBSD32_TYPE_0	TME_BIT(0)
#define TME_SPARC_IDLE_TYPE_SUNOS32_TYPE_0	TME_BIT(1)
#define TME_SPARC_IDLE_TYPES_SUPPORTED		(((unsigned int) 0) - 1)
#define TME_SPARC_IDLE_TYPE_IS(ic, x)		\
  ((TME_SPARC_IDLE_TYPES_SUPPORTED & (x))	\
   && (ic)->tme_sparc_idle_type == (x))
#define TME_SPARC_IDLE_TYPE_PC_STATE(x)		((x) % sizeof(tme_uint32_t))

/* major modes of the emulator: */
#define TME_SPARC_MODE_EXECUTION	(0)
#define TME_SPARC_MODE_STOP		(1)
#define TME_SPARC_MODE_HALT		(2)

/* the maximum number of windows: */
#define TME_SPARC_WINDOWS_MAX		(16)

/* given a CWP value, this returns the CWP register offset: */
#define TME_SPARC_CWP_OFFSET(cwp)	((cwp) * 16)

/* this converts the given lvalue from a register number into a
   register set index: */
#define TME_SPARC_REG_INDEX(ic, reg)				\
  do {								\
    if ((reg) > 7) {						\
      (reg) += ic->tme_sparc_cwp_offset;			\
    }								\
    if ((reg)							\
	> (TME_SPARC_CWP_OFFSET(TME_SPARC_NWINDOWS(ic)) + 7)) {	\
      (reg) -= TME_SPARC_CWP_OFFSET(TME_SPARC_NWINDOWS(ic));	\
    }								\
  } while (/* CONSTCOND */ 0)

/* given a linear address, this hashes it into a DTLB entry: */
#define _TME_SPARC_DTLB_HASH_SIZE 	(1024)
#define TME_SPARC_DTLB_ENTRY(ic, address)	\
  (tme_memory_atomic_pointer_read(struct tme_sparc_tlb *,	\
				  (ic)->_tme_sparc_dtlb_array,	\
				  &(ic)->_tme_sparc_tlb_rwlock)	\
   + ((address >> 10) & (_TME_SPARC_DTLB_HASH_SIZE - 1)))

/* the size of the ITLB hash: */
#define _TME_SPARC_ITLB_HASH_SIZE	(32)

/* flags for the slow load and store functions: */
#define TME_SPARC_SLOW_FLAG_A			TME_BIT(5)
#define TME_SPARC_SLOW_FLAG_ATOMIC		TME_BIT(6)
#define TME_SPARC_SLOW_FLAG_INSN		TME_BIT(7)
#define TME_SPARC_SLOW_FLAG_NO_FAULTS		TME_BIT(8)

/* flags for FPU features: */
#define TME_SPARC_FPU_FLAG_NO_QUAD		TME_BIT(0)
#define TME_SPARC_FPU_FLAG_NO_FSQRT		TME_BIT(1)
#define TME_SPARC_FPU_FLAG_NO_FMUL_WIDER	TME_BIT(2)
#define TME_SPARC_FPU_FLAG_OK_REG_MISALIGNED	TME_BIT(3)

/* FPU modes: */
#define TME_SPARC_FPU_MODE_EXECUTE		(0)
#define TME_SPARC_FPU_MODE_EXCEPTION_PENDING	(1)
#define TME_SPARC_FPU_MODE_EXCEPTION		(2)

/* instruction handler macros: */
#define TME_SPARC_FORMAT3_DECL(name, type) void name _TME_P((struct tme_sparc *, const type *, const type *, type *))
#ifdef __STDC__
#define TME_SPARC_FORMAT3(name, type) void name(struct tme_sparc *ic, const type *_rs1, const type *_rs2, type *_rd)
#else  /* !__STDC__ */
#define TME_SPARC_FORMAT3(name, type) void name(ic, _rs1, _rs2, _rd) struct tme_sparc *ic; const type *_rs1, *_rs2; type *_rd;
#endif /* !__STDC__ */
#define TME_SPARC_FORMAT3_RS1		(*_rs1)
#define TME_SPARC_FORMAT3_RS2		(*_rs2)
#define TME_SPARC_FORMAT3_RD		(*_rd)
#define TME_SPARC_FORMAT3_RD_ODD	(*(_rd + (&(((struct tme_ic *) NULL)->tme_ic_ireg_uint32(1)) - &(((struct tme_ic *) NULL)->tme_ic_ireg_uint32(0)))))
#define TME_SPARC_INSN			ic->_tme_sparc_insn
#define TME_SPARC_INSN_OK		return
#define TME_SPARC_INSN_TRAP(trap)			\
  do {							\
    if (TME_SPARC_VERSION(ic) < 9) {			\
      tme_sparc32_trap(ic, trap);			\
    }							\
    else {						\
      tme_sparc64_trap(ic, trap);			\
    }							\
  } while (/* CONSTCOND */ 0)
#define TME_SPARC_INSN_PRIV				\
  do {							\
    if (__tme_predict_false(!TME_SPARC_PRIV(ic))) {	\
      TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_privileged_instruction);\
    }							\
  } while (/* CONSTCOND */ 0)
#define TME_SPARC_INSN_FPU				\
  do {							\
    if (__tme_predict_false((TME_SPARC_VERSION(ic) < 9)	\
			    ? ((ic)->tme_sparc32_ireg_psr & TME_SPARC32_PSR_EF) == 0 \
			    : ((ic)->tme_sparc64_ireg_pstate & TME_SPARC64_PSTATE_PEF) == 0)) { \
      TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_fp_disabled);	\
    }							\
    if (__tme_predict_false((ic)->tme_sparc_fpu_mode	\
			    != TME_SPARC_FPU_MODE_EXECUTE)) { \
      tme_sparc_fpu_exception_check(ic);		\
    }							\
  } while (/* CONSTCOND */ 0)
#define TME_SPARC_INSN_FPU_STORE(size)			\
  do {							\
    if (__tme_predict_false((TME_SPARC_VERSION(ic) < 9)	\
			    ? ((ic)->tme_sparc32_ireg_psr & TME_SPARC32_PSR_EF) == 0 \
			    : ((ic)->tme_sparc64_ireg_pstate & TME_SPARC64_PSTATE_PEF) == 0)) { \
      TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_fp_disabled);	\
    }							\
    if (__tme_predict_false(((TME_SPARC_FORMAT3_RS1	\
			      + TME_SPARC_FORMAT3_RS2)	\
			    % (size)) != 0)) {		\
      TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_mem_address_not_aligned); \
    }							\
    if (__tme_predict_false((ic)->tme_sparc_fpu_mode	\
			    == TME_SPARC_FPU_MODE_EXCEPTION_PENDING)) { \
      tme_sparc_fpu_exception_check(ic);		\
    }							\
  } while (/* CONSTCOND */ 0)

#define TME_SPARC_INSN_ILL				\
  TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_illegal_instruction)

/* logging: */
#define TME_SPARC_LOG_HANDLE(ic)				\
  (&(ic)->tme_sparc_element->tme_element_log_handle)
#define tme_sparc_log_start(ic, level, rc)			\
  do {								\
    tme_log_start(TME_SPARC_LOG_HANDLE(ic), level, rc) {	\
      if ((ic)->_tme_sparc_mode != TME_SPARC_MODE_EXECUTION) {	\
        tme_log_part(TME_SPARC_LOG_HANDLE(ic),			\
                     "mode=%d ",				\
                     (ic)->_tme_sparc_mode);			\
      }								\
      else {							\
        tme_log_part(TME_SPARC_LOG_HANDLE(ic),			\
	             "pc=%c/0x%08x ",				\
	             (TME_SPARC_PRIV(ic)			\
		      ? 'S'					\
		      : 'U'),					\
		     ((TME_SPARC_VERSION(ic) < 9)		\
		      ? ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC)\
		      : (tme_uint32_t) ic->tme_sparc_ireg_uint64(TME_SPARC_IREG_PC)));\
      }								\
      do
#define tme_sparc_log_finish(ic)				\
      while (/* CONSTCOND */ 0);				\
    } tme_log_finish(TME_SPARC_LOG_HANDLE(ic));			\
  } while (/* CONSTCOND */ 0)
#define tme_sparc_log(ic, level, rc, x)		\
  do {						\
    tme_sparc_log_start(ic, level, rc) {	\
      tme_log_part x;				\
    } tme_sparc_log_finish(ic);			\
  } while (/* CONSTCOND */ 0)

/* miscellaneous: */
#define _TME_SPARC_VERSION(ic)	((ic)->tme_sparc_version)
#define _TME_SPARC_NWINDOWS(ic) ((ic)->tme_sparc_nwindows)
#define _TME_SPARC32_PRIV(ic)   (((ic)->tme_sparc32_ireg_psr & TME_SPARC32_PSR_S) != 0)
#define _TME_SPARC64_PRIV(ic)   (((ic)->tme_sparc64_ireg_pstate & TME_SPARC64_PSTATE_PRIV) != 0)
#define TME_SPARC_VERSION(ic)	_TME_SPARC_VERSION(ic)
#define TME_SPARC_NWINDOWS(ic)	_TME_SPARC_NWINDOWS(ic)
#define TME_SPARC_PRIV(ic)		\
  ((TME_SPARC_VERSION(ic) < 9)		\
   ? _TME_SPARC32_PRIV(ic)		\
   : _TME_SPARC64_PRIV(ic))
#define TME_SPARC_ASI_DATA(ic)		\
  ((TME_SPARC_VERSION(ic) < 9)		\
   ? (_TME_SPARC32_PRIV(ic)		\
      ? TME_SPARC32_ASI_SD		\
      : TME_SPARC32_ASI_UD)		\
   : ((ic->tme_sparc64_ireg_pstate & TME_SPARC64_PSTATE_CLE)\
      ? TME_SPARC64_ASI_PRIMARY_LITTLE	\
      : TME_SPARC64_ASI_PRIMARY))

/* structures: */
struct tme_sparc;

/* format 3 instruction functions: */
typedef TME_SPARC_FORMAT3_DECL((*_tme_sparc32_format3), tme_uint32_t);

/* a sparc deferred-trap queue: */
struct tme_sparc_trapqueue {
#ifdef TME_HAVE_INT64_T
  tme_uint64_t tme_sparc_trapqueue_address;
#else  /* !TME_HAVE_INT64_T */
  tme_uint32_t tme_sparc_trapqueue_address;
#endif /* !TME_HAVE_INT64_T */
  tme_uint32_t tme_sparc_trapqueue_insn;
};

/* the sparc state: */
struct tme_sparc {

  /* the IC data structure.  it is beneficial to have this structure
     first, since register numbers can often simply be scaled and 
     added without an offset to the struct tme_sparc pointer to get
     to their contents: */
  struct tme_ic tme_sparc_ic;

  /* the current window pointer register offset: */
  unsigned int tme_sparc_cwp_offset;

  /* the architecture version, and number of windows: */
  unsigned int tme_sparc_version;
  unsigned int tme_sparc_nwindows;

  /* the backpointer to our element: */
  struct tme_element *tme_sparc_element;

  /* our bus connection.  if both are defined, the sparc bus connection
     is an adaptation layer for the generic bus connection: */
  struct tme_sparc_bus_connection *_tme_sparc_bus_connection;
  struct tme_bus_connection *_tme_sparc_bus_generic;

  /* a jmp_buf back to the dispatcher: */
  jmp_buf _tme_sparc_dispatcher;

  /* the current mode of the CPU: */
  int _tme_sparc_mode;

  /* address space identifiers and masks: */
  tme_uint32_t tme_sparc_asi_mask_insn;
  tme_uint32_t tme_sparc_asi_mask_data;

  /* the implementation-dependent functions: */
  void (*_tme_sparc_execute) _TME_P((struct tme_sparc *));
  tme_uint32_t (*_tme_sparc_bus_fault) _TME_P((struct tme_sparc *, const struct tme_bus_cycle *, unsigned int, int));
  tme_uint32_t (*_tme_sparc_fetch_slow) _TME_P((struct tme_sparc *, int));
  tme_uint32_t (*_tme_sparc_fpu_ver) _TME_P((struct tme_sparc *, const char *, char **));

  /* the instruction burst count, and the remaining burst: */
  unsigned int _tme_sparc_instruction_burst;
  unsigned int _tme_sparc_instruction_burst_remaining;

  /* any currently busy instruction TLB entry: */
  struct tme_sparc_tlb *_tme_sparc_itlb_busy;

  /* instruction information: */
  tme_uint32_t _tme_sparc_insn;
  
  /* the data and instruction TLB entry sets: */
  union {
    struct tme_sparc_tlb * tme_shared _tme_sparc_dtlb_array_u_sparc;
    struct tme_bus_tlb * tme_shared _tme_sparc_dtlb_array_u_bus;
  } _tme_sparc_dtlb_array_u;
#define _tme_sparc_dtlb_array _tme_sparc_dtlb_array_u._tme_sparc_dtlb_array_u_sparc
#define _tme_sparc_dtlb_array_bus _tme_sparc_dtlb_array_u._tme_sparc_dtlb_array_u_bus
  union {
    struct tme_sparc_tlb * tme_shared _tme_sparc_itlb_array_u_sparc;
    struct tme_bus_tlb * tme_shared _tme_sparc_itlb_array_u_bus;
  } _tme_sparc_itlb_array_u;
#define _tme_sparc_itlb_array _tme_sparc_itlb_array_u._tme_sparc_itlb_array_u_sparc
#define _tme_sparc_itlb_array_bus _tme_sparc_itlb_array_u._tme_sparc_itlb_array_u_bus
  tme_rwlock_t _tme_sparc_tlb_rwlock;

  /* the external request lines: */
  tme_mutex_t tme_sparc_external_mutex;
  tme_cond_t tme_sparc_external_cond;
  unsigned int tme_sparc_external_reset;
  unsigned int tme_sparc_external_halt;
  unsigned int tme_sparc_external_ipl;

  /* the slow load/store buffer: */
  tme_uint8_t tme_sparc_memory_buffer[sizeof(tme_uint32_t) * 2];

  /* any FPU state: */
  struct tme_ieee754_ctl tme_sparc_fpu_ieee754_ctl;
  _tme_const struct tme_ieee754_ops *tme_sparc_fpu_ieee754_ops;
  _tme_const struct tme_ieee754_ops *tme_sparc_fpu_ieee754_ops_user;
  _tme_const struct tme_ieee754_ops *tme_sparc_fpu_ieee754_ops_strict;
  struct tme_float tme_sparc_fpu_fpregs[32];
  unsigned int tme_sparc_fpu_fpreg_sizes[32];
  tme_uint32_t tme_sparc_fpu_fsr;
  struct tme_sparc_trapqueue tme_sparc_fpu_fq[1];
  unsigned int tme_sparc_fpu_mode;
  unsigned int tme_sparc_fpu_flags;
  int tme_sparc_fpu_incomplete_abort;

  /* any idle type, and idle type state: */
  unsigned int tme_sparc_idle_type;
  tme_uint32_t tme_sparc_idle_type_pc32;
  tme_uint32_t tme_sparc_idle_type_pc64;

#ifdef _TME_SPARC_STATS
  /* statistics: */
  struct {

    /* the total number of instructions executed: */
    tme_uint64_t tme_sparc_stats_insns_total;

    /* the total number of instructions fetched slowly: */
    tme_uint64_t tme_sparc_stats_fetch_slow;

    /* the total number of redispatches: */
    tme_uint64_t tme_sparc_stats_redispatches;

    /* the total number of data memory operations: */
    tme_uint64_t tme_sparc_stats_memory_total;

    /* the total number of ITLB fills: */
    tme_uint64_t tme_sparc_stats_itlb_fill;

    /* the total number of DTLB fills: */
    tme_uint64_t tme_sparc_stats_dtlb_fill;

  } tme_sparc_stats;
#endif /* _TME_SPARC_STATS */
};

/* globals: */
extern const tme_uint8_t _tme_sparc_conds_icc[16];
extern const tme_uint8_t _tme_sparc_conds_fcc[4];

/* prototypes: */
int tme_sparc_new _TME_P((struct tme_sparc *, const char * const *, const void *, char **));
void tme_sparc_redispatch _TME_P((struct tme_sparc *));
void tme_sparc_do_reset _TME_P((struct tme_sparc *));
void tme_sparc_do_idle _TME_P((struct tme_sparc *));
void tme_sparc32_external_check _TME_P((struct tme_sparc *));
tme_uint32_t tme_sparc32_fetch_slow _TME_P((struct tme_sparc *, int));
tme_uint32_t tme_sparc_fetch_nearby _TME_P((struct tme_sparc *, long));
void tme_sparc_callout_unlock _TME_P((struct tme_sparc *));
void tme_sparc_callout_relock _TME_P((struct tme_sparc *));

/* trap support: */
void tme_sparc32_trap_preinstruction _TME_P((struct tme_sparc *, unsigned int));
void tme_sparc32_trap _TME_P((struct tme_sparc *, tme_uint32_t));
tme_uint32_t tme_sparc32_bus_fault _TME_P((struct tme_sparc *, const struct tme_bus_cycle *, unsigned int, int));
void tme_sparc64_trap _TME_P((struct tme_sparc *, tme_uint32_t));

/* FPU support: */
int tme_sparc_fpu_new _TME_P((struct tme_sparc *, const char * const *, int *, int *, char **));
void tme_sparc_fpu_reset _TME_P((struct tme_sparc *));
void tme_sparc_fpu_usage _TME_P((struct tme_sparc *, char **));
void tme_sparc_fpu_strict _TME_P((struct tme_sparc_bus_connection *, unsigned int));
void tme_sparc_fpu_exception_check _TME_P((struct tme_sparc *));
int tme_sparc_fpu_fpreg_aligned _TME_P((struct tme_sparc *, unsigned int, unsigned int));
void tme_sparc_fpu_fpreg_format _TME_P((struct tme_sparc *, unsigned int, unsigned int));
void tme_sparc_fpu_fpop1 _TME_P((struct tme_sparc *));
void tme_sparc_fpu_fpop2 _TME_P((struct tme_sparc *));

/* verification: */
void tme_sparc_verify_hook _TME_P((void));
#ifdef _TME_SPARC_VERIFY
void tme_sparc_verify_init _TME_P((void));
void tme_sparc_verify_begin _TME_P((const struct tme_sparc *, const tme_uint8_t *));
void tme_sparc_verify_mem_any _TME_P((const struct tme_sparc *,
				     unsigned int, tme_uint32_t,
				     tme_uint8_t *, int, int));
void tme_sparc_verify_end_branch _TME_P((const struct tme_sparc *, tme_uint32_t));
void tme_sparc_verify_end _TME_P((const struct tme_sparc *,
				 void (*)(struct tme_sparc *, void *, void *)));
#else  /* _TME_SPARC_VERIFY */
#define tme_sparc_verify_init() do { } while (/* CONSTCOND */ 0)
#define tme_sparc_verify_begin(ic, s) do { } while (/* CONSTCOND */ 0)
#define tme_sparc_verify_mem_any(ic, fc, a, v, c, rw) do { } while (/* CONSTCOND */ 0)
#define tme_sparc_verify_end_branch(ic, pc) do { } while (/* CONSTCOND */ 0)
#define tme_sparc_verify_end(ic, f) do { } while (/* CONSTCOND */ 0)
#define tme_sparc_verify_hook() do { } while (/* CONSTCOND */ 0)
#endif /* _TME_SPARC_VERIFY */
#define tme_sparc_verify_mem8(ic, fc, a, v, rw) tme_sparc_verify_mem_any(ic, fc, a, (tme_uint8_t *) &(v), -sizeof(tme_uint8_t), rw)
#define tme_sparc_verify_mem16(ic, fc, a, v, rw) tme_sparc_verify_mem_any(ic, fc, a, (tme_uint8_t *) &(v), -sizeof(tme_uint16_t), rw)
#define tme_sparc_verify_mem32(ic, fc, a, v, rw) tme_sparc_verify_mem_any(ic, fc, a, (tme_uint8_t *) &(v), -sizeof(tme_uint32_t), rw)

/* instruction functions: */
TME_SPARC_FORMAT3_DECL(tme_sparc32_illegal, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_cpop1, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_cpop2, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_ldc, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_ldcsr, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_lddc, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_stc, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_stcsr, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_stdc, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_stdcq, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_rdasr, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_rdpsr, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_rdwim, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_rdtbr, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_wrasr, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_wrpsr, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_wrwim, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_wrtbr, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_flush, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_rett, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_save_restore, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_ticc, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_stdfq, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_fpop1, tme_uint32_t);
TME_SPARC_FORMAT3_DECL(tme_sparc32_fpop2, tme_uint32_t);

/* the automatically-generated header information: */
#include <sparc-auto.h>

#endif /* !_IC_SPARC_IMPL_H */
