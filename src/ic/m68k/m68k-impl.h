/* $Id: m68k-impl.h,v 1.10 2003/05/16 21:48:10 fredette Exp $ */

/* ic/m68k/m68k-impl.h - implementation header file for Motorola 68k emulation: */

/*
 * Copyright (c) 2002, 2003 Matt Fredette
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

#ifndef _IC_M68K_IMPL_H
#define _IC_M68K_IMPL_H

#include <tme/common.h>
_TME_RCSID("$Id: m68k-impl.h,v 1.10 2003/05/16 21:48:10 fredette Exp $");

/* includes: */
#include <tme/ic/m68k.h>
#include <tme/generic/ic.h>
#include <setjmp.h>

/* macros: */

/* CPUs: */
#define TME_M68K_M68000		(0)
#define TME_M68K_M68010		(1)
#define TME_M68K_M68020		(2)
#define TME_M68K_M68030		(3)

/* generic registers: */
#define tme_m68k_ireg_uint32(x)	tme_m68k_ic.tme_ic_ireg_uint32(x)
#define tme_m68k_ireg_int32(x)	tme_m68k_ic.tme_ic_ireg_int32(x)
#define tme_m68k_ireg_uint16(x)	tme_m68k_ic.tme_ic_ireg_uint16(x)
#define tme_m68k_ireg_int16(x)	tme_m68k_ic.tme_ic_ireg_int16(x)
#define tme_m68k_ireg_uint8(x)	tme_m68k_ic.tme_ic_ireg_uint8(x)
#define tme_m68k_ireg_int8(x)	tme_m68k_ic.tme_ic_ireg_int8(x)

/* flags: */
#define TME_M68K_FLAG_C 	TME_BIT(0)
#define TME_M68K_FLAG_V 	TME_BIT(1)
#define TME_M68K_FLAG_Z 	TME_BIT(2)
#define TME_M68K_FLAG_N 	TME_BIT(3)
#define TME_M68K_FLAG_X 	TME_BIT(4)
#define TME_M68K_FLAG_IPM(x)	TME_FIELD_EXTRACTU(x, 8, 3)
#define TME_M68K_FLAG_M		TME_BIT(12)
#define TME_M68K_FLAG_S		TME_BIT(13)
#define TME_M68K_FLAG_T(x)	TME_FIELD_EXTRACTU(x, 14, 2)
#define TME_M68K_FLAG_CCR	(TME_M68K_FLAG_C | TME_M68K_FLAG_V | \
				 TME_M68K_FLAG_Z | TME_M68K_FLAG_N | \
				 TME_M68K_FLAG_X)
#define TME_M68K_FLAG_SR	(TME_M68K_FLAG_CCR | (0x7 << 8) | \
				 TME_M68K_FLAG_M | TME_M68K_FLAG_S | \
				 (0x3 << 14))

/* conditions: */
#define TME_M68K_C_T	(0)
#define TME_M68K_C_F	(1)
#define TME_M68K_C_HI	(2)
#define TME_M68K_C_LS	(3)
#define TME_M68K_C_CC	(4)
#define TME_M68K_C_CS	(5)
#define TME_M68K_C_NE	(6)
#define TME_M68K_C_EQ	(7)
#define TME_M68K_C_VC	(8)
#define TME_M68K_C_VS	(9)
#define TME_M68K_C_PL	(10)
#define TME_M68K_C_MI	(11)
#define TME_M68K_C_GE	(12)
#define TME_M68K_C_LT	(13)
#define TME_M68K_C_GT	(14)
#define TME_M68K_C_LE	(15)
#define TME_M68K_COND_TRUE(ic, c) (_tme_m68k_conditions[(ic)->tme_m68k_ireg_ccr] & TME_BIT(c))

/* bus cycles: */
#define TME_M68K_BUS_CYCLE_NORMAL		(0)
#define TME_M68K_BUS_CYCLE_READ			TME_BIT(0)
#define TME_M68K_BUS_CYCLE_FETCH		TME_BIT(1)
#define TME_M68K_BUS_CYCLE_RMW			TME_BIT(2)
#define TME_M68K_BUS_CYCLE_RAW			TME_BIT(3)

/* exceptions: */
#define TME_M68K_EXCEPTION_NONE			(0)
#define TME_M68K_EXCEPTION_GROUP0_RESET		TME_BIT(0)
#define TME_M68K_EXCEPTION_GROUP0_AERR		TME_BIT(1)
#define TME_M68K_EXCEPTION_GROUP0_BERR		TME_BIT(2)
#define TME_M68K_EXCEPTION_GROUP1_TRACE		TME_BIT(3)
#define TME_M68K_EXCEPTION_GROUP1_INT(ipl, vec)	(((ipl) << 4) | ((vec) << 7))
#define TME_M68K_EXCEPTION_IS_GROUP1_INT(x)	(((x) >> 4) & 0x7)
#define TME_M68K_EXCEPTION_GROUP1_INT_VEC(x)	(((x) >> 7) & 0xff)
#define TME_M68K_EXCEPTION_GROUP1_ILL		TME_BIT(15)
#define TME_M68K_EXCEPTION_GROUP1_PRIV		TME_BIT(16)
#define TME_M68K_EXCEPTION_GROUP2(x)		((x) << 17)
#define TME_M68K_EXCEPTION_IS_GROUP2(x)		((x) >> 17)

/* exception frame formats: */
#define TME_M68K_FORMAT_0	(0)
#define TME_M68K_FORMAT_8	(8)

/* sizes: */
#define TME_M68K_SIZE_SUBMAP_X	(-2)
#define TME_M68K_SIZE_UNDEF	(-1)
#define TME_M68K_SIZE_UNSIZED	(0)
#define TME_M68K_SIZE_8		(1)
#define TME_M68K_SIZE_16	(2)
#define TME_M68K_SIZE_16U8	(3)
#define TME_M68K_SIZE_32	(4)
#define TME_M68K_SIZE_16S32	(5)

/* special opcodes: */
#define TME_M68K_SPECOP_UNDEF		(-1)
#define TME_M68K_SPECOP_BCC		(0)
#define TME_M68K_SPECOP_SPECOP16	(1)
#define TME_M68K_SPECOP_MOVES		(2)
#define TME_M68K_SPECOP_MOVEMEMTOMEM	(3)
#define TME_M68K_SPECOP_ILLEGAL		(4)
#define TME_M68K_SPECOP_MULUL		(5)
#define TME_M68K_SPECOP_DIVUL		(6)
#define TME_M68K_SPECOP_CAS2		(7)
#define TME_M68K_SPECOP_MOVENONMEMTOMEM	(8)

/* major modes of the emulator: */
#define TME_M68K_MODE_EXECUTION	(0)
#define TME_M68K_MODE_EXCEPTION	(1)
#define TME_M68K_MODE_RTE	(2)
#define TME_M68K_MODE_STOP	(3)
#define TME_M68K_MODE_HALT	(4)

/* mode-specific flags: */
#define TME_M68K_EXECUTION_INST_CANFAULT	TME_BIT(0)

/* given a linear address, this hashes it into a TLB entry: */
#define _TME_M68K_TLB_HASH_SIZE (1024)
#define TME_M68K_TLB_ENTRY(ic, function_code, linear_address) \
  (TME_ATOMIC_READ(struct tme_m68k_tlb *, (ic)->_tme_m68k_tlb_array) \
    + ((linear_address >> 10) & (_TME_M68K_TLB_HASH_SIZE - 1)))

/* macros for sequence control: */
#define TME_M68K_SEQUENCE_START						\
do {									\
  ic->_tme_m68k_sequence._tme_m68k_sequence_mode_flags = 0;		\
  ic->_tme_m68k_sequence._tme_m68k_sequence_transfer_faulted = 0;	\
  ic->_tme_m68k_sequence._tme_m68k_sequence_transfer_next = 1;		\
} while (/* CONSTCOND */ 0)
#define TME_M68K_SEQUENCE_RESTARTING					\
  (ic->_tme_m68k_sequence._tme_m68k_sequence_transfer_faulted		\
   >= ic->_tme_m68k_sequence._tme_m68k_sequence_transfer_next)
#define TME_M68K_SEQUENCE_RESTART					\
do {									\
  if (!TME_M68K_SEQUENCE_RESTARTING)					\
    ic->_tme_m68k_sequence._tme_m68k_sequence_transfer_faulted =	\
      ic->_tme_m68k_sequence._tme_m68k_sequence_transfer_next;		\
  ic->_tme_m68k_sequence._tme_m68k_sequence_transfer_next = 1;		\
} while (/* CONSTCOND */ 0)
#define TME_M68K_SEQUENCE_TRANSFER_STEP					\
do {									\
  ic->_tme_m68k_sequence._tme_m68k_sequence_transfer_next++;		\
} while (/* CONSTCOND */ 0)

/* instruction handler macros: */
#define TME_M68K_INSN_DECL(name) void name _TME_P((struct tme_m68k *, void *, void *))
#ifdef __STDC__
#define TME_M68K_INSN(name) void name(struct tme_m68k *ic, void *_op0, void *_op1)
#else  /* !__STDC__ */
#define TME_M68K_INSN(name) void name(ic, _op0, _op1) struct tme_m68k *ic; void *_op0, *_op1;
#endif /* !__STDC__ */
#define TME_M68K_INSN_OP0(t) 		(*((t *) _op0))
#define TME_M68K_INSN_OP1(t) 		(*((t *) _op1))
#define TME_M68K_INSN_OPCODE		ic->_tme_m68k_insn_opcode
#define TME_M68K_INSN_SPECOP		ic->_tme_m68k_insn_specop
#define TME_M68K_INSN_OK		return
#define TME_M68K_INSN_EXCEPTION(e)	tme_m68k_exception(ic, e)
#define TME_M68K_INSN_PRIV			\
  if (!TME_M68K_PRIV(ic))	\
    TME_M68K_INSN_EXCEPTION(TME_M68K_EXCEPTION_GROUP1_PRIV)
#define TME_M68K_INSN_ILL			\
  TME_M68K_INSN_EXCEPTION(TME_M68K_EXCEPTION_GROUP1_ILL)
#define TME_M68K_INSN_CANFAULT			\
do {						\
  ic->_tme_m68k_mode_flags			\
    |= TME_M68K_EXECUTION_INST_CANFAULT;	\
} while (/* CONSTCOND */ 0)
#define TME_M68K_INSN_BRANCH(pc)				\
do {								\
  tme_m68k_verify_end_branch(ic, pc);				\
  ic->tme_m68k_ireg_pc = ic->tme_m68k_ireg_pc_next = (pc);	\
  if (tme_m68k_go_slow(ic)) {					\
    TME_M68K_SEQUENCE_START;					\
    tme_m68k_redispatch(ic);					\
  }								\
} while (/* CONSTCOND */ 0)
#define TME_M68K_INSN_CHANGE_SR(reg)				\
do {								\
  TME_M68K_INSN_PRIV;						\
  tme_m68k_change_sr(ic, reg);					\
} while (/* CONSTCOND */ 0)

/* logging: */
#define TME_M68K_LOG_HANDLE(ic)					\
  (&(ic)->tme_m68k_element->tme_element_log_handle)
#define tme_m68k_log_start(ic, level, rc)			\
do {								\
  tme_log_start(TME_M68K_LOG_HANDLE(ic), level, rc) {		\
    if ((ic)->_tme_m68k_mode != TME_M68K_MODE_EXECUTION) {	\
      tme_log_part(TME_M68K_LOG_HANDLE(ic),			\
                   "mode=%d ",					\
                   (ic)->_tme_m68k_mode);			\
    }								\
    else {							\
      tme_log_part(TME_M68K_LOG_HANDLE(ic),			\
	           "pc=%c/0x%08x ",				\
	           (((ic)->tme_m68k_ireg_sr 			\
		     & (TME_M68K_FLAG_M				\
		        | TME_M68K_FLAG_S))			\
		    ? 'S'					\
		    : 'U'),					\
		   ic->tme_m68k_ireg_pc);			\
    }								\
    do
#define tme_m68k_log_finish(ic)					\
    while (/* CONSTCOND */ 0);					\
  } tme_log_finish(TME_M68K_LOG_HANDLE(ic));			\
} while (/* CONSTCOND */ 0)
#define tme_m68k_log(ic, level, rc, x)		\
do {						\
  tme_m68k_log_start(ic, level, rc) {		\
    tme_log_part x;				\
  } tme_m68k_log_finish(ic);			\
} while (/* CONSTCOND */ 0)

/* miscellaneous: */
#define TME_M68K_OPNUM_SUBMAP_X	(-2)
#define TME_M68K_OPNUM_UNDEF	(-1)
#define TME_M68K_PRIV(ic)	((ic)->tme_m68k_ireg_sr & TME_M68K_FLAG_S)
#define TME_M68K_FUNCTION_CODE_DATA(ic)	\
  (TME_M68K_PRIV(ic) ? TME_M68K_FC_SD : TME_M68K_FC_UD)
#define TME_M68K_FUNCTION_CODE_PROGRAM(ic)	\
  (TME_M68K_PRIV(ic) ? TME_M68K_FC_SP : TME_M68K_FC_UP)
#define TME_M68K_INSN_WORDS_MAX	(11)

/* structures: */
struct tme_m68k;

/* an memory read or write function: */
typedef void (*_tme_m68k_xfer_memx) _TME_P((struct tme_m68k *));
typedef void (*_tme_m68k_xfer_mem) _TME_P((struct tme_m68k *, int));

/* an m68k opcode map entry: */
struct _tme_m68k_opcode {

  /* the instruction: */
  void (*_tme_m68k_opcode_func) _TME_P((struct tme_m68k *, void *, void *));
  
  /* iff this opcode requires some special operand processing, this is
     its type, else this is TME_M68K_SPECOP_TYPE_UNDEF: */
  int _tme_m68k_opcode_specop_type;

  /* if this opcode has an EA, this is the mask of bus cycles: */
  int _tme_m68k_opcode_eax_cycles;
};

/* an m68k decoder generic map entry: */
struct _tme_m68k_decoder_gen {

  /* any known operands, else NULL: */
  void *_tme_m68k_decoder_gen_operand0;
  void *_tme_m68k_decoder_gen_operand1;
    
  /* any known EA size, else TME_M68K_SIZE_UNDEF: */
  int _tme_m68k_decoder_gen_eax_size;

  /* any known immediate operand number, else TME_M68K_OPNUM_UNDEF: */
  int _tme_m68k_decoder_gen_imm_operand;

  /* any known immediate operand size: */
  int _tme_m68k_decoder_gen_imm_size;  
};

/* an m68k decoder submap entry: */
struct _tme_m68k_decoder_submap {

  /* the generic map entry: */
  struct _tme_m68k_decoder_gen _tme_m68k_decoder_submap_gen;

  /* the index for the opcode map: */
  unsigned int _tme_m68k_decoder_submap_opcode_map_index;
};

/* an m68k decoder root map entry: */
struct _tme_m68k_decoder_root {
  
  /* the generic map entry: */
  struct _tme_m68k_decoder_gen _tme_m68k_decoder_root_gen;

  /* the opcode map: */
  const struct _tme_m68k_opcode *_tme_m68k_decoder_root_opcode_map;

  /* the submap: */
  const struct _tme_m68k_decoder_submap *_tme_m68k_decoder_root_submap;
};

/* an m68k sequence: */
struct _tme_m68k_sequence {
  
  /* the mode of the emulator: */
  int _tme_m68k_sequence_mode;

  /* any mode-specific flags for the sequence: */
  int _tme_m68k_sequence_mode_flags;
  
  /* the ordinal of the next memory transfer.  always starts from one: */
  unsigned short _tme_m68k_sequence_transfer_next;

  /* the ordinal of the memory transfer that faulted.  if this is
     greater than or equal to _tme_m68k_sequence_transfer_next, we are
     restarting, and bus cycles and changes to the m68k state are
     forbidden: */
  unsigned short _tme_m68k_sequence_transfer_faulted;

  /* the fault happened after this number of bytes were successfully
     transferred: */
  unsigned short _tme_m68k_sequence_transfer_faulted_after;

#ifdef _TME_M68K_VERIFY
  /* the sequence unique identifier: */
  unsigned long _tme_m68k_sequence_uid;
#endif /* _TME_M68K_VERIFY */
};

/* the m68k state: */
struct tme_m68k {

  /* the IC data structure.  it is beneficial to have this structure
     first, since register numbers can often simply be scaled and 
     added without an offset to the struct tme_m68k pointer to get
     to their contents: */
  struct tme_ic tme_m68k_ic;

  /* the m68k type: */
  int tme_m68k_type;

  /* the backpointer to our element: */
  struct tme_element *tme_m68k_element;

  /* our bus connection.  if both are defined, the m68k bus connection
     is an adaptation layer for the generic bus connection: */
  struct tme_m68k_bus_connection *_tme_m68k_bus_connection;
  struct tme_bus_connection *_tme_m68k_bus_generic;

  /* a jmp_buf back to the dispatcher: */
  jmp_buf _tme_m68k_dispatcher;

  /* the current sequence: */
  struct _tme_m68k_sequence _tme_m68k_sequence;
#define _tme_m68k_mode _tme_m68k_sequence._tme_m68k_sequence_mode
#define _tme_m68k_mode_flags _tme_m68k_sequence._tme_m68k_sequence_mode_flags
#define _tme_m68k_insn_uid _tme_m68k_sequence._tme_m68k_sequence_uid

  /* the CPU-dependent functions for the different modes: */
  void (*_tme_m68k_mode_execute) _TME_P((struct tme_m68k *));
  void (*_tme_m68k_mode_exception) _TME_P((struct tme_m68k *));
  void (*_tme_m68k_mode_rte) _TME_P((struct tme_m68k *));

  /* the instruction decoder root map, and the instruction burst count: */
  const struct _tme_m68k_decoder_root *_tme_m68k_decoder_root;
  unsigned int _tme_m68k_instruction_burst;

  /* the effective address: */
  tme_uint32_t _tme_m68k_ea_address;
  unsigned int _tme_m68k_ea_function_code;

  /* instruction fetch information: */
  tme_uint16_t _tme_m68k_insn_opcode;
  tme_uint16_t _tme_m68k_insn_specop;
  tme_uint16_t _tme_m68k_insn_specop2;
  
  /* the instruction buffer: */
  tme_uint8_t _tme_m68k_insn_buffer[TME_M68K_INSN_WORDS_MAX * sizeof(tme_uint32_t)];
  unsigned int _tme_m68k_insn_buffer_off;
  tme_uint16_t _tme_m68k_insn_buffer_fetch_total;
  tme_uint16_t _tme_m68k_insn_buffer_fetch_sizes;

  /* the TLB entry set, and a separate instruction TLB entry set
     reserved for the executors: */
  TME_ATOMIC(struct tme_m68k_tlb *, _tme_m68k_tlb_array);
  TME_ATOMIC(struct tme_m68k_tlb *, _tme_m68k_itlb);

  /* exception handling information: */
  tme_uint32_t _tme_m68k_exceptions;

  /* nonzero iff this CPU has a 16-bit bus: */
  int _tme_m68k_bus_16bit;

  /* group 0 exception information: */
  void (*_tme_m68k_group0_hook) _TME_P((struct tme_m68k *));
  unsigned int _tme_m68k_group0_flags;
  unsigned int _tme_m68k_group0_function_code;
  tme_uint32_t _tme_m68k_group0_address;
  struct _tme_m68k_sequence _tme_m68k_group0_sequence;
  tme_uint8_t _tme_m68k_group0_buffer_read[sizeof(tme_uint32_t)];
  unsigned int _tme_m68k_group0_buffer_read_size;
  unsigned int _tme_m68k_group0_buffer_read_softrr;
  tme_uint8_t _tme_m68k_group0_buffer_write[sizeof(tme_uint32_t)];
  unsigned int _tme_m68k_group0_buffer_write_size;
  unsigned int _tme_m68k_group0_buffer_write_softrr;

  /* the external request lines: */
  tme_mutex_t tme_m68k_external_mutex;
  tme_cond_t tme_m68k_external_cond;
  unsigned int tme_m68k_external_reset;
  unsigned int tme_m68k_external_halt;
  unsigned int tme_m68k_external_ipl;
  unsigned int tme_m68k_external_vector;
};

/* globals: */
extern const tme_uint16_t _tme_m68k_conditions[32];
extern const tme_uint32_t _tme_m68k_imm32[9];
extern const tme_uint16_t _tme_m68k_imm16[9];
extern const tme_uint8_t _tme_m68k_imm8[9];
extern const _tme_m68k_xfer_memx _tme_m68k_read_memx[5];
extern const _tme_m68k_xfer_memx _tme_m68k_write_memx[5];
extern const _tme_m68k_xfer_mem _tme_m68k_read_mem[5];
extern const _tme_m68k_xfer_mem _tme_m68k_write_mem[5];

/* prototypes: */
int tme_m68k_new _TME_P((struct tme_m68k *, const char * const *, const void *, char **));
void tme_m68k_redispatch _TME_P((struct tme_m68k *));
int tme_m68k_go_slow _TME_P((const struct tme_m68k *));
void tme_m68k_change_sr _TME_P((struct tme_m68k *, tme_uint16_t));
void tme_m68k_external_check _TME_P((struct tme_m68k *, tme_uint32_t));
void tme_m68k_tlb_fill _TME_P((struct tme_m68k *, struct tme_m68k_tlb *, unsigned int, tme_uint32_t, unsigned int));
void tme_m68k_do_reset _TME_P((struct tme_m68k *));

/* exception support: */
void tme_m68k_exception _TME_P((struct tme_m68k *, tme_uint32_t));
void tme_m68k_exception_process_start _TME_P((struct tme_m68k *, unsigned int));
void tme_m68k_exception_process_finish _TME_P((struct tme_m68k *, tme_uint8_t, tme_uint8_t));
void tme_m68k_exception_process _TME_P((struct tme_m68k *));

/* rte support: */
tme_uint16_t tme_m68k_rte_start _TME_P((struct tme_m68k *));
void tme_m68k_rte_finish _TME_P((struct tme_m68k *, tme_uint32_t));

/* decoder map support: */
void _tme_m68000_decoder_map_init _TME_P((struct tme_m68k *));
void _tme_m68010_decoder_map_init _TME_P((struct tme_m68k *));
void _tme_m68020_decoder_map_init _TME_P((struct tme_m68k *));

/* read/modify/write cycle support: */
struct tme_m68k_tlb *tme_m68k_rmw_start _TME_P((struct tme_m68k *));
void tme_m68k_rmw_finish _TME_P((struct tme_m68k *, struct tme_m68k_tlb *));

/* group 0 fault support: */
int tme_m68k_insn_buffer_xfer _TME_P((struct tme_m68k *, tme_uint8_t *, unsigned int, int));
void tme_m68k_group0_hook_fast _TME_P((struct tme_m68k *));
#define tme_m68k_insn_buffer_empty(ic, r, ra) \
  tme_m68k_insn_buffer_xfer(ic, r, ra, 1)
#define tme_m68k_insn_buffer_fill(ic, r, ra) \
  tme_m68k_insn_buffer_xfer(ic, r, ra, 2)
int tme_m68k_sequence_empty _TME_P((const struct tme_m68k *, tme_uint8_t *, unsigned int));
int tme_m68k_sequence_fill _TME_P((struct tme_m68k *, const tme_uint8_t *, unsigned int));

/* bitfield support: */
unsigned int tme_m68k_bitfield_width _TME_P((struct tme_m68k *));
tme_uint32_t _tme_m68k_bitfield_read _TME_P((struct tme_m68k *, int));
#define tme_m68k_bitfield_read_signed(ic) ((tme_int32_t) _tme_m68k_bitfield_read(ic, TRUE))
#define tme_m68k_bitfield_read_unsigned(ic) _tme_m68k_bitfield_read(ic, FALSE)
void tme_m68k_bitfield_write_unsigned _TME_P((struct tme_m68k *, tme_uint32_t, int));
#define tme_m68k_bitfield_write_signed(ic, v, sf) tme_m68k_bitfield_write_unsigned(ic, (tme_uint32_t) (v), sf)

/* verification: */
#ifdef _TME_M68K_VERIFY
void tme_m68k_verify_init _TME_P((void));
void tme_m68k_verify_begin _TME_P((const struct tme_m68k *, const tme_uint8_t *));
void tme_m68k_verify_mem_any _TME_P((const struct tme_m68k *,
				     unsigned int, tme_uint32_t,
				     tme_uint8_t *, int, int));
void tme_m68k_verify_end_branch _TME_P((const struct tme_m68k *, tme_uint32_t));
void tme_m68k_verify_end _TME_P((const struct tme_m68k *,
				 void (*)(struct tme_m68k *, void *, void *)));
void tme_m68k_verify_hook _TME_P((void));
#else  /* _TME_M68K_VERIFY */
#define tme_m68k_verify_init() do { } while (/* CONSTCOND */ 0)
#define tme_m68k_verify_begin(ic, s) do { } while (/* CONSTCOND */ 0)
#define tme_m68k_verify_mem_any(ic, fc, a, v, c, rw) do { } while (/* CONSTCOND */ 0)
#define tme_m68k_verify_end_branch(ic, pc) do { } while (/* CONSTCOND */ 0)
#define tme_m68k_verify_end(ic, f) do { } while (/* CONSTCOND */ 0)
#define tme_m68k_verify_hook() do { } while (/* CONSTCOND */ 0)
#endif /* _TME_M68K_VERIFY */
#define tme_m68k_verify_mem8(ic, fc, a, v, rw) tme_m68k_verify_mem_any(ic, fc, a, (tme_uint8_t *) &(v), -sizeof(tme_uint8_t), rw)
#define tme_m68k_verify_mem16(ic, fc, a, v, rw) tme_m68k_verify_mem_any(ic, fc, a, (tme_uint8_t *) &(v), -sizeof(tme_uint16_t), rw)
#define tme_m68k_verify_mem32(ic, fc, a, v, rw) tme_m68k_verify_mem_any(ic, fc, a, (tme_uint8_t *) &(v), -sizeof(tme_uint32_t), rw)

/* instruction functions: */
TME_M68K_INSN_DECL(tme_m68k_illegal);
TME_M68K_INSN_DECL(tme_m68k_exg);
TME_M68K_INSN_DECL(tme_m68k_extw);
TME_M68K_INSN_DECL(tme_m68k_extl);
TME_M68K_INSN_DECL(tme_m68k_extbl);
TME_M68K_INSN_DECL(tme_m68k_lea);
TME_M68K_INSN_DECL(tme_m68k_move_from_ccr);
TME_M68K_INSN_DECL(tme_m68k_move_from_sr);
TME_M68K_INSN_DECL(tme_m68k_move_from_sr0);
TME_M68K_INSN_DECL(tme_m68k_swap);
TME_M68K_INSN_DECL(tme_m68k_nop);
TME_M68K_INSN_DECL(tme_m68k_scc);
TME_M68K_INSN_DECL(tme_m68k_dbcc);
TME_M68K_INSN_DECL(tme_m68k_bcc);
TME_M68K_INSN_DECL(tme_m68k_bsr);
TME_M68K_INSN_DECL(tme_m68k_pea);
TME_M68K_INSN_DECL(tme_m68k_bkpt);
TME_M68K_INSN_DECL(tme_m68k_tas);
TME_M68K_INSN_DECL(tme_m68k_tas_r);
TME_M68K_INSN_DECL(tme_m68k_move_usp);
TME_M68K_INSN_DECL(tme_m68k_trap);
TME_M68K_INSN_DECL(tme_m68k_trapv);
TME_M68K_INSN_DECL(tme_m68k_link);
TME_M68K_INSN_DECL(tme_m68k_unlk);
TME_M68K_INSN_DECL(tme_m68k_movec);
TME_M68K_INSN_DECL(tme_m68k_reset);
TME_M68K_INSN_DECL(tme_m68k_rtd);
TME_M68K_INSN_DECL(tme_m68k_rtr);
TME_M68K_INSN_DECL(tme_m68k_rts);
TME_M68K_INSN_DECL(tme_m68k_jsr);
TME_M68K_INSN_DECL(tme_m68k_jmp);
TME_M68K_INSN_DECL(tme_m68k_rte);
TME_M68K_INSN_DECL(tme_m68k_stop);
TME_M68K_INSN_DECL(tme_m68k_priv);
TME_M68K_INSN_DECL(tme_m68k_cmp2_chk2);
TME_M68K_INSN_DECL(tme_m68k_callm);
TME_M68K_INSN_DECL(tme_m68k_rtm);
TME_M68K_INSN_DECL(tme_m68k_trapcc);
TME_M68K_INSN_DECL(tme_m68k_bfchg);
TME_M68K_INSN_DECL(tme_m68k_bfclr);
TME_M68K_INSN_DECL(tme_m68k_bfexts);
TME_M68K_INSN_DECL(tme_m68k_bfextu);
TME_M68K_INSN_DECL(tme_m68k_bfffo);
TME_M68K_INSN_DECL(tme_m68k_bfins);
TME_M68K_INSN_DECL(tme_m68k_bfset);
TME_M68K_INSN_DECL(tme_m68k_bftst);
TME_M68K_INSN_DECL(tme_m68k_pack);
TME_M68K_INSN_DECL(tme_m68k_unpk);

#endif /* !_IC_M68K_IMPL_H */

/* the automatically-generated header information: */
#include <m68k-auto.h>
