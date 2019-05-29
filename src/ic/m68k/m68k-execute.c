/* $Id: m68k-execute.c,v 1.14 2003/10/25 17:08:00 fredette Exp $ */

/* ic/m68k/m68k-execute.c - executes Motorola 68k instructions: */

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

_TME_RCSID("$Id: m68k-execute.c,v 1.14 2003/10/25 17:08:00 fredette Exp $");

/* includes: */
#include "m68k-auto.h"

/* the m68k instruction executor: */
static void
_TME_M68K_EXECUTE_NAME(struct tme_m68k *ic)
{
#undef _TME_M68K_SEQUENCE_RESTARTING
#undef _TME_M68K_INSN_FETCH_SAVE
#ifdef _TME_M68K_EXECUTE_FAST
  struct tme_m68k_tlb *tlb;
  tme_uint8_t *emulator_load, *emulator_load_last;
  tme_uint8_t *emulator_load_start;
#define _TME_M68K_INSN_FETCH_SAVE \
do { \
  ic->_tme_m68k_insn_buffer_fetch_total = emulator_load - emulator_load_start; \
  ic->_tme_m68k_insn_buffer_fetch_sizes = insn_fetch_sizes; \
} while (/* CONSTCOND */ 0)
#define _TME_M68K_SEQUENCE_RESTARTING	(FALSE)
#else  /* !_TME_M68K_EXECUTE_FAST */
  unsigned int exceptions;
  tme_uint32_t linear_pc;
#define _TME_M68K_INSN_FETCH_SAVE \
do { \
  ic->_tme_m68k_insn_buffer_fetch_total = linear_pc - ic->tme_m68k_ireg_pc; \
  ic->_tme_m68k_insn_buffer_fetch_sizes = insn_fetch_sizes; \
} while (/* CONSTCOND */ 0)
#define _TME_M68K_SEQUENCE_RESTARTING	TME_M68K_SEQUENCE_RESTARTING
#endif /* !_TME_M68K_EXECUTE_FAST */
#if (_TME_M68K_EXECUTE_CPU == TME_M68K_M68020) || (_TME_M68K_EXECUTE_CPU == TME_M68K_M68030)
  unsigned int eai_function_code;
  int ea_post_index;
  unsigned int ea_i_is;
#else  /* !TME_M68K_M68020 && !TME_M68K_M68030 */
#define eai_function_code ea_function_code
#endif  /* !TME_M68K_M68020 && !TME_M68K_M68030 */
  unsigned int function_code_program;
  unsigned int function_code_data;
  tme_uint16_t opw, extword;
  const struct _tme_m68k_decoder_root *root_entry;
  const struct _tme_m68k_decoder_submap *submap_entry;
  const struct _tme_m68k_opcode *opcode;
  void (*func) _TME_P((struct tme_m68k *, void *, void *));
  int specop_type;
  void *operand0, *operand1;
  tme_uint16_t insn_fetch_sizes;
  unsigned int first_ea_extword_offset;
  int ea_size, ea_cycles;
  unsigned int ea_mode;
  int ea_reg, ea_pre_index;
  unsigned int ea_index_long, ea_index_scale;
  tme_uint32_t ea_address;
  unsigned int ea_function_code;
  tme_int32_t disp, ea_bd;
  int imm_operand, imm_size;
  tme_uint32_t imm32;
  tme_uint16_t imm16;
  tme_uint8_t imm8;
  tme_uint16_t transfer_next_before;
  int rc;

  /* get the function codes.  if the privilege ever changes as a
     result of any instruction, we must redispatch: */
  if (TME_M68K_PRIV(ic)) {
    function_code_program = TME_M68K_FC_SP;
    function_code_data = TME_M68K_FC_SD;
  }
  else {
    function_code_program = TME_M68K_FC_UP;
    function_code_data = TME_M68K_FC_UD;
  }

  /* if we have used up our burst: */
  if (ic->_tme_m68k_instruction_burst_remaining == 0) {

    /* start a new burst: */
    ic->_tme_m68k_instruction_burst_remaining
      = ic->_tme_m68k_instruction_burst;

    /* if this is a cooperative threading system, yield: */
#ifdef TME_THREADS_COOPERATIVE
    tme_thread_yield();
#endif /* TME_THREADS_COOPERATIVE */
  }

#ifdef _TME_M68K_EXECUTE_FAST

  /* get our instruction TLB entry and reload it: */
  tlb = TME_ATOMIC_READ(struct tme_m68k_tlb *, ic->_tme_m68k_itlb);
  if (!TME_M68K_TLB_OK_FAST_READ(tlb, function_code_program, ic->tme_m68k_ireg_pc, ic->tme_m68k_ireg_pc)) {
    tme_m68k_tlb_fill(ic, tlb,
		      function_code_program,
		      ic->tme_m68k_ireg_pc,
		      TME_BUS_CYCLE_READ);
  }

  /* if we have to go slow, run the slow executor: */
  if (TME_M68K_SEQUENCE_RESTARTING
      || tme_m68k_go_slow(ic)) {
    return (_TME_M68K_EXECUTE_SLOW(ic));
  }

  /* set up to do fast reads from the instruction TLB entry: */
  emulator_load_last = tlb->tme_m68k_tlb_emulator_off_read + TME_ATOMIC_READ(tme_bus_addr_t, tlb->tme_m68k_tlb_linear_last);
  ic->_tme_m68k_group0_hook = tme_m68k_group0_hook_fast;
#else  /* !_TME_M68K_EXECUTE_FAST */

  /* set up to do slow reads from the instruction TLB entry: */
  ic->_tme_m68k_group0_hook = NULL;
#endif /* !_TME_M68K_EXECUTE_FAST */

  /* the execution loop: */
  for (;;) {

    /* reset for this instruction: */
#ifdef _TME_M68K_EXECUTE_FAST
    if (__tme_predict_false(TME_ATOMIC_READ(tme_bus_addr_t, tlb->tme_m68k_tlb_linear_last) == 0)) {
      tme_m68k_redispatch(ic);
    }
    emulator_load_start = emulator_load = tlb->tme_m68k_tlb_emulator_off_read + ic->tme_m68k_ireg_pc;
    assert(TME_M68K_TLB_OK_FAST_READ(tlb, function_code_program, ic->tme_m68k_ireg_pc, ic->tme_m68k_ireg_pc)
	   || (emulator_load - 1) == emulator_load_last);
    tme_m68k_verify_begin(ic, emulator_load_start);
#else  /* !_TME_M68K_EXECUTE_FAST */
    linear_pc = ic->tme_m68k_ireg_pc;
    ic->_tme_m68k_insn_buffer_off = 0;
    exceptions = 0;
    if (__tme_predict_false(TME_M68K_FLAG_T(ic->tme_m68k_ireg_sr) != 0)) {
      exceptions |= TME_M68K_EXCEPTION_GROUP1_TRACE;
    }
    tme_m68k_verify_begin(ic, NULL);
#endif /* _TME_M68K_EXECUTE_FAST */
#ifdef _TME_M68K_VERIFY
    if (ic->tme_m68k_ireg_pc == 0x6000) {
      tme_m68k_verify_hook();
    }
#endif
    insn_fetch_sizes = 0;
    first_ea_extword_offset = sizeof(opw);
    ic->_tme_m68k_instruction_burst_remaining--;
    
    /* fetch and decode the first word of this instruction: */
    _TME_M68K_EXECUTE_FETCH_U16(opw);
    ic->_tme_m68k_insn_opcode = opw;
    root_entry = ic->_tme_m68k_decoder_root
      + TME_FIELD_EXTRACTU(opw, 6, 10);
    submap_entry = root_entry->_tme_m68k_decoder_root_submap
      + TME_FIELD_EXTRACTU(opw, 0, 6);
    opcode = root_entry->_tme_m68k_decoder_root_opcode_map
      + submap_entry->_tme_m68k_decoder_submap_opcode_map_index;
    
    /* set func: */
    func = opcode->_tme_m68k_opcode_func;
    
    /* set operand 0: */
    operand0 = submap_entry->_tme_m68k_decoder_submap_gen._tme_m68k_decoder_gen_operand0;
    if (operand0 == NULL)
      operand0 = root_entry->_tme_m68k_decoder_root_gen._tme_m68k_decoder_gen_operand0;
    
    /* set operand 1: */
    operand1 = submap_entry->_tme_m68k_decoder_submap_gen._tme_m68k_decoder_gen_operand1;
    if (operand1 == NULL)
      operand1 = root_entry->_tme_m68k_decoder_root_gen._tme_m68k_decoder_gen_operand1;
    
    /* set ea_size: */
    ea_size = submap_entry->_tme_m68k_decoder_submap_gen._tme_m68k_decoder_gen_eax_size;
    if (ea_size == TME_M68K_SIZE_SUBMAP_X)
      ea_size = root_entry->_tme_m68k_decoder_root_gen._tme_m68k_decoder_gen_eax_size;
    
    /* set imm_operand: */
    imm_operand = submap_entry->_tme_m68k_decoder_submap_gen._tme_m68k_decoder_gen_imm_operand;
    imm_size = submap_entry->_tme_m68k_decoder_submap_gen._tme_m68k_decoder_gen_imm_size;
    if (imm_operand == TME_M68K_OPNUM_SUBMAP_X) {
      imm_operand = root_entry->_tme_m68k_decoder_root_gen._tme_m68k_decoder_gen_imm_operand;
      imm_size = root_entry->_tme_m68k_decoder_root_gen._tme_m68k_decoder_gen_imm_size;
    }
    
    /* set ea_mode, ea_reg, ea_cycles and ea_function_code -
       instructions or the EA itself may override these: */
    ea_mode = TME_FIELD_EXTRACTU(opw, 3, 3);
    ea_reg = TME_M68K_IREG_A0 + TME_FIELD_EXTRACTU(opw, 0, 3);
    ea_cycles = opcode->_tme_m68k_opcode_eax_cycles;
    ea_function_code = function_code_data;
    assert(ea_size == TME_M68K_SIZE_UNDEF
	   || ea_size == TME_M68K_SIZE_UNSIZED
	   || ea_cycles != TME_BUS_CYCLE_UNDEF);
      
    /* dispatch on the special operand specifier type: */
    specop_type = opcode->_tme_m68k_opcode_specop_type;
    if (__tme_predict_false(specop_type != TME_M68K_SPECOP_UNDEF)) {
      switch (specop_type) {
	
	/* a Bcc may have an extended displacement, and we need to
	   translate BF to BSR: */
      case TME_M68K_SPECOP_BCC:
	disp = TME_EXT_S8_S32((tme_int8_t) opw);
	if (disp == 0) {
	  _TME_M68K_EXECUTE_FETCH_S16(disp);
	}
#if (_TME_M68K_EXECUTE_CPU == TME_M68K_M68020) || (_TME_M68K_EXECUTE_CPU == TME_M68K_M68030)
	else if (disp == -255) {
	  _TME_M68K_EXECUTE_FETCH_S32(disp);
	}
#endif /* TME_M68K_M68020 || TME_M68K_M68030 */
	imm32 = (tme_uint32_t) disp;
	operand0 = &imm32;
	if (TME_FIELD_EXTRACTU(opw, 8, 4) == TME_M68K_C_F) {
	  func = tme_m68k_bsr;
	}
	break;
	
#if (_TME_M68K_EXECUTE_CPU == TME_M68K_M68020) || (_TME_M68K_EXECUTE_CPU == TME_M68K_M68030)
	/* these are similar: */
      case TME_M68K_SPECOP_DIVUL:
      case TME_M68K_SPECOP_MULUL:
	_TME_M68K_EXECUTE_FETCH_U16(specop);
	first_ea_extword_offset = 4;
	if (specop & TME_BIT(11)) {
	  /* this is really a signed operation, override
	     m68k-opmap-make.sh's guess that it is unsigned: */
	  func = ((opcode->_tme_m68k_opcode_specop_type == TME_M68K_SPECOP_DIVUL)
		  ? tme_m68k_divsl
		  : tme_m68k_mulsl);
	}
	break;
#endif /* TME_M68K_M68020 || TME_M68K_M68030 */
	
#if (_TME_M68K_EXECUTE_CPU != TME_M68K_M68000)
	/* moves: */
      case TME_M68K_SPECOP_MOVES:

	/* fetch the remainder of the instruction and decide what EA
	   function code and cycles we need: */
	_TME_M68K_EXECUTE_FETCH_U16(ic->_tme_m68k_insn_specop);
	first_ea_extword_offset = 4;
	if (ic->_tme_m68k_insn_specop & TME_BIT(11)) {
	  ea_cycles = TME_BUS_CYCLE_WRITE;
	  ea_function_code = ic->tme_m68k_ireg_dfc;
	}
	else {
	  ea_cycles = TME_BUS_CYCLE_READ;
	  ea_function_code = ic->tme_m68k_ireg_sfc;
	}

	/* if we're not privileged, don't fetch any EA, and run the
	   priv function instead: */
	if (!TME_M68K_PRIV(ic)) {
	  func = tme_m68k_priv;
	  ea_size = TME_M68K_SIZE_UNDEF;
	}
	break;
#endif /* !TME_M68K_M68000 */

	/* many instructions have a single special extension word: */
      case TME_M68K_SPECOP_SPECOP16:
	_TME_M68K_EXECUTE_FETCH_U16(ic->_tme_m68k_insn_specop);
	first_ea_extword_offset = 4;
	break;

	/* all of these always have two specop words: */
      case TME_M68K_SPECOP_CAS2:
	_TME_M68K_EXECUTE_FETCH_U16(ic->_tme_m68k_insn_specop);
	_TME_M68K_EXECUTE_FETCH_U16(ic->_tme_m68k_insn_specop2);
	first_ea_extword_offset = 6;
	break;
	
	/* a memory-to-memory move instruction has additional ea mode
	   and reg fields, but we don't need to do anything for that
	   yet: */
      case TME_M68K_SPECOP_MOVEMEMTOMEM:
	break;

	/* a nonmemory-to-memory move instruction has additional ea
	   mode and reg fields, and we use them now: */
      case TME_M68K_SPECOP_MOVENONMEMTOMEM:
	ea_mode = TME_FIELD_EXTRACTU(opw, 6, 3);
	ea_reg = TME_M68K_IREG_A0 + TME_FIELD_EXTRACTU(opw, 9, 3);
	assert(ea_cycles == TME_BUS_CYCLE_WRITE);
	break;
	
	/* the illegal specop makes sure not to fetch any EAs or
           immediates: */
      case TME_M68K_SPECOP_ILLEGAL:
	ea_size = TME_M68K_SIZE_UNDEF;
	imm_operand = TME_M68K_OPNUM_UNDEF;
	break;
	
      default: abort();
      }
    }
    
    /* get any immediate operand: */
    if (__tme_predict_false(imm_operand != TME_M68K_OPNUM_UNDEF)) {
      switch (imm_size) {
      case TME_M68K_SIZE_16:
	_TME_M68K_EXECUTE_FETCH_U16(imm16);
	if (imm_operand == 0) operand0 = &imm16; else operand1 = &imm16;
	break;
      case TME_M68K_SIZE_32:
	_TME_M68K_EXECUTE_FETCH_U32(imm32);
	if (imm_operand == 0) operand0 = &imm32; else operand1 = &imm32;
	break;
      case TME_M68K_SIZE_16S32:
	_TME_M68K_EXECUTE_FETCH_S16(imm32);
	if (imm_operand == 0) operand0 = &imm32; else operand1 = &imm32;
	break;
      case TME_M68K_SIZE_16U8:
	_TME_M68K_EXECUTE_FETCH_U16(imm8);
	if (imm_operand == 0) operand0 = &imm8; else operand1 = &imm8;
	break;
      default: assert(FALSE);
      }
    }
    
    /* loop over up to two effective addresses calculations.  this
       initializes for the normal, single effective address: */
    while (ea_size != TME_M68K_SIZE_UNDEF) {
      
      /* this EA must have either no size, or be exactly one, two, or
	 four bytes: */
      assert(ea_size == TME_M68K_SIZE_UNSIZED
	     || ea_size == TME_M68K_SIZE_8
	     || ea_size == TME_M68K_SIZE_16
	     || ea_size == TME_M68K_SIZE_32);

      /* for the effective address predecrement and postincrement
	 modes, we require that these size macros correspond exactly
	 to the number of bytes, that the %a7 register number be 15,
	 and that the ea reg not be greater than %a7: */
#if TME_M68K_SIZE_UNSIZED != 0
#error "TME_M68K_SIZE_UNSIZED must be 0"
#endif
#if TME_M68K_SIZE_8 != 1
#error "TME_M68K_SIZE_8 must be 1"
#endif
#if TME_M68K_SIZE_16 != 2
#error "TME_M68K_SIZE_16 must be 2"
#endif
#if TME_M68K_SIZE_32 != 4
#error "TME_M68K_SIZE_32 must be 4"
#endif
#if TME_M68K_IREG_A7 != 15
#error "TME_M68K_IREG_A7 must be 15"
#endif
      assert(ea_reg <= TME_M68K_IREG_A7);
#define TME_M68K_AREG_INCREMENT(areg, size) \
  (((((areg) + 1) / (TME_M68K_IREG_A7 + 1)) & (size)) + (size))

      /* initialize ea_address to silence -Wuninitialized: */
      ea_address = 0;

      /* set the EA inner function code: */
      eai_function_code = ea_function_code;

      /* dispatch on the mode: */
      switch (ea_mode) {
	
	/* address register indirect: */
      case 2:
	ea_address = ic->tme_m68k_ireg_uint32(ea_reg);
	break;
	  
	/* address register indirect postincrement: */
      case 3:
	/* if we are not restarting, set the effective address: */
	if (!_TME_M68K_SEQUENCE_RESTARTING) {
	  ea_address = ic->tme_m68k_ireg_uint32(ea_reg);
	  ic->tme_m68k_ireg_uint32(ea_reg) += TME_M68K_AREG_INCREMENT(ea_reg, ea_size);
	}
	break;
	  
	/* address register indirect predecrement: */
      case 4:
	/* if we are not restarting, set the effective address: */
	if (!_TME_M68K_SEQUENCE_RESTARTING) {
	  ic->tme_m68k_ireg_uint32(ea_reg) -= TME_M68K_AREG_INCREMENT(ea_reg, ea_size);
	  ea_address = ic->tme_m68k_ireg_uint32(ea_reg);
	}
	break;
	  
	/* address register indirect with 16-bit displacement: */
      case 5:
	_TME_M68K_EXECUTE_FETCH_S16(ea_bd);
	ea_address = ic->tme_m68k_ireg_uint32(ea_reg) + ea_bd;
	break;
	  
	/* miscellaneous modes: */
      case 7:
	  
	/* absolute short addressing: */
	if (ea_reg == TME_M68K_IREG_A0) {
	  _TME_M68K_EXECUTE_FETCH_S16(ea_address);
	  break;
	}	    
	  
	/* absolute long addressing: */
	if (ea_reg == TME_M68K_IREG_A1) {
	  _TME_M68K_EXECUTE_FETCH_S32(ea_address);
	  break;
	}	    
	  
	/* program counter indirect with 16-bit displacement: */
	if (ea_reg == TME_M68K_IREG_A2) {
	  _TME_M68K_EXECUTE_FETCH_S16(ea_bd);
	  /* XXX simulates preincremented pc: */
	  ea_address = ic->tme_m68k_ireg_pc + first_ea_extword_offset + ea_bd;
	  ea_function_code = function_code_program;
	  break;
	}
	  
	/* everything else is just like mode 6 except with the PC as
	   the base register: */
	assert (ea_reg == TME_M68K_IREG_A3);
	ea_reg = TME_M68K_IREG_PC;
	eai_function_code = function_code_program;
	/* FALLTHROUGH */
	  
	/* various indexed modes: */
      case 6:
	  
	/* fetch the extension word and take it apart.  the 68000 and
	   68010 ignore the scale field in the extension word and always
	   behave as if it is zero: */
	_TME_M68K_EXECUTE_FETCH_U16(extword);
	ea_pre_index = TME_M68K_IREG_D0 + TME_FIELD_EXTRACTU(extword, 12, 4);
	ea_index_long = (extword & TME_BIT(11));
#if (_TME_M68K_EXECUTE_CPU == TME_M68K_M68020) || (_TME_M68K_EXECUTE_CPU == TME_M68K_M68030)
	ea_index_scale = TME_FIELD_EXTRACTU(extword, 9, 2);
#else  /* !TME_M68K_M68020 && !TME_M68K_M68030 */
	ea_index_scale = 0;
#endif /* !TME_M68K_M68020 && !TME_M68K_M68030 */
	  
	/* if this is a full extension word: */
	if (extword & TME_BIT(8)) {
#if (_TME_M68K_EXECUTE_CPU == TME_M68K_M68020) || (_TME_M68K_EXECUTE_CPU == TME_M68K_M68030)

	  ea_i_is = TME_FIELD_EXTRACTU(extword, 0, 3);

	  /* optionally suppress the base register: */
	  if (extword & TME_BIT(7)) {
	    ea_reg = TME_M68K_IREG_ZERO32;
	  }
	    
	  /* fetch any base displacement: */
	  switch (TME_FIELD_EXTRACTU(extword, 4, 2)) {
	  case 0: abort();
	  case 1: ea_bd = 0; break;
	  case 2: _TME_M68K_EXECUTE_FETCH_S16(ea_bd); break;
	  case 3: _TME_M68K_EXECUTE_FETCH_S32(ea_bd); break;
	  }
	    
	  /* optionally suppress the index register.  this is also
	     where we check for combined IS-I/IS fields greater than
	     or equal to 0xc, which are reserved: */
	  if (extword & TME_BIT(6)) {
	    ea_pre_index = TME_M68K_IREG_ZERO32;
	    if (ea_i_is >= 0x4) {
	      abort();
	    }
	  }

	  /* fetch any outer displacement: */
	  switch (ea_i_is & 3) {
	  case 0: case 1: ea_od = 0; break;
	  case 2: _TME_M68K_EXECUTE_FETCH_S16(ea_od); break;
	  case 3: _TME_M68K_EXECUTE_FETCH_S32(ea_od); break;
	  }

	  /* dispatch on the I/IS fields: */
	  ea_post_index = TME_M68K_IREG_ZERO32;
	  switch (ea_i_is) {

	    /* no memory indirect action: */
	  case 0x0:
	    ea_post_index = TME_M68K_IREG_UNDEF;
	    break;
	    
	    /* indirect preindexed with null outer displacement: */
	    /* indirect preindexed with word outer displacement: */
	    /* indirect preindexed with long outer displacement: */
	  case 0x1: case 0x2: case 0x3:
	    break;

	    /* reserved: */
	  case 0x4: default: abort();

	    /* indirect postindexed with null outer displacement: */
	    /* indirect postindexed with word outer displacement: */
	    /* indirect postindexed with long outer displacement: */
	  case 0x5: case 0x6: case 0x7:
	    ea_post_index = ea_pre_index;
	    ea_pre_index = TME_M68K_IREG_ZERO32;
	    break;
	  }

	  /* preindex and base-displace the original address register
	     to arrive at the indirect EA: */
	  ea_address = 
	    (ic->tme_m68k_ireg_uint32(ea_reg)
	     + ((ea_index_long
		 ? ic->tme_m68k_ireg_int32(ea_pre_index)
		 : ((tme_int32_t) ic->tme_m68k_ireg_int16(ea_pre_index << 1)))
		<< ea_index_scale)
	     + ea_bd
	     + (ea_reg == TME_M68K_IREG_PC
		/* XXX simulates preincremented pc: */
		? first_ea_extword_offset
		: 0));
	  
	  /* if this is a memory indirect, read the indirect EA.
	     don't disturb the EA in the IC state if we're restarting,
	     for two reasons:

	     first, the value in the IC state may belong to some later
	     part of the instruction handling, in which case we must
	     (continue to) preserve it, and
	     
	     second, if the EA in the IC state *is* from this part of
	     the instruction handling, it's correct, while our EA may
	     *not* be correct, since it was generated from IC state
	     that may have changed since the instruction originally
	     started (i.e., address register changes by the user or by
	     our own postincrement/predecrement, or function code
	     register changes by the user): */
	  if (ea_post_index != TME_M68K_IREG_UNDEF) {
	    if (!_TME_M68K_SEQUENCE_RESTARTING) {
	      ic->_tme_m68k_ea_address = ea_address;
	      ic->_tme_m68k_ea_function_code = eai_function_code;
	    }
	    _TME_M68K_INSN_FETCH_SAVE;
	    tme_m68k_read_mem32(ic, TME_M68K_IREG_MEMY32);
	    ea_address =
	      (ic->tme_m68k_ireg_memy32
	       + ((ea_index_long
		   ? ic->tme_m68k_ireg_int32(ea_post_index)
		   : ((tme_int32_t) ic->tme_m68k_ireg_int16(ea_post_index << 1)))
		  << ea_index_scale)
	       + ea_od);
	  }
	  else {
	    ea_function_code = eai_function_code;
	  }
	  
#else  /* !TME_M68K_M68020 && !TME_M68K_M68030 */
	  /* XXX - illegal instruction */
	  abort();
#endif /* !TME_M68K_M68020 && !TME_M68K_M68030 */
	}
	
	/* otherwise, this is a brief extension word: */
	else {
	  ea_address = 
	    (ic->tme_m68k_ireg_uint32(ea_reg)
	     + ((tme_int32_t) ((tme_int8_t) (extword & 0xff)))
	     + ((ea_index_long
		 ? ic->tme_m68k_ireg_int32(ea_pre_index)
		 : ((tme_int32_t) ic->tme_m68k_ireg_int16(ea_pre_index << 1)))
		<< ea_index_scale)
	     + (ea_reg == TME_M68K_IREG_PC
		/* XXX simulates preincremented pc: */
		? first_ea_extword_offset
		: 0));
	  ea_function_code = eai_function_code;
	}
	break;

      default: assert(FALSE);
      }

      /* we have calculated the effective address.  we don't store it
	 if we're restarting, because it may have been calculated
	 using user-visible registers (address registers and even
	 function code registers!) that the user may have changed (or,
	 in the case of pre/postdecrement EAs, that *we* may have
	 changed) between the bus fault and the instruction restart.
	 when we restart an instruction we *always* want to use the
	 same effective address as before: */
      if (!_TME_M68K_SEQUENCE_RESTARTING) {
	ic->_tme_m68k_ea_address = ea_address;
	ic->_tme_m68k_ea_function_code = eai_function_code;
      }
      
      /* XXX XXX XXX - if we detect a store to program space, that's an illegal: */
      /* XXX but maybe not for moves? */
      if (ea_function_code == function_code_program
	  && (ea_cycles & TME_BUS_CYCLE_WRITE)) {
	abort();
      }

      /* if we're loading this operand: */
      if (ea_cycles & TME_BUS_CYCLE_READ) {
	_TME_M68K_INSN_FETCH_SAVE;
	(*_tme_m68k_read_memx[ea_size])(ic);
      }

      /* stop unless this is a memory-to-memory move: */
      if (specop_type != TME_M68K_SPECOP_MOVEMEMTOMEM)
	break;

      /* reload for the other memory EA at the same size: */
      ea_mode = TME_FIELD_EXTRACTU(opw, 6, 3);
      ea_reg = TME_M68K_IREG_A0 + TME_FIELD_EXTRACTU(opw, 9, 3);
      ea_cycles = TME_BUS_CYCLE_WRITE;
      ea_function_code = function_code_data;
      specop_type = TME_M68K_SPECOP_UNDEF;
    }

    /* we've fetched all of the instruction words: */
    _TME_M68K_INSN_FETCH_SAVE;

    /* set the next PC: */
#ifdef _TME_M68K_EXECUTE_FAST
    ic->tme_m68k_ireg_pc_next = ic->tme_m68k_ireg_pc + (emulator_load - emulator_load_start);
#else  /* !_TME_M68K_EXECUTE_FAST */
    ic->tme_m68k_ireg_pc_next = linear_pc;
#endif /* !_TME_M68K_EXECUTE_FAST */

    /* if we're not restarting, or if this instruction function can
       fault, call the instruction function: */
    if (!_TME_M68K_SEQUENCE_RESTARTING
	|| (ic->_tme_m68k_mode_flags & TME_M68K_EXECUTION_INST_CANFAULT)) {
      transfer_next_before = ic->_tme_m68k_sequence._tme_m68k_sequence_transfer_next;
      (*func)(ic, operand0, operand1);
      assert(!(ic->_tme_m68k_mode_flags & TME_M68K_EXECUTION_INST_CANFAULT)
	     != (ic->_tme_m68k_sequence._tme_m68k_sequence_transfer_next
		 != transfer_next_before));
      ic->_tme_m68k_mode_flags &= ~TME_M68K_EXECUTION_INST_CANFAULT;
    }

    /* store up to one EA path: */
    if (ea_size != TME_M68K_SIZE_UNDEF
	&& (ea_cycles & TME_BUS_CYCLE_WRITE)) {
      (*_tme_m68k_write_memx[ea_size])(ic);
    }

    /* an instruction has ended: */
    tme_m68k_verify_end(ic, func);

    /* update the PC: */
    ic->tme_m68k_ireg_pc = ic->tme_m68k_ireg_pc_next;
    TME_M68K_SEQUENCE_START;

#ifdef _TME_M68K_EXECUTE_FAST
    /* if we haven't finished the instruction burst yet, continue: */
    if (__tme_predict_true(ic->_tme_m68k_instruction_burst_remaining != 0)) {
      continue;
    }
#endif /* _TME_M68K_EXECUTE_FAST */

    /* try to acquire the external mutex and check for external
       resets, halts, or interrupts, and process them along
       with any internal exceptions: */
    rc = tme_mutex_trylock(&ic->tme_m68k_external_mutex);
    if (TME_THREADS_ERRNO(rc) == TME_OK) {
      tme_m68k_external_check(ic, 
#ifdef _TME_M68K_EXECUTE_FAST
			      0
#else  /* !_TME_M68K_EXECUTE_FAST */
			      exceptions
#endif /* !_TME_M68K_EXECUTE_FAST */
			      );

      /* unlock the external mutex: */
      tme_mutex_unlock(&ic->tme_m68k_external_mutex);
    }

#ifndef _TME_M68K_EXECUTE_FAST

    /* otherwise, if we have internal exceptions, process them: */
    else if (exceptions) {
      tme_m68k_exception(ic, exceptions);
    }

    /* if we can go fast now, go fast: */
    if (!tme_m68k_go_slow(ic)) {
      tme_m68k_redispatch(ic);
    }

    /* otherwise, unless we've used up our burst, continue: */
    if (ic->_tme_m68k_instruction_burst_remaining != 0) {
      continue;
    }

#endif /* !_TME_M68K_EXECUTE_FAST */

    /* start a new burst: */
    ic->_tme_m68k_instruction_burst_remaining
      = ic->_tme_m68k_instruction_burst;

    /* if this is a cooperative threading system, yield: */
#ifdef TME_THREADS_COOPERATIVE
    tme_thread_yield();
#endif /* TME_THREADS_COOPERATIVE */

  }
  /* NOTREACHED */

#ifdef _TME_M68K_EXECUTE_FAST

  /* if we get here, we "faulted" trying to fetch an instruction word
     from host memory.  it's possibly not a "real" fault, since this
     instruction may simply cross a page boundary, but since the fast
     executor can't restart instructions we have to treat this like a
     group 0 fault: */
 _tme_m68k_fast_fetch_failed:

  /* mimic a group 0 exception: */
  _TME_M68K_INSN_FETCH_SAVE;
  ic->_tme_m68k_group0_flags = TME_M68K_BUS_CYCLE_FETCH | TME_M68K_BUS_CYCLE_READ;
  ic->_tme_m68k_group0_function_code = function_code_program;
  ic->_tme_m68k_group0_address = ic->tme_m68k_ireg_pc + (emulator_load - emulator_load_start);
  ic->_tme_m68k_group0_sequence = ic->_tme_m68k_sequence;
  ic->_tme_m68k_group0_sequence._tme_m68k_sequence_transfer_faulted_after = 0;
  ic->_tme_m68k_group0_buffer_read_size = 0;
  ic->_tme_m68k_group0_buffer_read_softrr = 0;
  tme_m68k_group0_hook_fast(ic);
  ic->_tme_m68k_group0_sequence._tme_m68k_sequence_transfer_faulted =
    ic->_tme_m68k_group0_sequence._tme_m68k_sequence_transfer_next;

  /* mimic the rte: */
  ic->_tme_m68k_sequence = ic->_tme_m68k_group0_sequence;
  ic->_tme_m68k_sequence._tme_m68k_sequence_transfer_next = 1;
  TME_M68K_SEQUENCE_RESTART;

  tme_m68k_redispatch(ic);
  /* NOTREACHED */
#endif /* _TME_M68K_EXECUTE_FAST */
}
