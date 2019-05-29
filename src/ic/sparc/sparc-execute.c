/* $Id: sparc-execute.c,v 1.5 2007/03/29 01:06:59 fredette Exp $ */

/* ic/sparc/sparc-execute.c - executes SPARC instructions: */

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

_TME_RCSID("$Id: sparc-execute.c,v 1.5 2007/03/29 01:06:59 fredette Exp $");

/* includes: */
#include "sparc-auto.h"

#if (TME_SPARC_VERSION(ic) < 9)
#define tme_sparc_ireg_t tme_uint32_t
#define tme_sparc_ireg(x)  tme_sparc_ireg_uint32(x)
#define tme_sparc_idle_type_pc tme_sparc_idle_type_pc32
#else  /* TME_SPARC_VERSION(ic) >= 9 */
#define tme_sparc_ireg_t tme_uint64_t
#define tme_sparc_ireg(x)  tme_sparc_ireg_uint64(x)
#define tme_sparc_idle_type_pc tme_sparc_idle_type_pc64
#endif /* TME_SPARC_VERSION(ic) >= 9 */

/* the sparc instruction executor: */
static void
_TME_SPARC_EXECUTE_NAME(struct tme_sparc *ic)
{
  tme_uint32_t asi_mask_insn;
  tme_uint32_t asi_mask_data;
  struct tme_sparc_tlb *itlb_current;
  struct tme_sparc_tlb itlb_invalid;
  tme_sparc_ireg_t pc;
  tme_uint32_t insn;
  unsigned int opcode;
  unsigned int reg_rs1;
  unsigned int reg_rs2;
  unsigned int reg_rd;
  int annulled;
  int rc;
  tme_uint8_t conds_mask_icc;
  tme_uint8_t conds_mask_fcc;
  tme_uint16_t conds_mask;
  unsigned int cond;

  /* get the default address space identifiers and masks: */
  if (TME_SPARC_VERSION(ic) < 9) {
    if (TME_SPARC_PRIV(ic)) {
      asi_mask_insn = TME_SPARC32_ASI_MASK_SI;
      asi_mask_data = TME_SPARC32_ASI_MASK_SD;
    }
    else {
      asi_mask_insn = TME_SPARC32_ASI_MASK_UI;
      asi_mask_data = TME_SPARC32_ASI_MASK_UD;
    }
  }
  else {
    asi_mask_data
      = ((ic->tme_sparc64_ireg_pstate & TME_SPARC64_PSTATE_CLE)
	 ? TME_SPARC64_ASI_MASK_PRIMARY_LITTLE
	 : TME_SPARC64_ASI_MASK_PRIMARY);
    asi_mask_insn = asi_mask_data;
  }
  ic->tme_sparc_asi_mask_insn = asi_mask_insn;
  ic->tme_sparc_asi_mask_data = asi_mask_data;

  /* create an invalid instruction TLB entry, and use it as the initial
     current instruction TLB entry: */
  tme_bus_tlb_construct(&itlb_invalid.tme_sparc_tlb_bus_tlb);
  itlb_invalid.tme_sparc_tlb_addr_first = 1;
  itlb_invalid.tme_sparc_tlb_addr_last = 0;
  itlb_current = &itlb_invalid;

  /* busy the invalid instruction TLB entry: */
  assert (ic->_tme_sparc_itlb_busy == NULL);
  tme_sparc_tlb_busy(itlb_current);
  ic->_tme_sparc_itlb_busy = itlb_current;

  /* the first instruction will not be annulled: */
  annulled = FALSE;

  for (;;) {

    /* if we have used up our instruction burst: */
    if (__tme_predict_false(ic->_tme_sparc_instruction_burst_remaining == 0)) {

      /* try to acquire the external mutex and check for external
	 resets, halts, or interrupts, and process them: */
      rc = tme_mutex_trylock(&ic->tme_sparc_external_mutex);
      if (__tme_predict_true(TME_THREADS_ERRNO(rc) == TME_OK)) {
#if TME_SPARC_VERSION(ic) < 9
	tme_sparc32_external_check(ic);
#else  /* TME_SPARC_VERSION(ic) >= 9 */
	tme_sparc64_external_check(ic);
#endif /* TME_SPARC_VERSION(ic) >= 9 */

	/* unlock the external mutex: */
	tme_mutex_unlock(&ic->tme_sparc_external_mutex);
      }

      /* start a new instruction burst: */
      ic->_tme_sparc_instruction_burst_remaining
	= ic->_tme_sparc_instruction_burst;

      /* if we are in the idle loop: */
      if (__tme_predict_false((ic->tme_sparc_ireg(TME_SPARC_IREG_PC_NEXT)
			       == ic->tme_sparc_idle_type_pc)
			      && TME_SPARC_IDLE_TYPE_IS(ic,
							(TME_SPARC_IDLE_TYPE_SUNOS32_TYPE_0)))) {
	tme_sparc_do_idle(ic);
      }

      /* if this is a cooperative threading system: */
#if TME_THREADS_COOPERATIVE

      /* unbusy the current instruction TLB entry: */
      assert (ic->_tme_sparc_itlb_busy == itlb_current);
      tme_sparc_tlb_unbusy(itlb_current);
      ic->_tme_sparc_itlb_busy = NULL;

      /* yield: */
      tme_thread_yield();
#endif /* TME_THREADS_COOPERATIVE */
    }

    /* we are going to use one instruction in the burst: */
    ic->_tme_sparc_instruction_burst_remaining--;
#ifdef _TME_SPARC_STATS
    ic->tme_sparc_stats.tme_sparc_stats_insns_total++;
#endif /* _TME_SPARC_STATS */

    /* update the PCs and get the PC of the instruction to execute: */
    pc = ic->tme_sparc_ireg(TME_SPARC_IREG_PC_NEXT);
    ic->tme_sparc_ireg(TME_SPARC_IREG_PC) = pc;
    ic->tme_sparc_ireg(TME_SPARC_IREG_PC_NEXT) = ic->tme_sparc_ireg(TME_SPARC_IREG_PC_NEXT_NEXT);
    ic->tme_sparc_ireg(TME_SPARC_IREG_PC_NEXT_NEXT) += sizeof(tme_uint32_t);

    /* NB that we only save instruction TLB entries that allow fast
       reading, and we also change tme_sparc_tlb_addr_last to be the
       last PC covered by the entry (it's normally the last address
       covered by the entry).  this allows us to do minimal checking
       of the current instruction TLB entry at itlb_current: */

    /* if the current instruction TLB entry covers this address: */
    if (__tme_predict_true(itlb_current->tme_sparc_tlb_addr_first <= pc
			   && pc <= itlb_current->tme_sparc_tlb_addr_last)) {

      /* fetch the instruction: */
      insn = tme_memory_bus_read32((const tme_shared tme_uint32_t *) (itlb_current->tme_sparc_tlb_emulator_off_read + pc),
				   itlb_current->tme_sparc_tlb_bus_rwlock,
				   sizeof(tme_uint32_t),
				   sizeof(tme_sparc_ireg_t));
      insn = tme_betoh_u32(insn);
    }

    /* otherwise, our current TLB entry doesn't cover this address: */
    else {

      /* unbusy the current instruction TLB entry: */
      assert (ic->_tme_sparc_itlb_busy == itlb_current);
      tme_sparc_tlb_unbusy(itlb_current);

      /* rehash the current instruction TLB entry: */
      itlb_current = tme_memory_atomic_pointer_read(struct tme_sparc_tlb *,
						    ic->_tme_sparc_itlb_array,
						    &ic->_tme_sparc_tlb_rwlock);
      itlb_current += (pc >> 10) % _TME_SPARC_ITLB_HASH_SIZE;

      /* busy the current instruction TLB entry: */
      tme_sparc_tlb_busy(itlb_current);
      ic->_tme_sparc_itlb_busy = itlb_current;

      /* if the new current instruction TLB entry is valid and covers
         this address: */
      if (tme_bus_tlb_is_valid(&itlb_current->tme_sparc_tlb_bus_tlb)
	  && __tme_predict_true(TME_SPARC_TLB_ASI_MASK_OK(itlb_current, asi_mask_insn)
				&& itlb_current->tme_sparc_tlb_addr_first <= pc
				&& pc <= itlb_current->tme_sparc_tlb_addr_last)) {

	/* fetch the instruction: */
	insn = tme_memory_bus_read32((const tme_shared tme_uint32_t *) (itlb_current->tme_sparc_tlb_emulator_off_read + pc),
				     itlb_current->tme_sparc_tlb_bus_rwlock,
				     sizeof(tme_uint32_t),
				     sizeof(tme_sparc_ireg_t));
	insn = tme_betoh_u32(insn);
      }

      /* otherwise, the new current instruction TLB entry is not valid
         or does not cover this address: */
      else {

	/* we never fill TLB entries on the stack because we never
	   callout multiple fills at the same time, so the global TLB
	   entry pointer always points back to the TLB entry.  this
	   also means that we don't have to call tme_bus_tlb_back()
	   after the fill: */
	itlb_current->tme_sparc_tlb_bus_tlb.tme_bus_tlb_global = &itlb_current->tme_sparc_tlb_bus_tlb;

	/* loop until we can busy a valid TLB entry: */
	do {

	  /* unbusy the current instruction TLB entry for filling: */
	  tme_bus_tlb_unbusy_fill(&itlb_current->tme_sparc_tlb_bus_tlb);

	  /* fill the current instruction TLB entry: */
#ifdef _TME_SPARC_STATS
	  ic->tme_sparc_stats.tme_sparc_stats_itlb_fill++;
#endif /* _TME_SPARC_STATS */
	  (*ic->_tme_sparc_bus_connection->tme_sparc_bus_tlb_fill)
	    (ic->_tme_sparc_bus_connection, 
	     itlb_current,
	     asi_mask_insn,
	     ic->tme_sparc_ireg(TME_SPARC_IREG_PC),
	     TME_BUS_CYCLE_READ);

	  /* busy the current instruction TLB entry: */
	  tme_sparc_tlb_busy(itlb_current);

	} while (tme_bus_tlb_is_invalid(&itlb_current->tme_sparc_tlb_bus_tlb));

	/* the current instruction TLB entry must now cover this
           address and allow reading: */
	/* NB that tme_sparc_tlb_addr_last has not been changed yet: */
	assert (TME_SPARC_TLB_ASI_MASK_OK(itlb_current, asi_mask_insn)
		&& itlb_current->tme_sparc_tlb_addr_first <= pc
		&& pc <= itlb_current->tme_sparc_tlb_addr_last
		&& (itlb_current->tme_sparc_tlb_emulator_off_read != TME_EMULATOR_OFF_UNDEF
		    || (itlb_current->tme_sparc_tlb_cycles_ok & TME_BUS_CYCLE_READ)));

	/* if this current instruction TLB entry covers the entire
	   instruction and allows fast reading: */
	if (__tme_predict_true(itlb_current->tme_sparc_tlb_addr_last >= (pc + (sizeof(tme_uint32_t) - 1))
			       && itlb_current->tme_sparc_tlb_emulator_off_read != TME_EMULATOR_OFF_UNDEF)) {

	  /* fetch the instruction: */
	  insn = tme_memory_bus_read32((const tme_shared tme_uint32_t *) (itlb_current->tme_sparc_tlb_emulator_off_read + pc),
				       itlb_current->tme_sparc_tlb_bus_rwlock,
				       sizeof(tme_uint32_t),
				       sizeof(tme_sparc_ireg_t));
	  insn = tme_betoh_u32(insn);

	  /* modify tme_sparc_tlb_addr_last of this first to represent the last valid
	     PC covered by the entry: */
	  itlb_current->tme_sparc_tlb_addr_last
	    &= (((tme_bus_addr_t) 0) - sizeof(tme_uint32_t));
	}

	/* otherwise, this instruction TLB entry does not cover the
	   entire instruction and/or it does not allow fast reading.
	   fetching an instruction here may mean making multiple bus
	   cycles.  exactly what happens in this case is
	   implementation-dependent, so we call an
	   implementation-specific function here to handle it: */
	else {

	  /* unbusy the current instruction TLB entry and poison it,
	     so we won't try to do any fast fetches with it: */
	  assert (ic->_tme_sparc_itlb_busy == itlb_current);
	  tme_sparc_tlb_unbusy(itlb_current);
	  itlb_current->tme_sparc_tlb_addr_first = 1;
	  itlb_current->tme_sparc_tlb_addr_last = 0;
	  ic->_tme_sparc_itlb_busy = NULL;

	  /* NB that while annulled instructions are always fetched,
	     fetching an annulled instruction can never generate an
	     instruction_access exception, since the annul bit is not
	     part of the architected state.  so we pass in the
	     annulled indication to the slow instruction fetcher: */
	  insn = (*ic->_tme_sparc_fetch_slow)(ic, annulled);
#ifdef _TME_SPARC_STATS
	  ic->tme_sparc_stats.tme_sparc_stats_insns_slow++;
#endif /* _TME_SPARC_STATS */

	  /* busy the invalid instruction TLB entry: */
	  itlb_current = &itlb_invalid;
	  assert (ic->_tme_sparc_itlb_busy == NULL);
	  tme_sparc_tlb_busy(itlb_current);
	  ic->_tme_sparc_itlb_busy = itlb_current;
	}
      }

      /* if this instruction has been annulled: */
      if (__tme_predict_false(annulled)) {

	/* the netbsd32-type-0 idle type is detected when an annulled
	   "wr %g1, PSR_PIL, %psr" or "wr %l1, (IPL_SCHED << 8), %psr"
	   instruction is found four instructions after (in
	   disassembly order, not execution order) a "wr %g1, 0, %psr"
	   or "wr %l1, 0, %psr" instruction that sets PIL to 0x0: */
	if (__tme_predict_false((insn
				 & ~((31 << 25)		/* rd (reserved) */
				     | (0x10 << 14)	/* rs1 (mask %ln to %gn) */
				     | (0x4 << 8)))	/* imm13 (mask PSR_PIL to (IPL_SCHED << 8)) */
				== ((tme_uint32_t)
				    (2 << 30)		/* format */
				    | (0x31 << 19)	/* op3 (wrpsr) */
				    | (0x01 << 14)	/* rs1 (%g1) */
				    | (1 << 13)		/* i */
				    | 0x0b00))) {	/* imm13 (IPL_SCHED << 8) */
	  if (TME_SPARC_IDLE_TYPE_IS(ic, TME_SPARC_IDLE_TYPE_NETBSD32_TYPE_0)) {
	    if (ic->tme_sparc_ireg(TME_SPARC_IREG_PC)
		== (ic->tme_sparc_idle_type_pc
		    - TME_SPARC_IDLE_TYPE_PC_STATE(1)
		    + (sizeof(tme_uint32_t) * 4))) {
	      ic->tme_sparc_idle_type_pc = ic->tme_sparc_ireg(TME_SPARC_IREG_PC);
	    }
	    if (ic->tme_sparc_ireg(TME_SPARC_IREG_PC)
		== ic->tme_sparc_idle_type_pc) {
	      tme_sparc_do_idle(ic);
	    }
	  }
	}	   

	/* make this instruction a nop: */
	insn = 0x01000000;
      }

      /* the next instruction will not be annulled: */
      annulled = FALSE;
    }

    /* start this instruction: */
    ic->_tme_sparc_insn = insn;
#ifdef _TME_SPARC_VERIFY
    if (ic->tme_sparc_ireg(TME_SPARC_IREG_PC) == 0x6000) {
      tme_sparc_verify_hook();
    }
#endif

    /* set %g0 to zero: */
    ic->tme_sparc_ireg(TME_SPARC_IREG_G0) = 0;

    /* if this is a format three instruction (op is two or three): */
    if (__tme_predict_true(insn >= 0x80000000)) {

      /* if the i bit is zero: */
      if (__tme_predict_true((insn & TME_BIT(13)) == 0)) {

	/* decode rs2: */
	reg_rs2 = TME_FIELD_MASK_EXTRACTU(insn, TME_SPARC_FORMAT3_MASK_RS2);
	TME_SPARC_REG_INDEX(ic, reg_rs2);
      }

      /* otherwise, the i bit is one: */
      else {

	/* decode simm13: */
	ic->tme_sparc_ireg(TME_SPARC_IREG_IMM) = TME_FIELD_MASK_EXTRACTS(insn, (tme_sparc_ireg_t) 0x1fff);
	reg_rs2 = TME_SPARC_IREG_IMM;
      }

      /* decode rs1: */
      reg_rs1 = TME_FIELD_MASK_EXTRACTU(insn, TME_SPARC_FORMAT3_MASK_RS1);
      TME_SPARC_REG_INDEX(ic, reg_rs1);

      /* decode rd: */
      reg_rd = TME_FIELD_MASK_EXTRACTU(insn, TME_SPARC_FORMAT3_MASK_RD);
      TME_SPARC_REG_INDEX(ic, reg_rd);
    
      /* form the opcode index: */
      opcode = TME_FIELD_MASK_EXTRACTU(insn, (0x3f << 19));
      opcode += ((insn >> (30 - 6)) & 0x40);
      
      /* run the instruction: */
      (*_TME_SPARC_EXECUTE_OPMAP[opcode])
	(ic, 
	 &ic->tme_sparc_ireg(reg_rs1),
	 &ic->tme_sparc_ireg(reg_rs2),
	 &ic->tme_sparc_ireg(reg_rd));
    }

    /* otherwise, if this is a format two instruction: */
    else if (__tme_predict_true(insn < 0x40000000)) {

      /* dispatch on op2: */
      switch (TME_FIELD_MASK_EXTRACTU(insn, (0x7 << 22))) {
      default:

      case 0: /* UNIMP: */
#if TME_SPARC_VERSION(ic) < 9
	tme_sparc32_trap(ic, TME_SPARC_TRAP_illegal_instruction);
#else  /* TME_SPARC_VERSION(ic) >= 9 */
	tme_sparc64_trap(ic, TME_SPARC_TRAP_illegal_instruction);
#endif /* TME_SPARC_VERSION(ic) >= 9 */
	continue;

      case 2: /* Bicc: */
	conds_mask_icc = _tme_sparc_conds_icc[
#if TME_SPARC_VERSION(ic) < 9
	  TME_FIELD_MASK_EXTRACTU(ic->tme_sparc32_ireg_psr, TME_SPARC32_PSR_ICC)
#else  /* TME_SPARC_VERSION(ic) >= 9 */
	  TME_FIELD_MASK_EXTRACTU(ic->tme_sparc64_ireg_ccr, TME_SPARC64_CCR_ICC)
#endif /* TME_SPARC_VERSION(ic) >= 9 */
	];

	/* add the not-conditions to the conditions mask: */
	conds_mask = conds_mask_icc ^ 0xff;
	conds_mask = (conds_mask << 8) | conds_mask_icc;
	break;

      case 4: /* SETHI: */

	/* decode rd: */
	reg_rd = TME_FIELD_MASK_EXTRACTU(insn, TME_SPARC_FORMAT3_MASK_RD);
	TME_SPARC_REG_INDEX(ic, reg_rd);
	ic->tme_sparc_ireg(reg_rd) = (insn << 10);
	continue;

      case 6: /* FBfcc: */
	TME_SPARC_INSN_FPU;
	conds_mask_fcc = _tme_sparc_conds_fcc[TME_FIELD_MASK_EXTRACTU(ic->tme_sparc_fpu_fsr, TME_SPARC_FSR_FCC)];

	/* add the not-conditions to the conditions mask: */
	conds_mask = conds_mask_fcc ^ 0xff;
	conds_mask = (conds_mask << 8) | conds_mask_fcc;
	break;
      }

      /* get the condition field: */
      cond = TME_FIELD_MASK_EXTRACTU(insn, (0xf << 25));

      /* if this conditional branch is taken: */
      if (conds_mask & TME_BIT(cond)) {

	/* do the delayed control transfer: */
	ic->tme_sparc_ireg(TME_SPARC_IREG_PC_NEXT_NEXT)
	  = (ic->tme_sparc_ireg(TME_SPARC_IREG_PC)
	     + (TME_FIELD_MASK_EXTRACTS(insn, (tme_sparc_ireg_t) 0x003fffff) << 2));

	/* if this is a delayed control transfer to the idle loop: */
	if (__tme_predict_false(ic->tme_sparc_ireg(TME_SPARC_IREG_PC_NEXT_NEXT)
				== ic->tme_sparc_idle_type_pc)) {
	  if (TME_SPARC_IDLE_TYPE_IS(ic, (TME_SPARC_IDLE_TYPE_SUNOS32_TYPE_0))) {

	    /* reduce this instruction burst to include only the delay
	       instruction.  before the next instruction burst begins
	       (on the first instruction of the idle loop) we will
	       detect that and go idle: */
	    ic->_tme_sparc_instruction_burst_remaining = 1;
	  }
	}

	/* if this was a conditional branch, clear the annul bit in
           the instruction image: */
	if (cond & 7) {
	  insn &= ~TME_BIT(29);
	}
      }

      /* if the annul bit it set: */
      if (insn & TME_BIT(29)) {

	/* the next instruction will be annulled.  to get the
	   execution loop to pay attention to the annulled bit,
	   make the current instruction TLB entry invalid: */
	annulled = TRUE;
	assert (ic->_tme_sparc_itlb_busy == itlb_current);
	tme_sparc_tlb_unbusy(itlb_current);
	itlb_current = &itlb_invalid;
	tme_sparc_tlb_busy(itlb_current);
	ic->_tme_sparc_itlb_busy = itlb_current;
	/* NB that we have to make sure the next instruction gets executed
	   in the immediate next iteration of the execution loop, since the
	   annulled bit is not part of the architected CPU state, and we also
	   want to do good emulation and actually fetch the instruction (as
	   opposed to just advancing the PCs now).  to do this, we make sure
	   that there is at least one more instruction left in the burst: */
	ic->_tme_sparc_instruction_burst_remaining += (ic->_tme_sparc_instruction_burst_remaining == 0);
      }
    }

    /* otherwise, this is a format one instruction: */
    else {

      /* get the current PC: */
      pc = ic->tme_sparc_ireg(TME_SPARC_IREG_PC);

      /* write the PC of the CALL into r[15]: */
      ic->tme_sparc_ireg(ic->tme_sparc_cwp_offset + 15) = pc;

      /* log the call: */
      tme_sparc_log(ic, 250, TME_OK,
		    (TME_SPARC_LOG_HANDLE(ic),
		     _("call 0x%08x"),
		     pc + (insn << 2)));

      /* do the delayed control transfer: */
      ic->tme_sparc_ireg(TME_SPARC_IREG_PC_NEXT_NEXT) = pc + (insn << 2);
    }
  }
  
  /* NOTREACHED */
}
