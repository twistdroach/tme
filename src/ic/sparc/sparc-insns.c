/* $Id: sparc-insns.c,v 1.5 2007/03/29 01:07:46 fredette Exp $ */

/* ic/sparc/sparc-insns.c - SPARC instruction functions: */

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

/* includes: */
#include "sparc-impl.h"

_TME_RCSID("$Id: sparc-insns.c,v 1.5 2007/03/29 01:07:46 fredette Exp $");

#include "sparc-bus-auto.c"

TME_SPARC_FORMAT3(tme_sparc32_illegal, tme_uint32_t)
{
  TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_illegal_instruction);
}

/* the coprocessor instructions are all illegal for now: */
TME_SPARC_FORMAT3(tme_sparc32_cpop1, tme_uint32_t)
{
  TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_illegal_instruction);
}
TME_SPARC_FORMAT3(tme_sparc32_cpop2, tme_uint32_t)
{
  TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_illegal_instruction);
}
TME_SPARC_FORMAT3(tme_sparc32_ldc, tme_uint32_t)
{
  TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_illegal_instruction);
}
TME_SPARC_FORMAT3(tme_sparc32_ldcsr, tme_uint32_t)
{
  TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_illegal_instruction);
}
TME_SPARC_FORMAT3(tme_sparc32_lddc, tme_uint32_t)
{
  TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_illegal_instruction);
}
TME_SPARC_FORMAT3(tme_sparc32_stc, tme_uint32_t)
{
  TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_illegal_instruction);
}
TME_SPARC_FORMAT3(tme_sparc32_stcsr, tme_uint32_t)
{
  TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_illegal_instruction);
}
TME_SPARC_FORMAT3(tme_sparc32_stdc, tme_uint32_t)
{
  TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_illegal_instruction);
}
TME_SPARC_FORMAT3(tme_sparc32_stdcq, tme_uint32_t)
{
  TME_SPARC_INSN_PRIV;
  TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_illegal_instruction);
}

TME_SPARC_FORMAT3(tme_sparc32_rdasr, tme_uint32_t)
{
  unsigned int reg_rs1;
  unsigned int reg_rd;
  tme_uint32_t value;

  reg_rs1 = TME_FIELD_MASK_EXTRACTU(TME_SPARC_INSN, (0x1f << 14));
  reg_rd = TME_FIELD_MASK_EXTRACTU(TME_SPARC_INSN, TME_SPARC_FORMAT3_MASK_RD);

  /* rdy: */
  if (reg_rs1 == 0) {
    value = ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_Y);
  }

  /* stbar: */
  else if (reg_rs1 == 15 && reg_rd == 0) {
    TME_SPARC_INSN_OK;
  }

  else {

    /* all other rdasr instructions are privileged: */
    TME_SPARC_INSN_PRIV;

    value = 0;
    abort();
  }

  TME_SPARC_FORMAT3_RD = value;
  TME_SPARC_INSN_OK;
}

TME_SPARC_FORMAT3(tme_sparc32_rdpsr, tme_uint32_t)
{
  TME_SPARC_INSN_PRIV;
  TME_SPARC_FORMAT3_RD = ic->tme_sparc32_ireg_psr;

  /* the sunos32-type-0 idle type is detected when a "rd %psr, %l0"
     instruction is followed three instructions later (in disassembly
     order, not execution order) by a "mov %g1, %psr" that sets PIL
     to 0xa, and the next write to %psr is a "mov %g1, %psr" that sets
     PIL to 0x0 and is followed by a branch (to the idle loop): */
  if (__tme_predict_false((TME_SPARC_INSN
			   & ~((31 << 14)		/* rs1 (reserved) */
			       | (1 << 13)		/* i (reserved) */
			       | 0x1fff))		/* imm13 (reserved) */
			  == ((tme_uint32_t)
			      (2 << 30)			/* format */
			      | (0x10 << 25)		/* rd (%l0) */
			      | (0x29 << 19)))) {	/* op3 (rdpsr) */
    /* if we haven't detected the idle loop yet, remember the address
       of this "rd %psr, %l0" instruction and enter state one: */
    if (__tme_predict_false(TME_SPARC_IDLE_TYPE_PC_STATE(ic->tme_sparc_idle_type_pc32) != 0)) {
      if (TME_SPARC_IDLE_TYPE_IS(ic, TME_SPARC_IDLE_TYPE_SUNOS32_TYPE_0)) {
	ic->tme_sparc_idle_type_pc32
	  = (ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC)
	     + TME_SPARC_IDLE_TYPE_PC_STATE(1));
      }
    }
  }

  TME_SPARC_INSN_OK;
}

TME_SPARC_FORMAT3(tme_sparc32_rdwim, tme_uint32_t)
{
  TME_SPARC_INSN_PRIV;
  TME_SPARC_FORMAT3_RD = ic->tme_sparc32_ireg_wim;
  TME_SPARC_INSN_OK;
}

TME_SPARC_FORMAT3(tme_sparc32_rdtbr, tme_uint32_t)
{
  TME_SPARC_INSN_PRIV;
  TME_SPARC_FORMAT3_RD = ic->tme_sparc32_ireg_tbr;
  TME_SPARC_INSN_OK;
}

TME_SPARC_FORMAT3(tme_sparc32_wrasr, tme_uint32_t)
{
  unsigned int reg_rd;
  tme_uint32_t value_xor;
  
  reg_rd = TME_FIELD_MASK_EXTRACTU(TME_SPARC_INSN, TME_SPARC_FORMAT3_MASK_RD);
  value_xor = TME_SPARC_FORMAT3_RS1 ^ TME_SPARC_FORMAT3_RS2;

  /* "WRY ... writes r[rs1] xor r[rs2] if the i field is zero, or
     r[rs1] xor sign_ext(simm13) if the i field is one, to the
     writable fields of the specified IU state register. (Note the
     exclusive-or operation.)  Note that WRY is distinguished from
     WRASR only by the rd field. The rd field must be zero and op3 =
     0x30 to write the Y register." */
  if (reg_rd == 0) {
    ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_Y) = value_xor;
  }

  else {

    /* all other wrasr instructions are privileged: */
    TME_SPARC_INSN_PRIV;

    abort();
  }

  TME_SPARC_INSN_OK;
}

TME_SPARC_FORMAT3(tme_sparc32_wrpsr, tme_uint32_t)
{
  tme_uint32_t value;
  tme_uint32_t mask_writable;
  tme_uint32_t insn;
  unsigned int cwp;

  TME_SPARC_INSN_PRIV;

  /* "WRPSR ... writes r[rs1] xor r[rs2] if the i field is zero, or
     r[rs1] xor sign_ext(simm13) if the i field is one, to the
     writable fields of the specified IU state register. (Note the
     exclusive-or operation.)" */
  value = TME_SPARC_FORMAT3_RS1 ^ TME_SPARC_FORMAT3_RS2;

  /* "If the result of a WRPSR instruction would cause the CWP field
     of the PSR to point to an unimplemented window, it causes an
     illegal_instruction trap and does not write the PSR." */
  cwp = TME_FIELD_MASK_EXTRACTU(value, TME_SPARC32_PSR_CWP);
  if (__tme_predict_false(cwp >= ic->tme_sparc_nwindows)) {
    TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_illegal_instruction);
  }

  /* set the new CWP offset: */
  ic->tme_sparc_cwp_offset = TME_SPARC_CWP_OFFSET(cwp);

  /* set the new PSR value: */
  mask_writable = (TME_SPARC32_PSR_ICC
		   | TME_SPARC32_PSR_EC
		   | TME_SPARC32_PSR_EF
		   | TME_SPARC32_PSR_PIL
		   | TME_SPARC32_PSR_S
		   | TME_SPARC32_PSR_PS
		   | TME_SPARC32_PSR_ET
		   | TME_SPARC32_PSR_CWP);
  value = (value & mask_writable) | (ic->tme_sparc32_ireg_psr & ~mask_writable);
  ic->tme_sparc32_ireg_psr = value;

  /* the netbsd32-type-0 idle type is detected when an annulled "wr
     %g1, PSR_PIL, %psr" or "wr %l1, (IPL_SCHED << 8), %psr"
     instruction is found four instructions after (in disassembly
     order, not execution order) a "wr %g1, 0, %psr" or "wr %l1, 0,
     %psr" instruction that sets PIL to 0x0: */
  if (__tme_predict_false((TME_SPARC_INSN
			   & ~((31 << 25)	/* rd (reserved) */
			       | (0x10 << 14)))	/* rs1 (mask %ln to %gn) */
			  == ((tme_uint32_t)
			      (2 << 30)		/* format */
			      | (0x31 << 19)	/* op3 (wrpsr) */
			      | (0x01 << 14)	/* rs1 (%g1) */
			      | (1 << 13)	/* i */
			      | 0x0000))) {	/* imm13 (0) */
    if (__tme_predict_false(TME_SPARC_IDLE_TYPE_PC_STATE(ic->tme_sparc_idle_type_pc32) != 0)) {
      if (TME_SPARC_IDLE_TYPE_IS(ic, TME_SPARC_IDLE_TYPE_NETBSD32_TYPE_0)
	  && ((value & TME_SPARC32_PSR_PIL)
	      == (0x0 * _TME_FIELD_MASK_FACTOR(TME_SPARC32_PSR_PIL)))) {
	ic->tme_sparc_idle_type_pc32
	  = (ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC)
	     + TME_SPARC_IDLE_TYPE_PC_STATE(1));
      }
    }
  }

  /* the sunos32-type-0 idle type is detected when a "rd %psr, %l0"
     instruction is followed three instructions later (in disassembly
     order, not execution order) by a "mov %g1, %psr" that sets PIL
     to 0xa, and the next write to %psr is a "mov %g1, %psr" that sets
     PIL to 0x0 and is followed by a branch (to the idle loop): */
  if (__tme_predict_false(TME_SPARC_IDLE_TYPE_IS(ic, TME_SPARC_IDLE_TYPE_SUNOS32_TYPE_0))) {

    /* if we haven't detected the idle loop yet: */
    if (__tme_predict_false(TME_SPARC_IDLE_TYPE_PC_STATE(ic->tme_sparc_idle_type_pc32) != 0)) {

      /* if this is a "mov %g1, %psr" instruction: */
      if (__tme_predict_false((TME_SPARC_INSN
			       & ~((31 << 25)		/* rd (reserved) */
				   | 255 << 5))		/* unused (zero) */
			      == ((tme_uint32_t)
				  (2 << 30)		/* format */
				  | (0x31 << 19)	/* op3 (wrpsr) */
				  | (0x01 << 14)	/* rs1 (%g1) */
				  | (0 << 13)		/* i */
				  | (0 << 0)))) {	/* rs2 (%g0) */

	/* if this "mov %g1, %psr" instruction is three instructions
	   in disassembly order after a "rd %psr, %l0" instruction and
	   sets PIL to 0xa: */
	if ((ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC)
	     == (ic->tme_sparc_idle_type_pc32
		 - TME_SPARC_IDLE_TYPE_PC_STATE(1)
		 + (sizeof(tme_uint32_t) * 3)))
	    && ((value & TME_SPARC32_PSR_PIL)
		== (0xa * _TME_FIELD_MASK_FACTOR(TME_SPARC32_PSR_PIL)))) {

	  /* advance to the next state: */
	  ic->tme_sparc_idle_type_pc32 = TME_SPARC_IDLE_TYPE_PC_STATE(3);
	}

	/* otherwise, if we detected a "mov %g1, %psr" instruction
	   three instructions in disassembly order after a "rd %psr,
	   %l0" instruction, and this "mov %g1, %psr" instruction sets
	   PIL to 0x0, and the next instruction is an unconditional
	   branch with an annulled delay slot: */
	else if (ic->tme_sparc_idle_type_pc32 == TME_SPARC_IDLE_TYPE_PC_STATE(3)
		 && ((value & TME_SPARC32_PSR_PIL)
		     == (0x0 * _TME_FIELD_MASK_FACTOR(TME_SPARC32_PSR_PIL)))
		 && (((insn = tme_sparc_fetch_nearby(ic, 1))
		      & ~((0x3fffff << 0)))	/* disp22 */
		     == ((0 << 30)		/* format */
			 | (1 << 29)		/* a */
			 | (8 << 25)		/* cond (Always) */
			 | (2 << 22)))) {	/* op2 (bicc) */
	  
	  /* we have detected the address of the idle loop: */
	  ic->tme_sparc_idle_type_pc32
	    = (ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC)
	       + sizeof(tme_uint32_t)
	       + (TME_FIELD_MASK_EXTRACTS(insn, (tme_uint32_t) 0x003fffff) << 2));
	}

	/* otherwise, this is some other "mov %g1, %psr" instruction,
	   or we couldn't fetch the next instruction or it isn't the
	   expected branch: */
	else {

	  /* poison the idle type state: */
	  ic->tme_sparc_idle_type_pc32 = TME_SPARC_IDLE_TYPE_PC_STATE(1);
	}
      }

      /* otherwise, this is some other wrpsr instruction: */
      else {

	/* poison the idle type state: */
	ic->tme_sparc_idle_type_pc32 = TME_SPARC_IDLE_TYPE_PC_STATE(1);
      }
    }
  }

  /* redispatch, since the executor may have cached information
     derived from the PSR (for example, default data ASI, ITLB, etc.)
     that we need to invalidate: */
  tme_sparc_redispatch(ic);
  /* NOTREACHED */
}

TME_SPARC_FORMAT3(tme_sparc32_wrwim, tme_uint32_t)
{
  tme_uint32_t value;

  TME_SPARC_INSN_PRIV;

  /* "WRWIM ... writes r[rs1] xor r[rs2] if the i field is zero, or
     r[rs1] xor sign_ext(simm13) if the i field is one, to the
     writable fields of the specified IU state register. (Note the
     exclusive-or operation.)" */
  value = TME_SPARC_FORMAT3_RS1 ^ TME_SPARC_FORMAT3_RS2;

  /* "A WRWIM with all bits set to 1, followed by a RDWIM, yields a
     bit vector in which the imple- mented windows (and only the
     implemented windows) are indicated by 1s." */
  value &= (0xffffffff >> (32 - ic->tme_sparc_nwindows));
  
  ic->tme_sparc32_ireg_wim = value;

  TME_SPARC_INSN_OK;
}

TME_SPARC_FORMAT3(tme_sparc32_wrtbr, tme_uint32_t)
{
  tme_uint32_t value;

  TME_SPARC_INSN_PRIV;

  /* "WRTBR ... writes r[rs1] xor r[rs2] if the i field is zero, or
     r[rs1] xor sign_ext(simm13) if the i field is one, to the
     writable fields of the specified IU state register. (Note the
     exclusive-or operation.)" */
  value = TME_SPARC_FORMAT3_RS1 ^ TME_SPARC_FORMAT3_RS2;

  /* "Bits 11 through 4 comprise the trap type (tt) field.  This 8-bit
     field is written by the hardware when a trap occurs, and retains
     its value until the next trap.  It provides an offset into the
     trap table.  The WRTBR instruction does not affect the tt
     field.  TBR_zero (0) Bits 3 through 0 are zeroes.  The WRTBR
     instruction does not affect this field.  For future compatibility,
     supervisor software should only issue a WRTBR instruction with a
     zero value in this field." */
  value = (value & 0xfffff000) | (ic->tme_sparc32_ireg_tbr & 0x00000ff0);
  ic->tme_sparc32_ireg_tbr = value;

  TME_SPARC_INSN_OK;
}

TME_SPARC_FORMAT3(tme_sparc32_flush, tme_uint32_t)
{
  /* nothing to do */
  TME_SPARC_INSN_OK;
}

TME_SPARC_FORMAT3(tme_sparc32_rett, tme_uint32_t)
{
  tme_uint32_t psr;
  unsigned int cwp;
  tme_uint32_t pc_next_next;

  /* "One of several traps may occur when an RETT is executed.  These
     are described in priority order (highest priority first):

     If traps are enabled (ET=1) and the processor is in user mode
     (S=0), a privileged_instruction trap occurs.

     If traps are enabled (ET=1) and the processor is in supervisor
     mode (S=1), an illegal_instruction trap occurs.

     If traps are disabled (ET=0), and (a) the processor is in user
     mode (S=0), or (b) a window_underflow condition is detected (WIM
     and 2^new_CWP ) = 1, or (c) either of the low-order two bits of
     the target address is nonzero, then the processor indicates a
     trap condition of (a) privileged_instruction, (b)
     window_underflow, or (c) mem_address_not_aligned (respectively)
     in the tt field of the TBR register, and enters the error_mode
     state." */
  psr = ic->tme_sparc32_ireg_psr;
  
  if (__tme_predict_false((psr & TME_SPARC32_PSR_S) == 0)) {
    TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_privileged_instruction);
  }

  if (__tme_predict_false((psr & TME_SPARC32_PSR_ET) != 0)) {
    TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_illegal_instruction);
  }
  
  cwp = TME_FIELD_MASK_EXTRACTU(psr, TME_SPARC32_PSR_CWP);
  cwp += 1;
  cwp %= ic->tme_sparc_nwindows;
  if (ic->tme_sparc32_ireg_wim & (((tme_uint32_t) 1) << cwp)) {
    TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_window_underflow);
  }

  pc_next_next = TME_SPARC_FORMAT3_RS1 + TME_SPARC_FORMAT3_RS2;
  if (__tme_predict_false((pc_next_next % sizeof(tme_uint32_t)) != 0)) {
    TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_mem_address_not_aligned);
  }

  /* set the new PSR: */
  TME_FIELD_MASK_DEPOSITU(psr, TME_SPARC32_PSR_CWP, cwp);
  psr |= TME_SPARC32_PSR_ET;
  psr &= ~TME_SPARC32_PSR_S;
  psr |= ((psr & TME_SPARC32_PSR_PS) * (TME_SPARC32_PSR_S / TME_SPARC32_PSR_PS));
  ic->tme_sparc32_ireg_psr = psr;

  /* set the new CWP offset: */
  ic->tme_sparc_cwp_offset = TME_SPARC_CWP_OFFSET(cwp);

  /* set the delayed control transfer: */
  ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC_NEXT_NEXT) = pc_next_next;

  /* redispatch, since the executor may have cached information
     derived from the PSR (for example, default data ASI, ITLB, etc.)
     that we need to invalidate: */
  tme_sparc_redispatch(ic);
}

TME_SPARC_FORMAT3(tme_sparc32_save_restore, tme_uint32_t)
{
  int direction;
  tme_uint32_t psr;
  unsigned int cwp;
  unsigned int cwp_offset;
  unsigned int reg_rd;

  /* calculate the window direction: */
  direction = -1 + (((TME_SPARC_INSN & TME_BIT(19)) != 0) * 2);

  /* calculate the new CWP: */
  psr = ic->tme_sparc32_ireg_psr;
  cwp = TME_FIELD_MASK_EXTRACTU(psr, TME_SPARC32_PSR_CWP);
  cwp += direction;
  cwp %= ic->tme_sparc_nwindows;

  /* if the new window is invalid: */
  if (__tme_predict_false((ic->tme_sparc32_ireg_wim & (((tme_uint32_t) 1) << cwp)) != 0)) {
    TME_SPARC_INSN_TRAP((direction < 0)
			? TME_SPARC_TRAP_window_overflow
			: TME_SPARC_TRAP_window_underflow);
  }

  /* write the new PSR: */
  TME_FIELD_MASK_DEPOSITU(psr, TME_SPARC32_PSR_CWP, cwp);
  ic->tme_sparc32_ireg_psr = psr;

  /* set the new CWP offset: */
  cwp_offset = TME_SPARC_CWP_OFFSET(cwp);
  ic->tme_sparc_cwp_offset = cwp_offset;

  /* decode rd: */
  reg_rd = TME_FIELD_MASK_EXTRACTU(TME_SPARC_INSN, TME_SPARC_FORMAT3_MASK_RD);
  TME_SPARC_REG_INDEX(ic, reg_rd);

  /* do the add: */
  ic->tme_sparc_ireg_uint32(reg_rd) = TME_SPARC_FORMAT3_RS1 + TME_SPARC_FORMAT3_RS2;

  TME_SPARC_INSN_OK;
}

TME_SPARC_FORMAT3(tme_sparc32_ticc, tme_uint32_t)
{
  tme_uint8_t conds_mask_icc;
  tme_uint16_t conds_mask;
  unsigned int cond;

  conds_mask_icc = _tme_sparc_conds_icc[TME_FIELD_MASK_EXTRACTU(ic->tme_sparc32_ireg_psr, TME_SPARC32_PSR_ICC)];

  /* add the not-conditions to the conditions mask: */
  conds_mask = conds_mask_icc ^ 0xff;
  conds_mask = (conds_mask << 8) | conds_mask_icc;

  /* get the condition field: */
  cond = TME_FIELD_MASK_EXTRACTU(TME_SPARC_INSN, (0xf << 25));

  /* if this condition is true: */
  if (conds_mask & TME_BIT(cond)) {
    TME_SPARC_INSN_TRAP(TME_SPARC_TRAP_trap_instruction((TME_SPARC_FORMAT3_RS1 + TME_SPARC_FORMAT3_RS2) & 0x7f));
  }

  TME_SPARC_INSN_OK;
}

TME_SPARC_FORMAT3(tme_sparc32_fpop1, tme_uint32_t)
{
  tme_sparc_fpu_fpop1(ic);
  TME_SPARC_INSN_OK;
}
TME_SPARC_FORMAT3(tme_sparc32_fpop2, tme_uint32_t)
{
  tme_sparc_fpu_fpop2(ic);
  TME_SPARC_INSN_OK;
}

#include "sparc-insns-auto.c"
