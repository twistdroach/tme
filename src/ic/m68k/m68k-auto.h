/* automatically generated by m68k-misc-auto.sh, do not edit! */

#ifndef _IC_M68K_MISC_H
#define _IC_M68K_MISC_H

/* the register mapping: */
#define TME_M68K_IREG_UNDEF		(-1)
#define TME_M68K_IREG_D0		(0)
#define tme_m68k_ireg_d0		tme_m68k_ireg_uint32(TME_M68K_IREG_D0)
#define TME_M68K_IREG_D1		(1)
#define tme_m68k_ireg_d1		tme_m68k_ireg_uint32(TME_M68K_IREG_D1)
#define TME_M68K_IREG_D2		(2)
#define tme_m68k_ireg_d2		tme_m68k_ireg_uint32(TME_M68K_IREG_D2)
#define TME_M68K_IREG_D3		(3)
#define tme_m68k_ireg_d3		tme_m68k_ireg_uint32(TME_M68K_IREG_D3)
#define TME_M68K_IREG_D4		(4)
#define tme_m68k_ireg_d4		tme_m68k_ireg_uint32(TME_M68K_IREG_D4)
#define TME_M68K_IREG_D5		(5)
#define tme_m68k_ireg_d5		tme_m68k_ireg_uint32(TME_M68K_IREG_D5)
#define TME_M68K_IREG_D6		(6)
#define tme_m68k_ireg_d6		tme_m68k_ireg_uint32(TME_M68K_IREG_D6)
#define TME_M68K_IREG_D7		(7)
#define tme_m68k_ireg_d7		tme_m68k_ireg_uint32(TME_M68K_IREG_D7)
#define TME_M68K_IREG_A0		(8)
#define tme_m68k_ireg_a0		tme_m68k_ireg_uint32(TME_M68K_IREG_A0)
#define TME_M68K_IREG_A1		(9)
#define tme_m68k_ireg_a1		tme_m68k_ireg_uint32(TME_M68K_IREG_A1)
#define TME_M68K_IREG_A2		(10)
#define tme_m68k_ireg_a2		tme_m68k_ireg_uint32(TME_M68K_IREG_A2)
#define TME_M68K_IREG_A3		(11)
#define tme_m68k_ireg_a3		tme_m68k_ireg_uint32(TME_M68K_IREG_A3)
#define TME_M68K_IREG_A4		(12)
#define tme_m68k_ireg_a4		tme_m68k_ireg_uint32(TME_M68K_IREG_A4)
#define TME_M68K_IREG_A5		(13)
#define tme_m68k_ireg_a5		tme_m68k_ireg_uint32(TME_M68K_IREG_A5)
#define TME_M68K_IREG_A6		(14)
#define tme_m68k_ireg_a6		tme_m68k_ireg_uint32(TME_M68K_IREG_A6)
#define TME_M68K_IREG_A7		(15)
#define tme_m68k_ireg_a7		tme_m68k_ireg_uint32(TME_M68K_IREG_A7)
#define TME_M68K_IREG_PC		(16)
#define tme_m68k_ireg_pc		tme_m68k_ireg_uint32(TME_M68K_IREG_PC)
#define TME_M68K_IREG_PC_NEXT		(17)
#define tme_m68k_ireg_pc_next		tme_m68k_ireg_uint32(TME_M68K_IREG_PC_NEXT)
#define TME_M68K_IREG_PC_LAST		(18)
#define tme_m68k_ireg_pc_last		tme_m68k_ireg_uint32(TME_M68K_IREG_PC_LAST)
#define tme_m68k_ireg_sr		tme_m68k_ireg_uint16(19 << 1)
#define tme_m68k_ireg_ccr		tme_m68k_ireg_uint8(19 << 2)
#define TME_M68K_IREG_SHADOW_SR	(20 << 1)
#define tme_m68k_ireg_shadow_sr	tme_m68k_ireg_uint16(TME_M68K_IREG_SHADOW_SR)
#define TME_M68K_IREG_FORMAT_OFFSET	((20 << 1) + 1)
#define tme_m68k_ireg_format_offset	tme_m68k_ireg_uint16(TME_M68K_IREG_FORMAT_OFFSET)
#define TME_M68K_IREG_MEMX32		(21)
#define tme_m68k_ireg_memx32		tme_m68k_ireg_uint32(TME_M68K_IREG_MEMX32)
#define TME_M68K_IREG_MEMX16		(21 << 1)
#define tme_m68k_ireg_memx16		tme_m68k_ireg_uint16(TME_M68K_IREG_MEMX16)
#define TME_M68K_IREG_MEMX8		(21 << 2)
#define tme_m68k_ireg_memx8		tme_m68k_ireg_uint8(TME_M68K_IREG_MEMX8)
#define TME_M68K_IREG_MEMY32		(22)
#define tme_m68k_ireg_memy32		tme_m68k_ireg_uint32(TME_M68K_IREG_MEMY32)
#define TME_M68K_IREG_MEMY16		(22 << 1)
#define tme_m68k_ireg_memy16		tme_m68k_ireg_uint16(TME_M68K_IREG_MEMY16)
#define TME_M68K_IREG_MEMY8		(22 << 2)
#define tme_m68k_ireg_memy8		tme_m68k_ireg_uint8(TME_M68K_IREG_MEMY8)
#define TME_M68K_IREG_MEMZ32		(23)
#define tme_m68k_ireg_memz32		tme_m68k_ireg_uint32(TME_M68K_IREG_MEMZ32)
#define TME_M68K_IREG_MEMZ16		(23 << 1)
#define tme_m68k_ireg_memz16		tme_m68k_ireg_uint16(TME_M68K_IREG_MEMZ16)
#define TME_M68K_IREG_MEMZ8		(23 << 2)
#define tme_m68k_ireg_memz8		tme_m68k_ireg_uint8(TME_M68K_IREG_MEMZ8)
#define TME_M68K_IREG_USP		(24)
#define tme_m68k_ireg_usp		tme_m68k_ireg_uint32(TME_M68K_IREG_USP)
#define TME_M68K_IREG_ISP		(25)
#define tme_m68k_ireg_isp		tme_m68k_ireg_uint32(TME_M68K_IREG_ISP)
#define TME_M68K_IREG_MSP		(26)
#define tme_m68k_ireg_msp		tme_m68k_ireg_uint32(TME_M68K_IREG_MSP)
#define TME_M68K_IREG_SFC		(27)
#define tme_m68k_ireg_sfc		tme_m68k_ireg_uint32(TME_M68K_IREG_SFC)
#define TME_M68K_IREG_DFC		(28)
#define tme_m68k_ireg_dfc		tme_m68k_ireg_uint32(TME_M68K_IREG_DFC)
#define TME_M68K_IREG_VBR		(29)
#define tme_m68k_ireg_vbr		tme_m68k_ireg_uint32(TME_M68K_IREG_VBR)
#define TME_M68K_IREG_CACR		(30)
#define tme_m68k_ireg_cacr		tme_m68k_ireg_uint32(TME_M68K_IREG_CACR)
#define TME_M68K_IREG_CAAR		(31)
#define tme_m68k_ireg_caar		tme_m68k_ireg_uint32(TME_M68K_IREG_CAAR)
#define TME_M68K_IREG32_COUNT		(32)
#define TME_M68K_IREG_IMM32		(32)
#define tme_m68k_ireg_imm32		tme_m68k_ireg_uint32(TME_M68K_IREG_IMM32)
#define TME_M68K_IREG_EA		(33)
#define tme_m68k_ireg_ea		tme_m68k_ireg_uint32(TME_M68K_IREG_EA)
#define TME_M68K_IREG_ZERO		(34)
#define TME_M68K_IREG_ONE		(35)
#define TME_M68K_IREG_TWO		(36)
#define TME_M68K_IREG_THREE		(37)
#define TME_M68K_IREG_FOUR		(38)
#define TME_M68K_IREG_FIVE		(39)
#define TME_M68K_IREG_SIX		(40)
#define TME_M68K_IREG_SEVEN		(41)
#define TME_M68K_IREG_EIGHT		(42)

#endif /* _IC_M68K_MISC_H */

#ifdef _TME_M68K_EXECUTE_FAST

/* these macros are for the fast executor: */

/* on all hosts, 
   this loads a 16-bit unsigned value for the fast instruction executor: */
#undef _TME_M68K_EXECUTE_FETCH_U16
#define _TME_M68K_EXECUTE_FETCH_U16(v) \
  if ((emulator_load + (sizeof(tme_uint16_t) - 1)) > emulator_load_last) \
    goto _tme_m68k_fast_fetch_failed; \
  tme_memory_aligned_rdlock(tlb->tme_m68k_tlb_bus_rwlock); \
  (v) = (tme_uint16_t) tme_betoh_u16(*((tme_uint16_t *) emulator_load)); \
  tme_memory_aligned_unlock(tlb->tme_m68k_tlb_bus_rwlock); \
  insn_fetch_sizes <<= 1; \
  emulator_load += sizeof(tme_uint16_t)

/* on all hosts, 
   this loads a 16-bit signed value for the fast instruction executor: */
#undef _TME_M68K_EXECUTE_FETCH_S16
#define _TME_M68K_EXECUTE_FETCH_S16(v) \
  if ((emulator_load + (sizeof(tme_uint16_t) - 1)) > emulator_load_last) \
    goto _tme_m68k_fast_fetch_failed; \
  tme_memory_aligned_rdlock(tlb->tme_m68k_tlb_bus_rwlock); \
  (v) = (tme_int16_t) tme_betoh_u16(*((tme_uint16_t *) emulator_load)); \
  tme_memory_aligned_unlock(tlb->tme_m68k_tlb_bus_rwlock); \
  insn_fetch_sizes <<= 1; \
  emulator_load += sizeof(tme_int16_t)

/* on a host with any alignment, 
   this loads a 32-bit unsigned value for the fast instruction executor: */
#if ALIGNOF_INT32_T <= ALIGNOF_INT16_T
#undef _TME_M68K_EXECUTE_FETCH_U32
#define _TME_M68K_EXECUTE_FETCH_U32(v) \
  if ((emulator_load + (sizeof(tme_uint32_t) - 1)) > emulator_load_last) \
    goto _tme_m68k_fast_fetch_failed; \
  if (((unsigned long) emulator_load) & (sizeof(tme_uint32_t) - 1)) { \
    tme_memory_unaligned_rdlock(tlb->tme_m68k_tlb_bus_rwlock); \
    (v) = (tme_uint32_t) tme_betoh_u32(*((tme_uint32_t *) emulator_load)); \
    tme_memory_unaligned_unlock(tlb->tme_m68k_tlb_bus_rwlock); \
  } \
  else { \
    tme_memory_aligned_rdlock(tlb->tme_m68k_tlb_bus_rwlock); \
    (v) = (tme_uint32_t) tme_betoh_u32(*((tme_uint32_t *) emulator_load)); \
    tme_memory_aligned_unlock(tlb->tme_m68k_tlb_bus_rwlock); \
  } \
  insn_fetch_sizes = (insn_fetch_sizes << 1) | 1; \
  emulator_load += sizeof(tme_uint32_t)
#endif /* ALIGNOF_INT32_T <= ALIGNOF_INT16_T */

/* on a host with any alignment, 
   this loads a 32-bit signed value for the fast instruction executor: */
#if ALIGNOF_INT32_T <= ALIGNOF_INT16_T
#undef _TME_M68K_EXECUTE_FETCH_S32
#define _TME_M68K_EXECUTE_FETCH_S32(v) \
  if ((emulator_load + (sizeof(tme_uint32_t) - 1)) > emulator_load_last) \
    goto _tme_m68k_fast_fetch_failed; \
  if (((unsigned long) emulator_load) & (sizeof(tme_uint32_t) - 1)) { \
    tme_memory_unaligned_rdlock(tlb->tme_m68k_tlb_bus_rwlock); \
    (v) = (tme_int32_t) tme_betoh_u32(*((tme_uint32_t *) emulator_load)); \
    tme_memory_unaligned_unlock(tlb->tme_m68k_tlb_bus_rwlock); \
  } \
  else { \
    tme_memory_aligned_rdlock(tlb->tme_m68k_tlb_bus_rwlock); \
    (v) = (tme_int32_t) tme_betoh_u32(*((tme_uint32_t *) emulator_load)); \
    tme_memory_aligned_unlock(tlb->tme_m68k_tlb_bus_rwlock); \
  } \
  insn_fetch_sizes = (insn_fetch_sizes << 1) | 1; \
  emulator_load += sizeof(tme_int32_t)
#endif /* ALIGNOF_INT32_T <= ALIGNOF_INT16_T */

/* on a little-endian host with strict alignment, 
   this loads a 32-bit unsigned value for the fast instruction executor: */
#if (ALIGNOF_INT32_T > ALIGNOF_INT16_T) && !defined(WORDS_BIGENDIAN)
#undef _TME_M68K_EXECUTE_FETCH_U32
#define _TME_M68K_EXECUTE_FETCH_U32(v) \
  if ((emulator_load + (sizeof(tme_uint32_t) - 1)) > emulator_load_last) \
    goto _tme_m68k_fast_fetch_failed; \
  if (TME_SEQUENCE_ACCESS_NOT_COSTLIER || ((unsigned long) emulator_load) & (sizeof(tme_uint32_t) - 1)) { \
    tme_memory_sequence_rdlock(tlb->tme_m68k_bus_tlb_rwlock); \
    (v) = (tme_uint32_t) \
      tme_betoh_u32((((tme_uint32_t) ((tme_uint16_t *) emulator_load)[1]) << 16) | \
              ((tme_uint32_t) ((tme_uint16_t *) emulator_load)[0])); \
    tme_memory_sequence_unlock(tlb->tme_m68k_bus_tlb_rwlock); \
  } else { \
    tme_memory_aligned_rdlock(tlb->tme_m68k_tlb_bus_rwlock); \
    (v) = (tme_uint32_t) tme_betoh_u32(*((tme_uint32_t *) emulator_load)); \
    tme_memory_aligned_unlock(tlb->tme_m68k_tlb_bus_rwlock); \
  } \
  insn_fetch_sizes = (insn_fetch_sizes << 1) | 1; \
  emulator_load += sizeof(tme_uint32_t)
#endif /* (ALIGNOF_INT32_T > ALIGNOF_INT16_T) && !defined(WORDS_BIGENDIAN) */

/* on a little-endian host with strict alignment, 
   this loads a 32-bit signed value for the fast instruction executor: */
#if (ALIGNOF_INT32_T > ALIGNOF_INT16_T) && !defined(WORDS_BIGENDIAN)
#undef _TME_M68K_EXECUTE_FETCH_S32
#define _TME_M68K_EXECUTE_FETCH_S32(v) \
  if ((emulator_load + (sizeof(tme_uint32_t) - 1)) > emulator_load_last) \
    goto _tme_m68k_fast_fetch_failed; \
  if (TME_SEQUENCE_ACCESS_NOT_COSTLIER || ((unsigned long) emulator_load) & (sizeof(tme_uint32_t) - 1)) { \
    tme_memory_sequence_rdlock(tlb->tme_m68k_bus_tlb_rwlock); \
    (v) = (tme_int32_t) \
      tme_betoh_u32((((tme_uint32_t) ((tme_uint16_t *) emulator_load)[1]) << 16) | \
              ((tme_uint32_t) ((tme_uint16_t *) emulator_load)[0])); \
    tme_memory_sequence_unlock(tlb->tme_m68k_bus_tlb_rwlock); \
  } else { \
    tme_memory_aligned_rdlock(tlb->tme_m68k_tlb_bus_rwlock); \
    (v) = (tme_int32_t) tme_betoh_u32(*((tme_uint32_t *) emulator_load)); \
    tme_memory_aligned_unlock(tlb->tme_m68k_tlb_bus_rwlock); \
  } \
  insn_fetch_sizes = (insn_fetch_sizes << 1) | 1; \
  emulator_load += sizeof(tme_int32_t)
#endif /* (ALIGNOF_INT32_T > ALIGNOF_INT16_T) && !defined(WORDS_BIGENDIAN) */

/* on a big-endian host with strict alignment, 
   this loads a 32-bit unsigned value for the fast instruction executor: */
#if (ALIGNOF_INT32_T > ALIGNOF_INT16_T) && defined(WORDS_BIGENDIAN)
#undef _TME_M68K_EXECUTE_FETCH_U32
#define _TME_M68K_EXECUTE_FETCH_U32(v) \
  if ((emulator_load + (sizeof(tme_uint32_t) - 1)) > emulator_load_last) \
    goto _tme_m68k_fast_fetch_failed; \
  if (TME_SEQUENCE_ACCESS_NOT_COSTLIER || ((unsigned long) emulator_load) & (sizeof(tme_uint32_t) - 1)) { \
    tme_memory_sequence_rdlock(tlb->tme_m68k_bus_tlb_rwlock); \
    (v) = (tme_uint32_t) \
      ((((tme_uint32_t) ((tme_uint16_t *) emulator_load)[0]) << 16) | \
              ((tme_uint32_t) ((tme_uint16_t *) emulator_load)[1])); \
    tme_memory_sequence_unlock(tlb->tme_m68k_bus_tlb_rwlock); \
  } else { \
    tme_memory_aligned_rdlock(tlb->tme_m68k_tlb_bus_rwlock); \
    (v) = (tme_uint32_t) tme_betoh_u32(*((tme_uint32_t *) emulator_load)); \
    tme_memory_aligned_unlock(tlb->tme_m68k_tlb_bus_rwlock); \
  } \
  insn_fetch_sizes = (insn_fetch_sizes << 1) | 1; \
  emulator_load += sizeof(tme_uint32_t)
#endif /* (ALIGNOF_INT32_T > ALIGNOF_INT16_T) && defined(WORDS_BIGENDIAN) */

/* on a big-endian host with strict alignment, 
   this loads a 32-bit signed value for the fast instruction executor: */
#if (ALIGNOF_INT32_T > ALIGNOF_INT16_T) && defined(WORDS_BIGENDIAN)
#undef _TME_M68K_EXECUTE_FETCH_S32
#define _TME_M68K_EXECUTE_FETCH_S32(v) \
  if ((emulator_load + (sizeof(tme_uint32_t) - 1)) > emulator_load_last) \
    goto _tme_m68k_fast_fetch_failed; \
  if (TME_SEQUENCE_ACCESS_NOT_COSTLIER || ((unsigned long) emulator_load) & (sizeof(tme_uint32_t) - 1)) { \
    tme_memory_sequence_rdlock(tlb->tme_m68k_bus_tlb_rwlock); \
    (v) = (tme_int32_t) \
      ((((tme_uint32_t) ((tme_uint16_t *) emulator_load)[0]) << 16) | \
              ((tme_uint32_t) ((tme_uint16_t *) emulator_load)[1])); \
    tme_memory_sequence_unlock(tlb->tme_m68k_bus_tlb_rwlock); \
  } else { \
    tme_memory_aligned_rdlock(tlb->tme_m68k_tlb_bus_rwlock); \
    (v) = (tme_int32_t) tme_betoh_u32(*((tme_uint32_t *) emulator_load)); \
    tme_memory_aligned_unlock(tlb->tme_m68k_tlb_bus_rwlock); \
  } \
  insn_fetch_sizes = (insn_fetch_sizes << 1) | 1; \
  emulator_load += sizeof(tme_int32_t)
#endif /* (ALIGNOF_INT32_T > ALIGNOF_INT16_T) && defined(WORDS_BIGENDIAN) */

#endif /* _TME_M68K_EXECUTE_FAST */

#ifndef _TME_M68K_EXECUTE_FAST

/* these macros are for the slow executor: */

/* on all hosts, this fetches a 16-bit unsigned value for the slow executor: */
#undef _TME_M68K_EXECUTE_FETCH_U16
#define _TME_M68K_EXECUTE_FETCH_U16(v) \
  /* we update the instruction buffer fetch total and sizes values \
     before we do the actual fetch, because we may transfer a few \
     bytes and then fault.  without this, those few bytes wouldn't get \
     saved in the exception stack frame by tme_m68k_insn_buffer_xfer(), \
     because it wouldn't know about the fetch.  later, when the \
     instruction would be resumed, tme_m68k_fetch16() won't refetch \
     them, because it knows they've already been fetched and thinks \
     they're still in the instruction buffer: */ \
  ic->_tme_m68k_insn_buffer_fetch_total += sizeof(tme_uint16_t); \
  ic->_tme_m68k_insn_buffer_fetch_sizes <<= 1; \
  (v) = (tme_uint16_t) tme_m68k_fetch16(ic, linear_pc); \
  linear_pc += sizeof(tme_uint16_t)

/* on all hosts, this fetches a 16-bit signed value for the slow executor: */
#undef _TME_M68K_EXECUTE_FETCH_S16
#define _TME_M68K_EXECUTE_FETCH_S16(v) \
  /* we update the instruction buffer fetch total and sizes values \
     before we do the actual fetch, because we may transfer a few \
     bytes and then fault.  without this, those few bytes wouldn't get \
     saved in the exception stack frame by tme_m68k_insn_buffer_xfer(), \
     because it wouldn't know about the fetch.  later, when the \
     instruction would be resumed, tme_m68k_fetch16() won't refetch \
     them, because it knows they've already been fetched and thinks \
     they're still in the instruction buffer: */ \
  ic->_tme_m68k_insn_buffer_fetch_total += sizeof(tme_int16_t); \
  ic->_tme_m68k_insn_buffer_fetch_sizes <<= 1; \
  (v) = (tme_int16_t) tme_m68k_fetch16(ic, linear_pc); \
  linear_pc += sizeof(tme_int16_t)

/* on all hosts, this fetches a 32-bit unsigned value for the slow executor: */
#undef _TME_M68K_EXECUTE_FETCH_U32
#define _TME_M68K_EXECUTE_FETCH_U32(v) \
  /* we update the instruction buffer fetch total and sizes values \
     before we do the actual fetch, because we may transfer a few \
     bytes and then fault.  without this, those few bytes wouldn't get \
     saved in the exception stack frame by tme_m68k_insn_buffer_xfer(), \
     because it wouldn't know about the fetch.  later, when the \
     instruction would be resumed, tme_m68k_fetch32() won't refetch \
     them, because it knows they've already been fetched and thinks \
     they're still in the instruction buffer: */ \
  ic->_tme_m68k_insn_buffer_fetch_total += sizeof(tme_uint32_t); \
  ic->_tme_m68k_insn_buffer_fetch_sizes = (ic->_tme_m68k_insn_buffer_fetch_sizes << 1) | 1; \
  (v) = (tme_uint32_t) tme_m68k_fetch32(ic, linear_pc); \
  linear_pc += sizeof(tme_uint32_t)

/* on all hosts, this fetches a 32-bit signed value for the slow executor: */
#undef _TME_M68K_EXECUTE_FETCH_S32
#define _TME_M68K_EXECUTE_FETCH_S32(v) \
  /* we update the instruction buffer fetch total and sizes values \
     before we do the actual fetch, because we may transfer a few \
     bytes and then fault.  without this, those few bytes wouldn't get \
     saved in the exception stack frame by tme_m68k_insn_buffer_xfer(), \
     because it wouldn't know about the fetch.  later, when the \
     instruction would be resumed, tme_m68k_fetch32() won't refetch \
     them, because it knows they've already been fetched and thinks \
     they're still in the instruction buffer: */ \
  ic->_tme_m68k_insn_buffer_fetch_total += sizeof(tme_int32_t); \
  ic->_tme_m68k_insn_buffer_fetch_sizes = (ic->_tme_m68k_insn_buffer_fetch_sizes << 1) | 1; \
  (v) = (tme_int32_t) tme_m68k_fetch32(ic, linear_pc); \
  linear_pc += sizeof(tme_int32_t)

#endif /* !_TME_M68K_EXECUTE_FAST */
/* automatically generated by m68k-insns-auto.sh, do not edit! */
_TME_RCSID("$Id: m68k-insns-auto.sh,v 1.23 2005/03/10 13:26:23 fredette Exp $");

TME_M68K_INSN_DECL(tme_m68k_add8);
TME_M68K_INSN_DECL(tme_m68k_sub8);
TME_M68K_INSN_DECL(tme_m68k_cmp8);
TME_M68K_INSN_DECL(tme_m68k_neg8);
TME_M68K_INSN_DECL(tme_m68k_or8);
TME_M68K_INSN_DECL(tme_m68k_and8);
TME_M68K_INSN_DECL(tme_m68k_eor8);
TME_M68K_INSN_DECL(tme_m68k_not8);
TME_M68K_INSN_DECL(tme_m68k_tst8);
TME_M68K_INSN_DECL(tme_m68k_move8);
TME_M68K_INSN_DECL(tme_m68k_clr8);
TME_M68K_INSN_DECL(tme_m68k_negx8);
TME_M68K_INSN_DECL(tme_m68k_addx8);
TME_M68K_INSN_DECL(tme_m68k_subx8);
TME_M68K_INSN_DECL(tme_m68k_cmpm8);
TME_M68K_INSN_DECL(tme_m68k_btst8);
TME_M68K_INSN_DECL(tme_m68k_bchg8);
TME_M68K_INSN_DECL(tme_m68k_bclr8);
TME_M68K_INSN_DECL(tme_m68k_bset8);
TME_M68K_INSN_DECL(tme_m68k_asl8);
TME_M68K_INSN_DECL(tme_m68k_asr8);
TME_M68K_INSN_DECL(tme_m68k_lsl8);
TME_M68K_INSN_DECL(tme_m68k_lsr8);
TME_M68K_INSN_DECL(tme_m68k_rol8);
TME_M68K_INSN_DECL(tme_m68k_ror8);
TME_M68K_INSN_DECL(tme_m68k_roxl8);
TME_M68K_INSN_DECL(tme_m68k_roxr8);
TME_M68K_INSN_DECL(tme_m68k_cas8);
TME_M68K_INSN_DECL(tme_m68k_moves8);
TME_M68K_INSN_DECL(tme_m68k_add16);
TME_M68K_INSN_DECL(tme_m68k_sub16);
TME_M68K_INSN_DECL(tme_m68k_cmp16);
TME_M68K_INSN_DECL(tme_m68k_neg16);
TME_M68K_INSN_DECL(tme_m68k_or16);
TME_M68K_INSN_DECL(tme_m68k_and16);
TME_M68K_INSN_DECL(tme_m68k_eor16);
TME_M68K_INSN_DECL(tme_m68k_not16);
TME_M68K_INSN_DECL(tme_m68k_tst16);
TME_M68K_INSN_DECL(tme_m68k_move16);
TME_M68K_INSN_DECL(tme_m68k_clr16);
TME_M68K_INSN_DECL(tme_m68k_cmpa16);
TME_M68K_INSN_DECL(tme_m68k_negx16);
TME_M68K_INSN_DECL(tme_m68k_addx16);
TME_M68K_INSN_DECL(tme_m68k_subx16);
TME_M68K_INSN_DECL(tme_m68k_cmpm16);
TME_M68K_INSN_DECL(tme_m68k_suba16);
TME_M68K_INSN_DECL(tme_m68k_adda16);
TME_M68K_INSN_DECL(tme_m68k_movea16);
TME_M68K_INSN_DECL(tme_m68k_asl16);
TME_M68K_INSN_DECL(tme_m68k_asr16);
TME_M68K_INSN_DECL(tme_m68k_lsl16);
TME_M68K_INSN_DECL(tme_m68k_lsr16);
TME_M68K_INSN_DECL(tme_m68k_rol16);
TME_M68K_INSN_DECL(tme_m68k_ror16);
TME_M68K_INSN_DECL(tme_m68k_roxl16);
TME_M68K_INSN_DECL(tme_m68k_roxr16);
TME_M68K_INSN_DECL(tme_m68k_movep_rm16);
TME_M68K_INSN_DECL(tme_m68k_movem_rm16);
TME_M68K_INSN_DECL(tme_m68k_movep_mr16);
TME_M68K_INSN_DECL(tme_m68k_movem_mr16);
TME_M68K_INSN_DECL(tme_m68k_chk16);
TME_M68K_INSN_DECL(tme_m68k_cas16);
TME_M68K_INSN_DECL(tme_m68k_cas2_16);
TME_M68K_INSN_DECL(tme_m68k_moves16);
TME_M68K_INSN_DECL(tme_m68k_add32);
TME_M68K_INSN_DECL(tme_m68k_sub32);
TME_M68K_INSN_DECL(tme_m68k_cmp32);
TME_M68K_INSN_DECL(tme_m68k_neg32);
TME_M68K_INSN_DECL(tme_m68k_or32);
TME_M68K_INSN_DECL(tme_m68k_and32);
TME_M68K_INSN_DECL(tme_m68k_eor32);
TME_M68K_INSN_DECL(tme_m68k_not32);
TME_M68K_INSN_DECL(tme_m68k_tst32);
TME_M68K_INSN_DECL(tme_m68k_move32);
TME_M68K_INSN_DECL(tme_m68k_moveq32);
TME_M68K_INSN_DECL(tme_m68k_clr32);
TME_M68K_INSN_DECL(tme_m68k_negx32);
TME_M68K_INSN_DECL(tme_m68k_addx32);
TME_M68K_INSN_DECL(tme_m68k_subx32);
TME_M68K_INSN_DECL(tme_m68k_cmpm32);
TME_M68K_INSN_DECL(tme_m68k_suba32);
TME_M68K_INSN_DECL(tme_m68k_adda32);
TME_M68K_INSN_DECL(tme_m68k_movea32);
TME_M68K_INSN_DECL(tme_m68k_btst32);
TME_M68K_INSN_DECL(tme_m68k_bchg32);
TME_M68K_INSN_DECL(tme_m68k_bclr32);
TME_M68K_INSN_DECL(tme_m68k_bset32);
TME_M68K_INSN_DECL(tme_m68k_asl32);
TME_M68K_INSN_DECL(tme_m68k_asr32);
TME_M68K_INSN_DECL(tme_m68k_lsl32);
TME_M68K_INSN_DECL(tme_m68k_lsr32);
TME_M68K_INSN_DECL(tme_m68k_rol32);
TME_M68K_INSN_DECL(tme_m68k_ror32);
TME_M68K_INSN_DECL(tme_m68k_roxl32);
TME_M68K_INSN_DECL(tme_m68k_roxr32);
TME_M68K_INSN_DECL(tme_m68k_movep_rm32);
TME_M68K_INSN_DECL(tme_m68k_movem_rm32);
TME_M68K_INSN_DECL(tme_m68k_movep_mr32);
TME_M68K_INSN_DECL(tme_m68k_movem_mr32);
TME_M68K_INSN_DECL(tme_m68k_chk32);
TME_M68K_INSN_DECL(tme_m68k_cas32);
TME_M68K_INSN_DECL(tme_m68k_cas2_32);
TME_M68K_INSN_DECL(tme_m68k_moves32);
void tme_m68k_read_memx8 _TME_P((struct tme_m68k *));
void tme_m68k_read_mem8 _TME_P((struct tme_m68k *, int));
#define tme_m68k_read8(ic, t, fc, la, _v, f) \
  tme_m68k_read(ic, t, fc, la, (tme_uint8_t *) (_v), sizeof(tme_uint8_t), f)
void tme_m68k_write_memx8 _TME_P((struct tme_m68k *));
void tme_m68k_write_mem8 _TME_P((struct tme_m68k *, int));
#define tme_m68k_write8(ic, t, fc, la, _v, f) \
  tme_m68k_write(ic, t, fc, la, (tme_uint8_t *) (_v), sizeof(tme_uint8_t), f)
void tme_m68k_read_memx16 _TME_P((struct tme_m68k *));
void tme_m68k_read_mem16 _TME_P((struct tme_m68k *, int));
tme_uint16_t tme_m68k_fetch16 _TME_P((struct tme_m68k *, tme_uint32_t));
void tme_m68k_pop16 _TME_P((struct tme_m68k *, tme_uint16_t *));
#define tme_m68k_read16(ic, t, fc, la, _v, f) \
  tme_m68k_read(ic, t, fc, la, (tme_uint8_t *) (_v), sizeof(tme_uint16_t), f)
void tme_m68k_write_memx16 _TME_P((struct tme_m68k *));
void tme_m68k_write_mem16 _TME_P((struct tme_m68k *, int));
void tme_m68k_push16 _TME_P((struct tme_m68k *, tme_uint16_t ));
#define tme_m68k_write16(ic, t, fc, la, _v, f) \
  tme_m68k_write(ic, t, fc, la, (tme_uint8_t *) (_v), sizeof(tme_uint16_t), f)
void tme_m68k_read_memx32 _TME_P((struct tme_m68k *));
void tme_m68k_read_mem32 _TME_P((struct tme_m68k *, int));
tme_uint32_t tme_m68k_fetch32 _TME_P((struct tme_m68k *, tme_uint32_t));
void tme_m68k_pop32 _TME_P((struct tme_m68k *, tme_uint32_t *));
#define tme_m68k_read32(ic, t, fc, la, _v, f) \
  tme_m68k_read(ic, t, fc, la, (tme_uint8_t *) (_v), sizeof(tme_uint32_t), f)
void tme_m68k_write_memx32 _TME_P((struct tme_m68k *));
void tme_m68k_write_mem32 _TME_P((struct tme_m68k *, int));
void tme_m68k_push32 _TME_P((struct tme_m68k *, tme_uint32_t ));
#define tme_m68k_write32(ic, t, fc, la, _v, f) \
  tme_m68k_write(ic, t, fc, la, (tme_uint8_t *) (_v), sizeof(tme_uint32_t), f)
void tme_m68k_read_mem _TME_P((struct tme_m68k *, tme_uint8_t *, unsigned int));
void tme_m68k_read _TME_P((struct tme_m68k *, struct tme_m68k_tlb *, unsigned int *, tme_uint32_t *, tme_uint8_t *, unsigned int, unsigned int));
void tme_m68k_write_mem _TME_P((struct tme_m68k *, tme_uint8_t *, unsigned int));
void tme_m68k_write _TME_P((struct tme_m68k *, struct tme_m68k_tlb *, unsigned int *, tme_uint32_t *, tme_uint8_t *, unsigned int, unsigned int));
TME_M68K_INSN_DECL(tme_m68k_abcd);
TME_M68K_INSN_DECL(tme_m68k_sbcd);
TME_M68K_INSN_DECL(tme_m68k_nbcd);
TME_M68K_INSN_DECL(tme_m68k_ori_ccr);
TME_M68K_INSN_DECL(tme_m68k_andi_ccr);
TME_M68K_INSN_DECL(tme_m68k_eori_ccr);
TME_M68K_INSN_DECL(tme_m68k_move_to_ccr);
TME_M68K_INSN_DECL(tme_m68k_ori_sr);
TME_M68K_INSN_DECL(tme_m68k_andi_sr);
TME_M68K_INSN_DECL(tme_m68k_eori_sr);
TME_M68K_INSN_DECL(tme_m68k_move_to_sr);
TME_M68K_INSN_DECL(tme_m68k_mulu);
TME_M68K_INSN_DECL(tme_m68k_divu);
TME_M68K_INSN_DECL(tme_m68k_mulul);
TME_M68K_INSN_DECL(tme_m68k_divul);
TME_M68K_INSN_DECL(tme_m68k_muls);
TME_M68K_INSN_DECL(tme_m68k_divs);
TME_M68K_INSN_DECL(tme_m68k_mulsl);
TME_M68K_INSN_DECL(tme_m68k_divsl);
