/* $Id: ieee754-misc.c,v 1.2 2005/05/14 22:18:37 fredette Exp $ */

/* ic/ieee754/ieee754-misc.c - IEEE 754 miscellaneous: */

/*
 * Copyright (c) 2004 Matt Fredette
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

#include <tme/common.h>
_TME_RCSID("$Id: ieee754-misc.c,v 1.2 2005/05/14 22:18:37 fredette Exp $");

/* includes: */
#include <tme/ic/ieee754.h>

/* constants: */

const tme_uint32_t tme_ieee754_single_constant_pi = 0x40490fdb;
const tme_uint32_t tme_ieee754_single_constant_log10_2 = 0x3e9a209b;
const tme_uint32_t tme_ieee754_single_constant_e = 0x402df854;
const tme_uint32_t tme_ieee754_single_constant_log2_e = 0x3fb8aa3b;
const tme_uint32_t tme_ieee754_single_constant_log10_e = 0x3ede5bd9;
const tme_uint32_t tme_ieee754_single_constant_zero = 0x00000000;
const tme_uint32_t tme_ieee754_single_constant_ln_2 = 0x3f317218;
const tme_uint32_t tme_ieee754_single_constant_ln_10 = 0x40135d8e;
const tme_uint32_t tme_ieee754_single_constant_one = 0x3f800000;
const struct tme_ieee754_double_constant tme_ieee754_double_constant_pi = { 0x400921fb, 0x5421d100 };
const struct tme_ieee754_double_constant tme_ieee754_double_constant_log10_2 = { 0x3fd34413, 0x509f79ff };
const struct tme_ieee754_double_constant tme_ieee754_double_constant_e = { 0x4005bf0a, 0x8b145769 };
const struct tme_ieee754_double_constant tme_ieee754_double_constant_log2_e = { 0x3ff71547, 0x652b82fe };
const struct tme_ieee754_double_constant tme_ieee754_double_constant_log10_e = { 0x3fdbcb7b, 0x1526e50e };
const struct tme_ieee754_double_constant tme_ieee754_double_constant_zero = { 0x00000000, 0x00000000 };
const struct tme_ieee754_double_constant tme_ieee754_double_constant_ln_2 = { 0x3fe62e42, 0xfefa39ef };
const struct tme_ieee754_double_constant tme_ieee754_double_constant_ln_10 = { 0x40026bb1, 0xbbb55516 };
const struct tme_ieee754_double_constant tme_ieee754_double_constant_one = { 0x3ff00000, 0x00000000 };
const struct tme_ieee754_extended80_constant tme_ieee754_extended80_constant_pi = { 0x4000, 0xc90fdaa2, 0x2168c000 };
const struct tme_ieee754_extended80_constant tme_ieee754_extended80_constant_log10_2 = { 0x3ffd, 0x9a209a84, 0xfbcff800 };
const struct tme_ieee754_extended80_constant tme_ieee754_extended80_constant_e = { 0x4000, 0xadf85458, 0xa2bb4800 };
const struct tme_ieee754_extended80_constant tme_ieee754_extended80_constant_log2_e = { 0x3fff, 0xb8aa3b29, 0x5c17f000 };
const struct tme_ieee754_extended80_constant tme_ieee754_extended80_constant_log10_e = { 0x3ffd, 0xde5bd8a9, 0x37287000 };
const struct tme_ieee754_extended80_constant tme_ieee754_extended80_constant_zero = { 0x0000, 0x00000000, 0x00000000 };
const struct tme_ieee754_extended80_constant tme_ieee754_extended80_constant_ln_2 = { 0x3ffe, 0xb17217f7, 0xd1cf7800 };
const struct tme_ieee754_extended80_constant tme_ieee754_extended80_constant_ln_10 = { 0x4000, 0x935d8ddd, 0xaaa8b000 };
const struct tme_ieee754_extended80_constant tme_ieee754_extended80_constant_one = { 0x3fff, 0x80000000, 0x00000000 };

/* IEEE 754 single precision values of the form 2^x, where x is a power of two: */
const tme_uint32_t tme_ieee754_single_constant_2e2ex[] = {

  /* 2^1: */
  0x40000000,

  /* 2^2: */
  0x40800000,

  /* 2^4: */
  0x41800000,

  /* 2^8: */
  0x43800000,

  /* 2^16: */
  0x47800000,

  /* 2^32: */
  0x4f800000,

  /* 2^64: */
  0x5f800000
};

/* IEEE 754 single precision values of the form 2^-x, where x is a power of two: */
const tme_uint32_t tme_ieee754_single_constant_2e_minus_2ex[] = {

  /* 2^-1: */
  0x3f000000,

  /* 2^-2: */
  0x3e800000,

  /* 2^-4: */
  0x3d800000,

  /* 2^-8: */
  0x3b800000,

  /* 2^-16: */
  0x37800000,

  /* 2^-32: */
  0x2f800000,

  /* 2^-64: */
  0x1f800000
};

/* IEEE 754 single precision values of the form 10^x, where x is a power of two: */
const tme_uint32_t tme_ieee754_single_constant_10e2ex[] = {

  /* 10^1: */
  0x41200000,

  /* 10^2: */
  0x42c80000,

  /* 10^4: */
  0x461c4000,

  /* 10^8: */
  0x4cbebc20,

  /* 10^16: */
  0x5a0e1bca,

  /* 10^32: */
  0x749dc5ae
};

/* IEEE 754 single precision values of the form 10^-x, where x is a power of two: */
const tme_uint32_t tme_ieee754_single_constant_10e_minus_2ex[] = {

  /* 10^-1: */
  0x3dcccccd,

  /* 10^-2: */
  0x3c23d70b,

  /* 10^-4: */
  0x38d1b719,

  /* 10^-8: */
  0x322bcc7a,

  /* 10^-16: */
  0x24e6959d,

  /* 10^-32: */
  0x0a4fb12e
};

/* IEEE 754 double precision values of the form 2^x, where x is a power of two: */
const struct tme_ieee754_double_constant tme_ieee754_double_constant_2e2ex[] = {

  /* 2^1: */
  { 0x40000000, 0x00000000 },

  /* 2^2: */
  { 0x40100000, 0x00000000 },

  /* 2^4: */
  { 0x40300000, 0x00000000 },

  /* 2^8: */
  { 0x40700000, 0x00000000 },

  /* 2^16: */
  { 0x40f00000, 0x00000000 },

  /* 2^32: */
  { 0x41f00000, 0x00000000 },

  /* 2^64: */
  { 0x43f00000, 0x00000000 },

  /* 2^128: */
  { 0x47f00000, 0x00000000 },

  /* 2^256: */
  { 0x4ff00000, 0x00000000 },

  /* 2^512: */
  { 0x5ff00000, 0x00000000 }
};

/* IEEE 754 double precision values of the form 2^-x, where x is a power of two: */
const struct tme_ieee754_double_constant tme_ieee754_double_constant_2e_minus_2ex[] = {

  /* 2^-1: */
  { 0x3fe00000, 0x00000000 },

  /* 2^-2: */
  { 0x3fd00000, 0x00000000 },

  /* 2^-4: */
  { 0x3fb00000, 0x00000000 },

  /* 2^-8: */
  { 0x3f700000, 0x00000000 },

  /* 2^-16: */
  { 0x3ef00000, 0x00000000 },

  /* 2^-32: */
  { 0x3df00000, 0x00000000 },

  /* 2^-64: */
  { 0x3bf00000, 0x00000000 },

  /* 2^-128: */
  { 0x37f00000, 0x00000000 },

  /* 2^-256: */
  { 0x2ff00000, 0x00000000 },

  /* 2^-512: */
  { 0x1ff00000, 0x00000000 }
};

/* IEEE 754 double precision values of the form 10^x, where x is a power of two: */
const struct tme_ieee754_double_constant tme_ieee754_double_constant_10e2ex[] = {

  /* 10^1: */
  { 0x40240000, 0x00000000 },

  /* 10^2: */
  { 0x40590000, 0x00000000 },

  /* 10^4: */
  { 0x40c38800, 0x00000000 },

  /* 10^8: */
  { 0x4197d784, 0x00000000 },

  /* 10^16: */
  { 0x4341c379, 0x37e08000 },

  /* 10^32: */
  { 0x4693b8b5, 0xb5056e17 },

  /* 10^64: */
  { 0x4d384f03, 0xe93ff9f6 },

  /* 10^128: */
  { 0x5a827748, 0xf9301d33 },

  /* 10^256: */
  { 0x75154fdd, 0x7f73bf3f }
};

/* IEEE 754 double precision values of the form 10^-x, where x is a power of two: */
const struct tme_ieee754_double_constant tme_ieee754_double_constant_10e_minus_2ex[] = {

  /* 10^-1: */
  { 0x3fb99999, 0x9999999a },

  /* 10^-2: */
  { 0x3f847ae1, 0x47ae147c },

  /* 10^-4: */
  { 0x3f1a36e2, 0xeb1c4330 },

  /* 10^-8: */
  { 0x3e45798e, 0xe2308c3f },

  /* 10^-16: */
  { 0x3c9cd2b2, 0x97d889ca },

  /* 10^-32: */
  { 0x3949f623, 0xd5a8a74c },

  /* 10^-64: */
  { 0x32a50ffd, 0x44f4a766 },

  /* 10^-128: */
  { 0x255bba08, 0xcf8c9808 },

  /* 10^-256: */
  { 0x0ac80628, 0x64ac6ffd }
};

/* IEEE 754 extended80 precision values of the form 2^x, where x is a power of two: */
const struct tme_ieee754_extended80_constant tme_ieee754_extended80_constant_2e2ex[] = {

  /* 2^1: */
  { 0x4000, 0x80000000, 0x00000000 },

  /* 2^2: */
  { 0x4001, 0x80000000, 0x00000000 },

  /* 2^4: */
  { 0x4003, 0x80000000, 0x00000000 },

  /* 2^8: */
  { 0x4007, 0x80000000, 0x00000000 },

  /* 2^16: */
  { 0x400f, 0x80000000, 0x00000000 },

  /* 2^32: */
  { 0x401f, 0x80000000, 0x00000000 },

  /* 2^64: */
  { 0x403f, 0x80000000, 0x00000000 },

  /* 2^128: */
  { 0x407f, 0x80000000, 0x00000000 },

  /* 2^256: */
  { 0x40ff, 0x80000000, 0x00000000 },

  /* 2^512: */
  { 0x41ff, 0x80000000, 0x00000000 },

  /* 2^1024: */
  { 0x43ff, 0x80000000, 0x00000000 },

  /* 2^2048: */
  { 0x47ff, 0x80000000, 0x00000000 },

  /* 2^4096: */
  { 0x4fff, 0x80000000, 0x00000000 },

  /* 2^8192: */
  { 0x5fff, 0x80000000, 0x00000000 }
};

/* IEEE 754 extended80 precision values of the form 2^-x, where x is a power of two: */
const struct tme_ieee754_extended80_constant tme_ieee754_extended80_constant_2e_minus_2ex[] = {

  /* 2^-1: */
  { 0x3ffe, 0x80000000, 0x00000000 },

  /* 2^-2: */
  { 0x3ffd, 0x80000000, 0x00000000 },

  /* 2^-4: */
  { 0x3ffb, 0x80000000, 0x00000000 },

  /* 2^-8: */
  { 0x3ff7, 0x80000000, 0x00000000 },

  /* 2^-16: */
  { 0x3fef, 0x80000000, 0x00000000 },

  /* 2^-32: */
  { 0x3fdf, 0x80000000, 0x00000000 },

  /* 2^-64: */
  { 0x3fbf, 0x80000000, 0x00000000 },

  /* 2^-128: */
  { 0x3f7f, 0x80000000, 0x00000000 },

  /* 2^-256: */
  { 0x3eff, 0x80000000, 0x00000000 },

  /* 2^-512: */
  { 0x3dff, 0x80000000, 0x00000000 },

  /* 2^-1024: */
  { 0x3bff, 0x80000000, 0x00000000 },

  /* 2^-2048: */
  { 0x37ff, 0x80000000, 0x00000000 },

  /* 2^-4096: */
  { 0x2fff, 0x80000000, 0x00000000 },

  /* 2^-8192: */
  { 0x1fff, 0x80000000, 0x00000000 }
};

/* IEEE 754 extended80 precision values of the form 10^x, where x is a power of two: */
const struct tme_ieee754_extended80_constant tme_ieee754_extended80_constant_10e2ex[] = {

  /* 10^1: */
  { 0x4002, 0xa0000000, 0x00000000 },

  /* 10^2: */
  { 0x4005, 0xc8000000, 0x00000000 },

  /* 10^4: */
  { 0x400c, 0x9c400000, 0x00000000 },

  /* 10^8: */
  { 0x4019, 0xbebc2000, 0x00000000 },

  /* 10^16: */
  { 0x4034, 0x8e1bc9bf, 0x04000000 },

  /* 10^32: */
  { 0x4069, 0x9dc5ada8, 0x2b70b800 },

  /* 10^64: */
  { 0x40d3, 0xc2781f49, 0xffcfb000 },

  /* 10^128: */
  { 0x41a8, 0x93ba47c9, 0x80e99800 },

  /* 10^256: */
  { 0x4351, 0xaa7eebfb, 0x9df9f800 },

  /* 10^512: */
  { 0x46a3, 0xe319a0ae, 0xa60ed800 },

  /* 10^1024: */
  { 0x4d48, 0xc9767586, 0x81758800 },

  /* 10^2048: */
  { 0x5a92, 0x9e8b3b5d, 0xc53e2000 },

  /* 10^4096: */
  { 0x7525, 0xc4605202, 0x8a227800 }
};

/* IEEE 754 extended80 precision values of the form 10^-x, where x is a power of two: */
const struct tme_ieee754_extended80_constant tme_ieee754_extended80_constant_10e_minus_2ex[] = {

  /* 10^-1: */
  { 0x3ffb, 0xcccccccc, 0xccccd000 },

  /* 10^-2: */
  { 0x3ff8, 0xa3d70a3d, 0x70a3e000 },

  /* 10^-4: */
  { 0x3ff1, 0xd1b71758, 0xe2198000 },

  /* 10^-8: */
  { 0x3fe4, 0xabcc7711, 0x8461f800 },

  /* 10^-16: */
  { 0x3fc9, 0xe69594be, 0xc44e5000 },

  /* 10^-32: */
  { 0x3f94, 0xcfb11ead, 0x453a6000 },

  /* 10^-64: */
  { 0x3f2a, 0xa87fea27, 0xa53b3000 },

  /* 10^-128: */
  { 0x3e55, 0xddd0467c, 0x64c04000 },

  /* 10^-256: */
  { 0x3cac, 0xc0314325, 0x637fe800 },

  /* 10^-512: */
  { 0x395a, 0x9049ee32, 0xdb2c8800 },

  /* 10^-1024: */
  { 0x32b5, 0xa2a682a5, 0xda6b6800 },

  /* 10^-2048: */
  { 0x256b, 0xceae534f, 0x34682000 },

  /* 10^-4096: */
  { 0x0ad8, 0xa6dd04c8, 0xd31f4800 }
};

/* the native floating-point exception function: */
void
tme_ieee754_exception_float(int exceptions, void *_ieee754_ctl)
{
  struct tme_ieee754_ctl *ieee754_ctl;

  /* recover our global control: */
  ieee754_ctl = (struct tme_ieee754_ctl *) _ieee754_ctl;

  /* signal the exception: */
  (*ieee754_ctl->tme_ieee754_ctl_exception)(ieee754_ctl, exceptions);
}

/* the softfloat unlock function: */
int
tme_ieee754_unlock_softfloat(void)
{
  int exceptions;

  tme_ieee754_global_ctl = NULL;
  exceptions = tme_ieee754_global_exceptions;
  tme_mutex_unlock(&tme_ieee754_global_mutex);
  return (exceptions);
}

#include "ieee754-misc-auto.c"
