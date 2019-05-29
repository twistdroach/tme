
/*  A Bison parser, made from ../../tmesh/tmesh-input.y
    by GNU Bison version 1.28  */

#define YYBISON 1  /* Identify Bison output.  */

#define	TMESH_TOKEN_SOURCE	257
#define	TMESH_TOKEN_MKDIR	258
#define	TMESH_TOKEN_RMDIR	259
#define	TMESH_TOKEN_CD	260
#define	TMESH_TOKEN_PWD	261
#define	TMESH_TOKEN_LS	262
#define	TMESH_TOKEN_CONNECT	263
#define	TMESH_TOKEN_RM	264
#define	TMESH_TOKEN_MV	265
#define	TMESH_TOKEN_COMMAND	266
#define	TMESH_TOKEN_LOG	267
#define	TMESH_TOKEN_ALIAS	268
#define	TMESH_TOKEN_AT	269
#define	TMESH_TOKEN_PATHNAME	270
#define	TMESH_TOKEN_ARG	271
#define	TMESH_TOKEN_OPTS	272

#line 1 "../../tmesh/tmesh-input.y"

/* $Id: tmesh-input.y,v 1.4 2006/11/15 23:11:31 fredette Exp $ */

/* tmesh/tmesh-input.y - the tme shell scanner and parser: */

/*
 * Copyright (c) 2003 Matt Fredette
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
_TME_RCSID("$Id: tmesh-input.y,v 1.4 2006/11/15 23:11:31 fredette Exp $");

/* includes: */
#include <tme/threads.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "tmesh-impl.h"

/* macros: */

/* internal token numbers: */
#define TMESH_TOKEN_UNDEF		(-1)
#define TMESH_TOKEN_EOF			(0)

/* internal character numbers: */
#define TMESH_C_EOF_SEMICOLON		(TMESH_C_YIELD - 1)
#define TMESH_C_UNDEF			(TMESH_C_EOF_SEMICOLON - 2)

#define YYSTYPE struct tmesh_parser_value
#define YYDEBUG 1
#define YYMAXDEPTH 10000

/* types: */

/* globals: */
static tme_mutex_t _tmesh_input_mutex;
static struct tmesh *_tmesh_input;
static char **_tmesh_output;
static int _tmesh_input_yielding;
static YYSTYPE *_tmesh_input_parsed;

/* prototypes: */
static int yylex _TME_P((void));
static void yyerror _TME_P((char *));
static void _tmesh_scanner_in_args _TME_P((void));
static void _tmesh_parser_argv_arg _TME_P((struct tmesh_parser_argv *, char *, int));

#ifndef YYSTYPE
#define YYSTYPE int
#endif
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		74
#define	YYFLAG		-32768
#define	YYNTBASE	21

#define YYTRANSLATE(x) ((unsigned)(x) <= 272 ? yytranslate[x] : 42)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,    20,    19,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     3,     4,     5,     6,
     7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
    17,    18
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     4,     6,     8,    10,    12,    14,    16,    18,
    20,    22,    24,    27,    29,    33,    37,    41,    45,    48,
    53,    57,    60,    64,    69,    73,    77,    82,    84,    86,
    87,    89,    92,    94,    97,    99,   101,   105,   111,   115,
   117
};

static const short yyrhs[] = {    22,
     0,    23,     0,    24,     0,    25,     0,    26,     0,    27,
     0,    28,     0,    29,     0,    30,     0,    31,     0,    32,
     0,    33,     0,     1,    19,     0,    19,     0,     3,    34,
    19,     0,     4,    34,    19,     0,     5,    34,    19,     0,
     6,    34,    19,     0,     7,    19,     0,     8,    41,    35,
    19,     0,     9,    40,    19,     0,    40,    19,     0,    10,
    34,    19,     0,    11,    34,    34,    19,     0,    12,    36,
    19,     0,    13,    36,    19,     0,    14,    34,    34,    19,
     0,    16,     0,    16,     0,     0,    16,     0,    36,    17,
     0,    17,     0,    37,    17,     0,    20,     0,    15,     0,
    36,    38,    37,     0,    36,    39,    37,    38,    37,     0,
    36,    39,    37,     0,    18,     0,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
    99,   100,   101,   102,   103,   104,   105,   106,   107,   108,
   109,   110,   111,   112,   117,   121,   126,   131,   136,   139,
   148,   150,   155,   160,   169,   174,   179,   188,   192,   193,
   198,   205,   215,   221,   230,   233,   236,   246,   252,   261,
   262
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","TMESH_TOKEN_SOURCE",
"TMESH_TOKEN_MKDIR","TMESH_TOKEN_RMDIR","TMESH_TOKEN_CD","TMESH_TOKEN_PWD","TMESH_TOKEN_LS",
"TMESH_TOKEN_CONNECT","TMESH_TOKEN_RM","TMESH_TOKEN_MV","TMESH_TOKEN_COMMAND",
"TMESH_TOKEN_LOG","TMESH_TOKEN_ALIAS","TMESH_TOKEN_AT","TMESH_TOKEN_PATHNAME",
"TMESH_TOKEN_ARG","TMESH_TOKEN_OPTS","';'","':'","command","command_source",
"command_mkdir","command_rmdir","command_cd","command_pwd","command_ls","command_connect",
"command_rm","command_mv","command_command","command_log","command_alias","pathname",
"pathname_opt","pathname_args","args","colon","at","connection","opts_opt", NULL
};
#endif

static const short yyr1[] = {     0,
    21,    21,    21,    21,    21,    21,    21,    21,    21,    21,
    21,    21,    21,    21,    22,    23,    24,    25,    26,    27,
    28,    28,    29,    30,    31,    32,    33,    34,    35,    35,
    36,    36,    37,    37,    38,    39,    40,    40,    40,    41,
    41
};

static const short yyr2[] = {     0,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     2,     1,     3,     3,     3,     3,     2,     4,
     3,     2,     3,     4,     3,     3,     4,     1,     1,     0,
     1,     2,     1,     2,     1,     1,     3,     5,     3,     1,
     0
};

static const short yydefact[] = {     0,
     0,     0,     0,     0,     0,     0,    41,     0,     0,     0,
     0,     0,     0,    31,    14,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,     0,     0,    13,
    28,     0,     0,     0,     0,    19,    40,    30,     0,     0,
     0,     0,     0,     0,    36,    32,    35,     0,     0,    22,
    15,    16,    17,    18,    29,     0,    21,    23,     0,    25,
    26,     0,    33,    37,    39,    20,    24,    27,    34,     0,
    38,     0,     0,     0
};

static const short yydefgoto[] = {    72,
    16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
    26,    27,    32,    56,    28,    64,    48,    49,    29,    38
};

static const short yypact[] = {    39,
   -15,     1,     1,     1,     1,    -7,     3,     6,     1,     1,
     6,     6,     1,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,    -6,     4,-32768,
-32768,     7,     9,    10,    11,-32768,-32768,    15,    13,    14,
     1,    -4,    -1,     1,-32768,-32768,-32768,    17,    17,-32768,
-32768,-32768,-32768,-32768,-32768,    16,-32768,-32768,    18,-32768,
-32768,    20,-32768,    19,   -12,-32768,-32768,-32768,-32768,    17,
    19,    25,    27,-32768
};

static const short yypgoto[] = {-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,    -3,-32768,     8,   -46,   -11,-32768,    48,-32768
};


#define	YYLAST		58


static const short yytable[] = {    33,
    34,    35,    65,    30,    69,    40,    41,    47,    45,    44,
    46,    36,    46,    47,    60,    46,    31,    61,    42,    43,
    37,    14,    50,    71,    73,    51,    74,    52,    53,    54,
    55,    57,    58,    63,    66,    69,    67,    59,    68,     1,
    62,     2,     3,     4,     5,     6,     7,     8,     9,    10,
    11,    12,    13,    70,    14,    39,     0,    15
};

static const short yycheck[] = {     3,
     4,     5,    49,    19,    17,     9,    10,    20,    15,    13,
    17,    19,    17,    20,    19,    17,    16,    19,    11,    12,
    18,    16,    19,    70,     0,    19,     0,    19,    19,    19,
    16,    19,    19,    17,    19,    17,    19,    41,    19,     1,
    44,     3,     4,     5,     6,     7,     8,     9,    10,    11,
    12,    13,    14,    65,    16,     8,    -1,    19
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/pkg/share/bison.simple"
/* This file comes from bison-1.28.  */

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

#ifndef YYSTACK_USE_ALLOCA
#ifdef alloca
#define YYSTACK_USE_ALLOCA
#else /* alloca not defined */
#ifdef __GNUC__
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi) || (defined (__sun) && defined (__i386))
#define YYSTACK_USE_ALLOCA
#include <alloca.h>
#else /* not sparc */
/* We think this test detects Watcom and Microsoft C.  */
/* This used to test MSDOS, but that is a bad idea
   since that symbol is in the user namespace.  */
#if (defined (_MSDOS) || defined (_MSDOS_)) && !defined (__TURBOC__)
#if 0 /* No need for malloc.h, which pollutes the namespace;
	 instead, just don't use alloca.  */
#include <malloc.h>
#endif
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
/* I don't know what this was needed for, but it pollutes the namespace.
   So I turned it off.   rms, 2 May 1997.  */
/* #include <malloc.h>  */
 #pragma alloca
#define YYSTACK_USE_ALLOCA
#else /* not MSDOS, or __TURBOC__, or _AIX */
#if 0
#ifdef __hpux /* haible@ilog.fr says this works for HPUX 9.05 and up,
		 and on HPUX 10.  Eventually we can turn this on.  */
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#endif /* __hpux */
#endif
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc */
#endif /* not GNU C */
#endif /* alloca not defined */
#endif /* YYSTACK_USE_ALLOCA not defined */

#ifdef YYSTACK_USE_ALLOCA
#define YYSTACK_ALLOC alloca
#else
#define YYSTACK_ALLOC malloc
#endif

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	goto yyacceptlab
#define YYABORT 	goto yyabortlab
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, &yylloc, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval, &yylloc)
#endif
#else /* not YYLSP_NEEDED */
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif /* not YYLSP_NEEDED */
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Define __yy_memcpy.  Note that the size argument
   should be passed with type unsigned int, because that is what the non-GCC
   definitions require.  With GCC, __builtin_memcpy takes an arg
   of type size_t, but it can handle unsigned int.  */

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_memcpy(TO,FROM,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (to, from, count)
     char *to;
     char *from;
     unsigned int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (char *to, char *from, unsigned int count)
{
  register char *t = to;
  register char *f = from;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 217 "/usr/pkg/share/bison.simple"

/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
#ifdef __cplusplus
#define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else /* not __cplusplus */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
#endif /* not __cplusplus */
#else /* not YYPARSE_PARAM */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif /* not YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
#ifdef YYPARSE_PARAM
int yyparse (void *);
#else
int yyparse (void);
#endif
#endif

int
yyparse(YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;
  int yyfree_stacks = 0;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  if (yyfree_stacks)
	    {
	      free (yyss);
	      free (yyvs);
#ifdef YYLSP_NEEDED
	      free (yyls);
#endif
	    }
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
#ifndef YYSTACK_USE_ALLOCA
      yyfree_stacks = 1;
#endif
      yyss = (short *) YYSTACK_ALLOC (yystacksize * sizeof (*yyssp));
      __yy_memcpy ((char *)yyss, (char *)yyss1,
		   size * (unsigned int) sizeof (*yyssp));
      yyvs = (YYSTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yyvsp));
      __yy_memcpy ((char *)yyvs, (char *)yyvs1,
		   size * (unsigned int) sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yylsp));
      __yy_memcpy ((char *)yyls, (char *)yyls1,
		   size * (unsigned int) sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 1:
#line 99 "../../tmesh/tmesh-input.y"
{ *_tmesh_input_parsed = yyvsp[0]; YYACCEPT; ;
    break;}
case 2:
#line 100 "../../tmesh/tmesh-input.y"
{ *_tmesh_input_parsed = yyvsp[0]; YYACCEPT; ;
    break;}
case 3:
#line 101 "../../tmesh/tmesh-input.y"
{ *_tmesh_input_parsed = yyvsp[0]; YYACCEPT; ;
    break;}
case 4:
#line 102 "../../tmesh/tmesh-input.y"
{ *_tmesh_input_parsed = yyvsp[0]; YYACCEPT; ;
    break;}
case 5:
#line 103 "../../tmesh/tmesh-input.y"
{ *_tmesh_input_parsed = yyvsp[0]; YYACCEPT; ;
    break;}
case 6:
#line 104 "../../tmesh/tmesh-input.y"
{ *_tmesh_input_parsed = yyvsp[0]; YYACCEPT; ;
    break;}
case 7:
#line 105 "../../tmesh/tmesh-input.y"
{ *_tmesh_input_parsed = yyvsp[0]; YYACCEPT; ;
    break;}
case 8:
#line 106 "../../tmesh/tmesh-input.y"
{ *_tmesh_input_parsed = yyvsp[0]; YYACCEPT; ;
    break;}
case 9:
#line 107 "../../tmesh/tmesh-input.y"
{ *_tmesh_input_parsed = yyvsp[0]; YYACCEPT; ;
    break;}
case 10:
#line 108 "../../tmesh/tmesh-input.y"
{ *_tmesh_input_parsed = yyvsp[0]; YYACCEPT; ;
    break;}
case 11:
#line 109 "../../tmesh/tmesh-input.y"
{ *_tmesh_input_parsed = yyvsp[0]; YYACCEPT; ;
    break;}
case 12:
#line 110 "../../tmesh/tmesh-input.y"
{ *_tmesh_input_parsed = yyvsp[0]; YYACCEPT; ;
    break;}
case 13:
#line 111 "../../tmesh/tmesh-input.y"
{ YYABORT; ;
    break;}
case 14:
#line 113 "../../tmesh/tmesh-input.y"
{ _tmesh_input_parsed->tmesh_parser_value_token = TMESH_TOKEN_UNDEF; YYACCEPT; ;
    break;}
case 15:
#line 118 "../../tmesh/tmesh-input.y"
{ yyval = yyvsp[-1]; yyval.tmesh_parser_value_token = yyvsp[-2].tmesh_parser_value_token; ;
    break;}
case 16:
#line 122 "../../tmesh/tmesh-input.y"
{ yyval = yyvsp[-1]; yyval.tmesh_parser_value_token = yyvsp[-2].tmesh_parser_value_token; ;
    break;}
case 17:
#line 127 "../../tmesh/tmesh-input.y"
{ yyval = yyvsp[-1]; yyval.tmesh_parser_value_token = yyvsp[-2].tmesh_parser_value_token; ;
    break;}
case 18:
#line 132 "../../tmesh/tmesh-input.y"
{ yyval = yyvsp[-1]; yyval.tmesh_parser_value_token = yyvsp[-2].tmesh_parser_value_token; ;
    break;}
case 20:
#line 140 "../../tmesh/tmesh-input.y"
{
  yyval = yyvsp[-2];
  yyval.tmesh_parser_value_strings[1] = yyvsp[-1].tmesh_parser_value_strings[0];
  yyval.tmesh_parser_value_token = yyvsp[-3].tmesh_parser_value_token;
;
    break;}
case 21:
#line 149 "../../tmesh/tmesh-input.y"
{ yyval = yyvsp[-1]; yyval.tmesh_parser_value_token = yyvsp[-2].tmesh_parser_value_token; ;
    break;}
case 22:
#line 151 "../../tmesh/tmesh-input.y"
{ yyval = yyvsp[-1]; yyval.tmesh_parser_value_token = TMESH_TOKEN_CONNECT; ;
    break;}
case 23:
#line 156 "../../tmesh/tmesh-input.y"
{ yyval = yyvsp[-1]; yyval.tmesh_parser_value_token = yyvsp[-2].tmesh_parser_value_token; ;
    break;}
case 24:
#line 161 "../../tmesh/tmesh-input.y"
{
  yyval = yyvsp[-2];
  yyval.tmesh_parser_value_strings[1] = yyvsp[-1].tmesh_parser_value_strings[0];
  yyval.tmesh_parser_value_token = yyvsp[-3].tmesh_parser_value_token;
;
    break;}
case 25:
#line 170 "../../tmesh/tmesh-input.y"
{ yyval = yyvsp[-1]; yyval.tmesh_parser_value_token = yyvsp[-2].tmesh_parser_value_token; ;
    break;}
case 26:
#line 175 "../../tmesh/tmesh-input.y"
{ yyval = yyvsp[-1]; yyval.tmesh_parser_value_token = yyvsp[-2].tmesh_parser_value_token; ;
    break;}
case 27:
#line 180 "../../tmesh/tmesh-input.y"
{
  yyval = yyvsp[-2];
  yyval.tmesh_parser_value_strings[1] = yyvsp[-1].tmesh_parser_value_strings[0];
  yyval.tmesh_parser_value_token = yyvsp[-3].tmesh_parser_value_token;
;
    break;}
case 30:
#line 194 "../../tmesh/tmesh-input.y"
{ yyval.tmesh_parser_value_strings[0] = NULL; ;
    break;}
case 31:
#line 199 "../../tmesh/tmesh-input.y"
{ 
  _tmesh_parser_argv_arg(&yyval.tmesh_parser_value_argvs[0], 
			 yyvsp[0].tmesh_parser_value_pathname0, 
			 TRUE);
  _tmesh_scanner_in_args();
;
    break;}
case 32:
#line 206 "../../tmesh/tmesh-input.y"
{
  yyval = yyvsp[-1]; 
  _tmesh_parser_argv_arg(&yyval.tmesh_parser_value_argvs[0], 
			 yyvsp[0].tmesh_parser_value_arg, 
			 FALSE);
;
    break;}
case 33:
#line 216 "../../tmesh/tmesh-input.y"
{
  _tmesh_parser_argv_arg(&yyval.tmesh_parser_value_argvs[0], 
			 yyvsp[0].tmesh_parser_value_arg, 
			 TRUE);
;
    break;}
case 34:
#line 222 "../../tmesh/tmesh-input.y"
{ 
  yyval = yyvsp[-1]; 
  _tmesh_parser_argv_arg(&yyval.tmesh_parser_value_argvs[0], 
			 yyvsp[0].tmesh_parser_value_arg, 
			 FALSE);
;
    break;}
case 35:
#line 230 "../../tmesh/tmesh-input.y"
{ _tmesh_scanner_in_args(); ;
    break;}
case 36:
#line 233 "../../tmesh/tmesh-input.y"
{ _tmesh_scanner_in_args(); ;
    break;}
case 37:
#line 237 "../../tmesh/tmesh-input.y"
{
  if (yyvsp[-2].tmesh_parser_value_argvs[0].tmesh_parser_argv_argc > 1) {
    yyerror(_("expected 'at'"));
    YYERROR;
  }
  yyval.tmesh_parser_value_argvs[0] = yyvsp[-2].tmesh_parser_value_argvs[0];
  yyval.tmesh_parser_value_argvs[1].tmesh_parser_argv_argv = NULL;
  yyval.tmesh_parser_value_argvs[2] = yyvsp[0].tmesh_parser_value_argvs[0];
;
    break;}
case 38:
#line 247 "../../tmesh/tmesh-input.y"
{
  yyval.tmesh_parser_value_argvs[0] = yyvsp[-4].tmesh_parser_value_argvs[0];
  yyval.tmesh_parser_value_argvs[1] = yyvsp[-2].tmesh_parser_value_argvs[0];
  yyval.tmesh_parser_value_argvs[2] = yyvsp[0].tmesh_parser_value_argvs[0];
;
    break;}
case 39:
#line 253 "../../tmesh/tmesh-input.y"
{
  yyval.tmesh_parser_value_argvs[0] = yyvsp[-2].tmesh_parser_value_argvs[0];
  yyval.tmesh_parser_value_argvs[1] = yyvsp[0].tmesh_parser_value_argvs[0];
  yyval.tmesh_parser_value_argvs[2].tmesh_parser_argv_argv = NULL;
;
    break;}
case 41:
#line 263 "../../tmesh/tmesh-input.y"
{ yyval.tmesh_parser_value_strings[0] = NULL; ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 543 "/usr/pkg/share/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (int)(sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (int)(sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;

 yyacceptlab:
  /* YYACCEPT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 0;

 yyabortlab:
  /* YYABORT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 1;
}
#line 266 "../../tmesh/tmesh-input.y"


/* this adds a new argument to an argument vector: */
static void
_tmesh_parser_argv_arg(struct tmesh_parser_argv *argv, char *arg, int new)
{

  /* if we're starting a new argv, allocate the initial vector, else
     make sure the argv has enough room for the new argument and a
     trailing NULL that _tmesh_command_connect will add later: */
  if (new) {
    argv->tmesh_parser_argv_size = 8;
    argv->tmesh_parser_argv_argv = 
      _tmesh_gc_new(_tmesh_input,
		    char *, 
		    argv->tmesh_parser_argv_size);
    argv->tmesh_parser_argv_argc = 0;
  }
  else if ((argv->tmesh_parser_argv_argc + 1)
	   >= argv->tmesh_parser_argv_size) {
    argv->tmesh_parser_argv_size +=
      (2
       + (argv->tmesh_parser_argv_size >> 1));
    argv->tmesh_parser_argv_argv = 
      _tmesh_gc_renew(_tmesh_input,
		      char *, 
		      argv->tmesh_parser_argv_argv,
		      argv->tmesh_parser_argv_size);
  }

  /* put in the new argument: */
  argv->tmesh_parser_argv_argv[argv->tmesh_parser_argv_argc++] = arg;
}

/* this is called by the parser when it encounters an error: */
static void
yyerror(char *msg)
{
  tme_output_append(_tmesh_output, msg);
  _tmesh_input->tmesh_scanner.tmesh_scanner_in_args = FALSE;
}

/* this is called by the parser when args can be expected: */
static void
_tmesh_scanner_in_args(void)
{
  _tmesh_input->tmesh_scanner.tmesh_scanner_in_args = TRUE;
}

/* this matches a collected token: */
static int
_tmesh_scanner_token(struct tmesh_scanner *scanner)
{
  int token;
  char *string;
  int keep_string;

  /* if we have no collected token, return no token: */
  if (scanner->tmesh_scanner_token_string_size == 0
      || scanner->tmesh_scanner_token_string_len == 0) {
    return (TMESH_TOKEN_UNDEF);
  }

  /* get the collected token: */
  string = scanner->tmesh_scanner_token_string;
  string[scanner->tmesh_scanner_token_string_len] = '\0';

  /* assume we won't need to keep this string: */
  keep_string = FALSE;

  /* the reserved word "at" is always recognized, since it can
     terminate a list of arguments: */
  if (!strcmp(string, "at")) {
    token = TMESH_TOKEN_AT;
    scanner->tmesh_scanner_in_args = FALSE;
  }
  
  /* if we're in arguments, every other collected token is an argument: */
  else if (scanner->tmesh_scanner_in_args) {
    token = TMESH_TOKEN_ARG;
    keep_string = TRUE;
  }

  /* otherwise, if we're not in arguments, every other collected token
     is either a reserved word, options, or a pathname: */
  else {
    if (!strcmp(string, "source")) {
      token = TMESH_TOKEN_SOURCE;
    }  
    else if (!strcmp(string, "cd")) {
      token = TMESH_TOKEN_CD;
    }  
    else if (!strcmp(string, "pwd")) {
      token = TMESH_TOKEN_CD;
    }  
    else if (!strcmp(string, "ls")) {
      token = TMESH_TOKEN_LS;
    }  
    else if (!strcmp(string, "rm")) {
      token = TMESH_TOKEN_RM;
    }  
    else if (!strcmp(string, "connect")) {
      token = TMESH_TOKEN_CONNECT;
    }  
    else if (!strcmp(string, "mkdir")) {
      token = TMESH_TOKEN_MKDIR;
    }  
    else if (!strcmp(string, "rmdir")) {
      token = TMESH_TOKEN_RMDIR;
    }  
    else if (!strcmp(string, "mv")) {
      token = TMESH_TOKEN_MV;
    }
    else if (!strcmp(string, "command")) {
      token = TMESH_TOKEN_COMMAND;
    }  
    else if (!strcmp(string, "log")) {
      token = TMESH_TOKEN_LOG;
    }  
    else if (!strcmp(string, "alias")) {
      token = TMESH_TOKEN_ALIAS;
    }  
    else if (string[0] == '-') {
      token = TMESH_TOKEN_OPTS;
      keep_string = TRUE;
    }
    else {
      token = TMESH_TOKEN_PATHNAME;
      keep_string = TRUE;
    }
  }

  /* if we need to keep this string, put it in yylval, else recycle it: */
  yylval.tmesh_parser_value_token = token;
  if (keep_string) {
    yylval.tmesh_parser_value_strings[0] = string;
    scanner->tmesh_scanner_token_string_size = 0;
  }
  else {
    yylval.tmesh_parser_value_strings[0] = NULL;
    scanner->tmesh_scanner_token_string_len = 0;
  }

  return (token);
}

/* our scanner: */
int
yylex(void)
{
  struct tmesh_scanner *scanner;
  struct tmesh_io_stack *stack;
  struct tmesh_io *source;
  int token, c;
  
  /* recover our scanner state: */
  scanner = &_tmesh_input->tmesh_scanner;
  stack = _tmesh_input->tmesh_io_stack;
  source = &stack->tmesh_io_stack_io;

  /* bump the input line: */
  source->tmesh_io_input_line += scanner->tmesh_scanner_next_line;
  scanner->tmesh_scanner_next_line = 0;

  /* if we previously scanned the next token to return, return it
     and clear it, unless it's EOF, which sticks: */
  token = scanner->tmesh_scanner_token_next;
  if (token != TMESH_TOKEN_UNDEF) {
    if (token != TMESH_TOKEN_EOF) {
      scanner->tmesh_scanner_token_next = TMESH_TOKEN_UNDEF;
    }
    return (token);
  }

  /* loop forever: */
  for (;;) {

    /* get the next character: */
    c = scanner->tmesh_scanner_c_next;
    if (c == TMESH_C_UNDEF) {
      c = (*source->tmesh_io_getc)(source);
    }
    scanner->tmesh_scanner_c_next = TMESH_C_UNDEF;

    /* if this is an EOF: */
    if (c == TMESH_C_EOF) {

      /* turn c into the EOF semicolon: */
      c = TMESH_C_EOF_SEMICOLON;

      /* if we have collected a token, save the EOF semicolon and return the token: */
      token = _tmesh_scanner_token(scanner);
      if (token != TMESH_TOKEN_UNDEF) {
	scanner->tmesh_scanner_c_next = c;
	return (token);
      }
    }

    /* if this is an EOF semicolon: */
    if (c == TMESH_C_EOF_SEMICOLON) {

      /* quoted strings and comments (and commands, for that matter) cannot cross EOF boundaries: */
      scanner->tmesh_scanner_in_quotes = FALSE;
      scanner->tmesh_scanner_in_comment = FALSE;

      /* close the now-finished source: */
      (*source->tmesh_io_close)(source, 
				(stack->tmesh_io_stack_next != NULL
				 ? &stack->tmesh_io_stack_next->tmesh_io_stack_io
				 : NULL));

      /* pop the io stack: */
      _tmesh_input->tmesh_io_stack = stack->tmesh_io_stack_next;
      tme_free(source->tmesh_io_name);
      tme_free(stack);
      
      /* if we have emptied the source stack, we are really at EOF,
	 and the next time we're called we will return that: */
      stack = _tmesh_input->tmesh_io_stack;
      source = &stack->tmesh_io_stack_io;
      if (stack == NULL) {
	scanner->tmesh_scanner_token_next = TMESH_TOKEN_EOF;
	return (TMESH_TOKEN_EOF);
      }

      /* return the EOF semicolon: */
      return (';');
    }

    /* if this is a yield: */
    if (c == TMESH_C_YIELD) {

      /* we are yielding: */
      _tmesh_input_yielding = TRUE;

      /* return an EOF token: */
      return (TMESH_TOKEN_EOF);
    }    

    /* if we're in a comment: */
    if (scanner->tmesh_scanner_in_comment) {
      if (c != '\n') {
	continue;
      }
      scanner->tmesh_scanner_in_comment = FALSE;
    }

    /* if this is quotation marks: */
    if (c == '"') {
      scanner->tmesh_scanner_in_quotes = !scanner->tmesh_scanner_in_quotes;
      continue;
    }

    /* other than quotation marks, every character either delimits
       tokens or is collected into the current token: */
    if (

	/* any character inside quotes is collected: */
	scanner->tmesh_scanner_in_quotes

	/* any alphanumeric character is collected: */
	|| isalnum(c)

	/* any period, slash, hyphen, and underscore character is collected: */
	|| c == '.'
	|| c == '/'
	|| c == '-'
	|| c == '_'
	) {

      /* allocate or grow the token buffer as needed.  we always
	 make sure there's room for this new character, and a trailing
	 NUL that _tmesh_scanner_token may add: */
      if (scanner->tmesh_scanner_token_string_size == 0) {
	scanner->tmesh_scanner_token_string_len = 0;
	scanner->tmesh_scanner_token_string_size = 8;
	scanner->tmesh_scanner_token_string = 
	  _tmesh_gc_new(_tmesh_input,
			char, 
			scanner->tmesh_scanner_token_string_size);
      }
      else if ((scanner->tmesh_scanner_token_string_len + 1)
	       >= scanner->tmesh_scanner_token_string_size) {
	scanner->tmesh_scanner_token_string_size +=
	  (2
	   + (scanner->tmesh_scanner_token_string_size >> 1));
	scanner->tmesh_scanner_token_string = 
	  _tmesh_gc_renew(_tmesh_input,
			  char, 
			  scanner->tmesh_scanner_token_string,
			  scanner->tmesh_scanner_token_string_size);
      }

      /* collect the character into the buffer: */
      scanner->tmesh_scanner_token_string[scanner->tmesh_scanner_token_string_len++] = c;
    }

    /* delimit this token: */
    else {

      /* if we have collected a token, save the delimiter and return the token: */
      token = _tmesh_scanner_token(scanner);
      if (token != TMESH_TOKEN_UNDEF) {
	scanner->tmesh_scanner_c_next = c;
	return (token);
      }

      /* a carriage return or a newline becomes a semicolon, and
	 a pound sign begins a comment: */
      if (c == '\n') {
	c = ';';
	scanner->tmesh_scanner_next_line = 1;
      }
      else if (c == '\r') {
	c = ';';
      }
      else if (c == '#') {
	scanner->tmesh_scanner_in_comment = TRUE;
	scanner->tmesh_scanner_in_args = FALSE;
	continue;
      }

      /* return a non-whitespace delimiter as a token, and this resets
         the args state: */
      if (!isspace(c)) {
	scanner->tmesh_scanner_in_args = FALSE;
	return (c);
      }
    }
  }
  /* NOTREACHED */
}

/* this is called to parse input: */
int
_tmesh_yyparse(struct tmesh *tmesh, struct tmesh_parser_value *value, char **_output, int *_yield)
{
  struct tmesh_scanner *scanner;
  int rc;
  int command;

  /* initialize the scanner: */
  scanner = &tmesh->tmesh_scanner;
  scanner->tmesh_scanner_token_next = TMESH_TOKEN_UNDEF;
  scanner->tmesh_scanner_c_next = TMESH_C_UNDEF;
  scanner->tmesh_scanner_in_comment = FALSE;
  scanner->tmesh_scanner_in_quotes = FALSE;
  scanner->tmesh_scanner_in_args = FALSE;
  scanner->tmesh_scanner_token_string_size = 0;

  /* lock the input mutex: */
  tme_mutex_lock(&_tmesh_input_mutex);

  /* set this tmesh for input: */
  _tmesh_input = tmesh;
  _tmesh_output = _output;
  
  /* assume that we will not have to yield: */
  _tmesh_input_yielding = FALSE;

  /* call the parser: */
  _tmesh_input_parsed = value;
  rc = (yyparse()
	? EINVAL
	: TME_OK);

  /* tell our caller if we're yielding: */
  *_yield = _tmesh_input_yielding;

  /* unlock the input mutex: */
  tme_mutex_unlock(&_tmesh_input_mutex);

  /* if the parse was successful, map the command token number to a command number: */
  if (rc == TME_OK && !*_yield) {
    switch (value->tmesh_parser_value_token) {
    default: assert(FALSE);
    case TMESH_TOKEN_UNDEF:	command = TMESH_COMMAND_NOP; break;
    case TMESH_TOKEN_SOURCE:	command = TMESH_COMMAND_SOURCE; break;
    case TMESH_TOKEN_MKDIR:	command = TMESH_COMMAND_MKDIR; break;
    case TMESH_TOKEN_RMDIR:	command = TMESH_COMMAND_RMDIR; break;
    case TMESH_TOKEN_CD:	command = TMESH_COMMAND_CD; break;
    case TMESH_TOKEN_PWD:	command = TMESH_COMMAND_PWD; break;
    case TMESH_TOKEN_LS:	command = TMESH_COMMAND_LS; break;
    case TMESH_TOKEN_CONNECT:	command = TMESH_COMMAND_CONNECT; break;
    case TMESH_TOKEN_RM:	command = TMESH_COMMAND_RM; break;
    case TMESH_TOKEN_COMMAND:	command = TMESH_COMMAND_COMMAND; break;
    case TMESH_TOKEN_LOG:	command = TMESH_COMMAND_LOG; break;
    case TMESH_TOKEN_ALIAS:	command = TMESH_COMMAND_ALIAS; break;
    }
    value->tmesh_parser_value_command = command;
  }

  /* done: */
  return (rc);
}
