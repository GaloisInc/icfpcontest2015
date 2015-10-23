/* parse.h --- generating a markov chain.
 * DadaDodo, Copyright (c) 1997, 2003 Jamie Zawinski <jwz@jwz.org>
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation.  No representations are made about the suitability of this
 * software for any purpose.  It is provided "as is" without express or 
 * implied warranty.
 */

#ifndef __DADADODO_PARSE_H__
#define __DADADODO_PARSE_H__

#include "hash.h"

typedef struct pword pword;

extern pword *scan_line (unsigned char *line, hash_table *table, pword *prev);

extern int total_words;
extern int total_links;
extern int total_starters;

#endif /* __DADADODO_PARSE_H__ */
