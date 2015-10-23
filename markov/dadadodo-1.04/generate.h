/* generate.h --- generating random sentences from a markov chain.
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

#ifndef __DADADODO_GENERATE_H__
#define __DADADODO_GENERATE_H__

#include "parse.h"

extern int random_sentence (FILE *stream, int *column,
			    int indent, int fill_column, int html_p);
extern void word_stats (FILE *stream, word *word);
extern void string_stats (FILE *stream, const unsigned char *word,
                          int print_header);
extern void stats (FILE *stream);

#endif /* __DADADODO_GENERATE_H__ */
