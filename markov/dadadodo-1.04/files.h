/* files.h --- input and output
 * DadaDodo, Copyright (c) 1997 Jamie Zawinski <jwz@jwz.org>
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation.  No representations are made about the suitability of this
 * software for any purpose.  It is provided "as is" without express or 
 * implied warranty.
 */

#ifndef __DADADODO_FILES_H__
#define __DADADODO_FILES_H__

#define DADADODO_MAGIC "#!DadaDodo\n"
extern int write_dadadodo_file (FILE *out, const char *output_name);
extern int read_dadadodo_file (FILE *in);

#endif /* __DADADODO_FILES_H__ */
