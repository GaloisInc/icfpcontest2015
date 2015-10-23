/* DadaDodo, Copyright (c) 1997 Jamie Zawinski <jwz@jwz.org>
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation.  No representations are made about the suitability of this
 * software for any purpose.  It is provided "as is" without express or 
 * implied warranty.
 */

/*#if sizeof(int) >= 4*/
typedef int int32;
typedef unsigned int uint32;
/*#else
typedef long int32;
typedef unsigned long uint32;
#endif*/

typedef struct word word;
typedef struct word_link word_link;

struct word {
  int string;
  int count;
  int start;
  int cap;
  int comma;
  int period;
  int quem;
  int bang;
  int succ_length, pred_length;
  word_link *succ, *pred;
};

struct word_link {
  int count;
  int word;
};

#define RAND(N) ((random() & 0x7FFFFFFF) % (N))
