/* parsei.h --- generating a markov chain.
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

#ifndef __DADADODO_PARSEI_H__
#define __DADADODO_PARSEI_H__

typedef struct pword_link pword_link;

/* A larger version of `struct word' that contains data needed at parse-time
   but not at generate-time. */
struct pword {
  int id;
  const unsigned char *string;
  int count;
  int start;
  int cap;
  int comma;
  int period;
  int quem;
  int bang;
  pword_link *succ, *pred;
  int succ_length, pred_length;
  int succ_size, pred_size;
};

struct pword_link {
  int count;
  pword *word;
};


/* allocation pools. */

typedef struct pword_pool pword_pool;
typedef struct string_pool string_pool;


#define PWORD_POOL_SIZE  (500*1024)
#define STRING_POOL_SIZE (500*1024)
#define PWORD_POOL_COUNT ((PWORD_POOL_SIZE - (sizeof(void *)*4))/sizeof(pword))
#define STRING_POOL_COUNT (STRING_POOL_SIZE - (sizeof (void *) * 4))

struct pword_pool {
  pword pwords [PWORD_POOL_COUNT];
  int fp;
  pword_pool *next;
};

struct string_pool {
  unsigned char chars[STRING_POOL_COUNT];
  int fp;
  string_pool *next;
};

extern pword_pool *wpool;
extern string_pool *spool;

extern word *all_words;
extern unsigned char **all_strings;
extern int *starters;

#endif /* __DADADODO_PARSEI_H__ */
