/* parse.c --- generating a markov chain.
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <memory.h>

#include "dadadodo.h"
#include "parse.h"
#include "parsei.h"


int total_words = 0;
int total_links = 0;


pword_pool *wpool = 0;
string_pool *spool = 0;


static const unsigned char *
new_string (const unsigned char *s)
{
  int L = strlen(s);
  if (!spool || spool->fp+L+2 >= STRING_POOL_COUNT)
    {
      string_pool *p = (string_pool *) malloc (sizeof(*p));
      if (!p) return 0;
      p->fp = 0;
      p->next = spool;
      spool = p;
    }
  memcpy (spool->chars + spool->fp, s, L+1);
  s = spool->chars + spool->fp;
  spool->fp += L+1;
  return s;
}


static pword *
new_pword (const unsigned char *string)
{
  pword *pw;
  string = new_string (string);
  if (!string) return 0;
  if (!wpool || wpool->fp+1 >= PWORD_POOL_COUNT)
    {
      pword_pool *p = (pword_pool *) calloc (1, sizeof(*p));
      if (!p) return 0;
      p->fp = 0;
      p->next = wpool;
      wpool = p;
    }

  pw = &wpool->pwords[wpool->fp++];
  pw->id = total_words++;
  pw->string = string;
  return pw;
}


static int
increment (pword *pword, pword_link **listP, int *lengthP, int *sizeP)
{
  int size = *sizeP;
  int length = *lengthP;
  pword_link *list = *listP;

  int bot = 0;
  int top = length;

  /* Binary search for word in the pword_link list.
     Assume it's sorted by &pword_link->word.
   */
  while (top > bot)
    {
      int mid = ((top - bot) >> 1) + bot;
      if (pword == list[mid].word)
	{
	  list[mid].count++;
	  return 0;
	}
      else if (pword < list[mid].word)
	top = mid;
      else if (bot == mid)
	break;
      else
	bot = mid;
    }

  /* We didn't find it, but top == bot == the place it ought to go.
     Expand the array, move the others down, and insert it.  This is
     a lot of memmoves, but...
   */
  if (size == 0)
    {
      size = 10;
      list = (pword_link *) calloc (size, sizeof(*list));
      if (!list) return 0;
      *listP = list;
      *sizeP = size;
    }
  else if (length+1 >= size)
    {
      size = (((*sizeP + 10) * 13) / 10);
      list = (pword_link *) realloc (list, size * sizeof(*list));
      if (!list) return 0;
      memset (list+length, 0, (size-length) * sizeof(*list));
      *listP = list;
      *sizeP = size;
    }

  if (top != length)
    /* Note: regions overlap, so memmove() better work as advertised. */
    memmove (list+top+1, list+top, (length - top) * sizeof(*list));

  total_links++;
  (*lengthP)++;
  list[top].word = pword;
  list[top].count = 1;

  return 0;
}


static int
follow (pword *prev, pword *next)
{
  int s = increment (next, &prev->succ, &prev->succ_length, &prev->succ_size); 
  if (s < 0) return s;
  return  increment (prev, &next->pred, &next->pred_length, &next->pred_size); 
}


static pword *
intern (const unsigned char *string, hash_table *table)
{
  pword *pw = (pword *) gethash (table, (void *) string, 0);
  if (!pw)
    {
      int status;
      pw = new_pword (string);
      if (! pw) return 0;
      status = puthash (table, (void *) pw->string, (void *) pw);
      if (status < 0)
	{
	  /* free_pword (pw); */
	  return 0;
	}
    }
  return pw;
}

/* Interns and indexes the word (a null-terminated string.)
 */
static pword *
push (const unsigned char *string, char punc, hash_table *table, pword *prev)
{
  pword *pword = intern (string, table);
  int terminal = 0;
  if (!pword) return 0;

  pword->count++;

  if (!prev)
    pword->start++;
  else if (isupper(*string))
    pword->cap++;

  switch (punc)
    {
    case ',': pword->comma++; break;
    case '.': pword->period++; terminal=1; break;
    case ':': pword->period++; terminal=1; break;
    case ';': pword->period++; break;
    case '?': pword->quem++; terminal=1; break;
    case '!': pword->bang++; terminal=1; break;
    case '(': pword->comma++; break;
    case ')': pword->comma++; break;
    }

  if (prev) follow (prev, pword);
  if (terminal) return 0;
  return pword;
}


/* Map over the line, and call push() with each word.
   `line' is modified/destroyed.
 */
pword *
scan_line (unsigned char *line, hash_table *table, pword *prev)
{
  unsigned char *s = line;
  while (*s)
    {
      int had_dot = 0;
      int had_digit = 0;
      unsigned char *start, *end;
      /* Skip forward to first alphanumeric or slash. */
      while (*s && !(isalnum(*s) || *s == '/' || *s == '\\'))
	s++;
      start = s;

      /* Ignore pathnames and URLs. They don't contain nice word selections. */
      if (*start == '/' ||
	  *start == '\\' ||
	  (*start == 'h' && !strncmp(start, "http://", 7)) ||
	  (*start == 'm' && !strncmp(start, "mailto:", 7)) ||
	  (*start == 'f' && !strncmp(start, "ftp://", 6)) ||
	  (*start == 'g' && !strncmp(start, "file://", 7)))
	{
	  while (*s && !isspace(*s))
	    s++;
	  start = s;
	  if (*s) break;
	}

    AGAIN:
      /* Skip forward to next non-alphanumeric-non-apostrophe. */
      while (*s && (isalnum(*s) || *s == '\''))
	{
	  if (isdigit(*s)) had_digit = 1;
	  s++;
	}
      end = s;

      if ((*s == '.' || *s == '@') && isalnum(s[1]))
	{
	  s++;				/* treat "xxx@foo.com" as one pword */
	  had_dot = 1;
	  goto AGAIN;
	}

      if (had_dot)	/* turns out that we get a lot of lists of newsgroup
			   names, so let's just reject all dotted words. */
	continue;

      if (had_digit)	/* Let's just ignore words that contain any digits. */
	continue;

      while (s > start && ispunct(s[-1]))
	s--;

      if (s > start + 30)	/* Ignore words that are more than 30 long. */
	continue;

      if (s != start)
	{
	  unsigned char punc = *s;
	  *s = 0;
	  prev = push (start, punc, table, prev);
	  *s = punc;
	}
      s = end;
    }
  return prev;
}
