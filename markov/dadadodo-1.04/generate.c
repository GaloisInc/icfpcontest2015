/* generate.c --- generating random sentences from a markov chain.
 * DadaDodo, Copyright (c) 1997, 1998, 2003 Jamie Zawinski <jwz@jwz.org>
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
#include <string.h>
#include <stdio.h>
#include <ctype.h>

#include "hash.h"
#include "dadadodo.h"
#include "generate.h"
#include "yarandom.h"


extern int total_words;

word *all_words = 0;
unsigned char **all_strings = 0;

int total_starters = 0;
int *starters = 0;


static word *
random_first_word (void)
{
  word *w = 0;
  int *s = starters;
  int i = RAND(total_starters);
  do {
    w = &all_words[*s];
    i -= w->start;
    s++;
  } while (i > 0);
  return w;
}


static word *
random_linked (word_link *links, int link_length)
{
  int i;
  int count = 0;
  /* Could cache this number by making `word' bigger, but it doesn't seem
     to be a performance problem. */
  for (i = 0; i < link_length; i++)
    count += links[i].count;
  count = RAND(count);
  for (i = 0; i < link_length; i++)
    {
      count -= links[i].count;
      if (count <= 0)
	return &all_words [links[i].word];
    }
  abort();
}

static word *
random_next_word (word *w)
{
  if (w->pred_length && !(RAND(10)))
    /* One time in ten, pick a random sibling instead of a random child. */
    {
      word *w2 = random_next_word (random_linked (w->pred, w->pred_length));
      if (w != w2)  /* but don't stutter. */
	return w2;
    }

  if (w->succ_length)
    return random_linked (w->succ, w->succ_length);
  else if (random() % 4)
    return &all_words[RAND(total_words)];	/* triple word score */
  else
    return 0;
}


int
random_sentence (FILE *stream, int *column, int indent, int fill_column,
		 int html_p)
{
  int count = 0;
  unsigned char buf [10240];
  word *word = random_first_word ();
  int first_p = 1;
  int paren_open_p = 0;
  int open_paren_p = 0;
  unsigned char indent_str[255];

  for (count = 0; count < indent; count++)
    indent_str[count] = ' ';
  indent_str[count] = 0;
  count = 0;

  while (*column < indent)
    {
      fputs(" ", stream);
      (*column)++;
    }

  while (word)
    {
      int open_paren_next_p = 0;
      int close_paren_p = 0;
      int punctuate_chance = RAND(word->count);
      int always_cap_p = (word->count - word->start) <= word->cap;
      int cap_p = (first_p || always_cap_p);
      const unsigned char *c1 = all_strings[word->string];
      unsigned char *c2 = buf;
      int new_col;
      const unsigned char *punc = 0;
      int premature_end_slop = 0;


      if (count > 400)		/* Stuck in a long-running loop. */
	{
	  fprintf(stream, "\nYow!\n%s", indent_str);
	  
	  *column = indent;
	  return count;
	}

      count++;

      /* Every ten words, increase the chance of the sentence ending
	 by 16% (yet, only end at places where periods were possible.)  */
      if ((count % 10) == 0)
	{
	  int delta = (word->count / 6);
	  premature_end_slop += (delta == 0 ? 1 : delta);
	}

      if (punctuate_chance < word->comma)
	{
	  if (paren_open_p)
	    {
	      punc = 0;
	      close_paren_p = 0;
	    }
	  else if (RAND(20))	/* once in 20, use parens instead of commas */
	    punc = ",";
	  else
	    open_paren_next_p = 1;
	}
      else
	{
	  punctuate_chance -= word->comma;
	  if (word->period &&
	      punctuate_chance < word->period + premature_end_slop)
	    {
#if 0
	      if (punctuate_chance >= word->period)
		fprintf(stderr, "premature: %d %d %d %s\n",
			premature_end_slop, word->count, word->period,
			all_strings[word->string]);
#endif

	      if ((RAND(5)) == 0)
		punc = ":";
	      else if ((RAND(4)) == 0)
		punc = ";";
	      else
		{
		  close_paren_p = 1;
		  punc = ". ";
		}
	    }
	  else
	    {
	      punctuate_chance -= word->period;
	      if (punctuate_chance < word->quem)
		punc = "? ";
	      else
		{
		  punctuate_chance -= word->quem;
		  if (punctuate_chance < word->bang)
		    punc = "! ";
		  else
		    punc = 0;
		}
	      if (punc)
		close_paren_p = 1;
	    }
	}

      new_col = *column + strlen (c1) + (punc ? strlen(punc) : 0);
      if (open_paren_p || close_paren_p)
	new_col++;

#if 0
    printf("\n%s\t\tpun=%3d cou=%3d sta=%3d cap=%3d com=%3d per=%3d que=%3d\n",
	   all_strings[word->string],
	   punctuate_chance,
	   word->count, word->start, word->cap, word->comma, word->period,
	   word->quem);
#endif /* 0 */

      first_p = 0;

      if (*column <= indent)
	*column = new_col;
      else if (new_col < fill_column)
	{
	  fputs (" ", stream);
	  *column = new_col+1;
	}
      else
	{
	  fputs ("\n", stream);
	  fputs (indent_str, stream);
	  *column = indent + strlen (c1)+1;
	}


      if (open_paren_p)
	{
	  *c2++ = '(';
	  open_paren_p = 0;
	  open_paren_next_p = 0;
	  paren_open_p = 1;
	}

      /* If the word was always capitalized, then use the capitalization
	 (of all letters) exactly as seen.
	 Else, if we're at the beginning of a sentence, upcase the first
	 character and downcase the rest.
	 Else, capitalize the word as probability suggests. */

      if (cap_p ||
	  (word->count > word->start &&
	   RAND(word->count - word->start) < word->cap))
	*c2++ = toupper (*c1++);

      if (always_cap_p)
	strcpy (c2, c1);
      else
	{
	  while (*c1)
	    *c2++ = tolower (*c1++);
	  *c2 = 0;
	}

      fputs (buf, stream);

      if (punc && (*punc == '.' || *punc == '!' || *punc == '?'))
	word = 0;
      else
	{
	  word = random_next_word (word);

	  if (!word)		/* no next; maybe the word never had a next. */
	    {			/* punctuate it anyway. */
	      switch (RAND(5))
		{
		case 0: punc = "! "; break;
		case 1: punc = "? "; break;
		default: punc = ". "; break;
		}
	      (*column) += 2;
	      close_paren_p = 1;
	    }
	}

      if (paren_open_p && close_paren_p)
	{
	  fputs(")", stream);
	  paren_open_p = 0;
	  close_paren_p = 0;
	  open_paren_next_p = 0;
	}

      if (punc)
	fputs (punc, stream);

      if (open_paren_next_p)
	open_paren_p = 1;
    }

  if (paren_open_p) abort();
  return count;
}

void
word_stats (FILE *stream, word *word)
{
  int i;
  fprintf (stream, "%-16s %5d %5d %5d %5d %5d %5d %5d\n",
	   all_strings[word->string], word->count, word->start, word->cap,
	   word->comma, word->period, word->quem, word->bang);
  if (word->succ_length)
    fprintf (stream, "  -->\n");
  for (i = 0; i < word->succ_length; i++)
    fprintf (stream, "    %5d %s\n",
	     word->succ[i].count,
	     all_strings [all_words[word->succ[i].word].string]);
  if (word->pred_length)
    fprintf (stream, "  <--\n");
  for (i = 0; i < word->pred_length; i++)
    fprintf (stream, "    %5d %s\n",
	     word->pred[i].count,
	     all_strings [all_words[word->pred[i].word].string]);
}

#define STAT_HEAD \
  "\nWORD               COUNT START  CAP  COMMA  END  QUEM  BANG\n\n"

void
stats (FILE *stream)
{
  int i;
  fputs (STAT_HEAD, stream);
  for (i = 0; i < total_words; i++)
    word_stats (stream, &all_words[i]);
  fputs ("\n", stream);
}

void
string_stats (FILE *stream, const unsigned char *s, int print_header)
{
  int i;
  if (print_header)
    fputs (STAT_HEAD, stream);
  for (i = 0; i < total_words; i++)
    if (!strcasecmp (s, all_strings [all_words[i].string]))
      word_stats (stream, &all_words[i]);
}
