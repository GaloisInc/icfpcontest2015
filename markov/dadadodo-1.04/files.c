/* files.c --- input and output.
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
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <memory.h>

#include <sys/types.h>
#include <netinet/in.h>		/* for htonl */

#include "dadadodo.h"
#include "files.h"
#include "parse.h"
#include "parsei.h"
#include "generate.h"


/* Tick this when the file format changes.  No relation to version.h. */
#define FILE_VERSION 1


/* Output. */

static unsigned char write_buf[10240];
static int write_fp = 0;


static int
write_flush (FILE *out)
{
  int status;
  if (!write_fp) return 0;
  status = fwrite (write_buf, 1, write_fp, out);
  write_fp = 0;
  return status;
}

static int
write_int32 (FILE *out, int x)
{
  u_long lx = (u_long) x;   /* no htonl, since we're writing big-endian */
  write_buf[write_fp++] = (lx >> 24);
  write_buf[write_fp++] = (lx >> 16) & 0xFF;
  write_buf[write_fp++] = (lx >> 8) & 0xFF;
  write_buf[write_fp++] = lx & 0xFF;
  if (write_fp >= (sizeof(write_buf)-4))
    return write_flush(out);
  return 0;
}


static int
write_int16 (FILE *out, int x)
{
  u_short sx = (u_short) x;   /* no htonl, since we're writing big-endian */
  write_buf[write_fp++] = (sx >> 8) & 0xFF;
  write_buf[write_fp++] = sx & 0xFF;
  if (write_fp >= (sizeof(write_buf)-4))
    return write_flush(out);
  return 0;
}


static int
write_int8 (FILE *out, int x)
{
  u_short sx = (u_short) x;   /* no htonl, since we're writing big-endian */
  write_buf[write_fp++] = ((sx & 0xFF) ? (sx & 0xFF) : (sx >> 8));
  if (write_fp >= (sizeof(write_buf)-4))
    return write_flush(out);
  return 0;
}


int
write_dadadodo_file (FILE *out, const char *output_name)
{
  int status;
  int i;
  int (*write_fn) (FILE *out, int x) = write_int32;

  if (output_name)
    fprintf (stderr, "writing %s (%d words, %d pairs)\n",
	     output_name, total_words, total_links/2);

  fputs (DADADODO_MAGIC, out);

  /* Write file version. */
  status = write_int32 (out, FILE_VERSION);
  if (status < 0) return status;

  /* Write `total_words' */
  status = write_int32 (out, total_words);
  if (status < 0) return status;

  /* Write `total_links' */
  status = write_int32 (out, total_links);
  if (status < 0) return status;

  /* Write out the strings. */
  {
    string_pool *p, **pools;
    int npools = 0;
    int string_bytes = 0;
    for (p = spool; p; p = p->next)
      {
	npools++;

	if (p == spool)
	  string_bytes += p->fp;
	else
	  string_bytes += STRING_POOL_COUNT;
      }

    pools = (string_pool **) malloc(npools * sizeof(*pools));
    if (!pools) return -1;

    i = npools-1;
    for (p = spool; p; p = p->next, i--)
      pools[i] = p;
      
    /* Write total string bytes */
    status = write_int32 (out, string_bytes);
   if (status < 0) return status;

    /* Important to flush now, since we call fwrite... */
    status = write_flush (out);
    if (status < 0) return status;

    /* Write out the string data.
       I wanted to just write out the strings, one after another, and read
       them into one contiguous segment -- but that doesn't work, because
       if you malloc something larger than a page, the read() syscall can't
       write into it -- on Irix at least, you die with a "memory segment
       error".  So, we write them out in ~500k chunks, padding with nulls
       between chunks so that strings aren't split across them.  (The last
       chunk may be less than 500k and isn't padded.)
     */
    for (i = 0; i < npools; i++)
      {
	p = pools[i];

	if (i == npools-1)
	  status = fwrite (p->chars, 1, p->fp, out);
	else
	  {
	    if (p->fp < STRING_POOL_COUNT)
	      memset (p->chars + p->fp, 0, STRING_POOL_COUNT - p->fp);
	    status = fwrite (p->chars, 1, STRING_POOL_COUNT, out);
	  }

	free (p);
	if (status < 0) return status;
      }
    free (pools);
  }


  /* Write out the `word' structures. */
  {
    pword_pool *p, **pools;
    int npools = 0;
    for (p = wpool; p; p = p->next)
      npools++;
    pools = (pword_pool **) malloc(npools * sizeof(*pools));
    if (!pools) return -1;

    for (p = wpool, i = 0; p; p = p->next, i++)
      pools[npools-i-1] = p;
      
    /* Decide on a word-length. */
    if (total_words > 0xFFFF)
      write_fn = write_int32;
    else if (total_words > 0xFF)
      write_fn = write_int16;
    else
      write_fn = write_int8;

    for (i = 0; i < npools; i++)
      {
	int j;
	p = pools[i];
	for (j = 0; j < p->fp; j++)
	  {
	    int k;
	    pword *w = &p->pwords[j];
# define WRITE(INT) \
		status = write_fn(out,INT); \
		if (status < 0) return status
	    WRITE(w->count);
	    WRITE(w->start);
	    WRITE(w->cap);
	    WRITE(w->comma);
	    WRITE(w->period);
	    WRITE(w->quem);
	    WRITE(w->bang);
	    WRITE(w->succ_length);
	    WRITE(w->pred_length);
	    for (k = 0; k < w->succ_length; k++)
	      {
		WRITE(w->succ[k].count);
		WRITE(w->succ[k].word->id);
	      }
	    for (k = 0; k < w->pred_length; k++)
	      {
		WRITE(w->pred[k].count);
		WRITE(w->pred[k].word->id);
	      }
# undef WRITE
	    if (w->succ) free (w->succ);
	    if (w->pred) free (w->pred);
	  }
      }

    /* Compute and write `total_starters' */
    {
      int nstarters = 0;
      total_starters = 0;
      for (i = 0; i < npools; i++)
	{
	  int j;
	  p = pools[i];
	  for (j = 0; j < p->fp; j++)
	    {
	      pword *w = &p->pwords[j];
	      if (w->start)
		{
		  total_starters += w->start;
		  nstarters++;
		}
	    }
	}
      status = write_int32 (out, total_starters);
      if (status < 0) return status;

      status = write_int32 (out, nstarters);
      if (status < 0) return status;

      /* Write out `starters' array. */
      for (i = 0; i < npools; i++)
	{
	  int j;
	  p = pools[i];
	  for (j = 0; j < p->fp; j++)
	    {
	      pword *w = &p->pwords[j];
	      if (w->start)
		write_fn(out, w->id);
	    }
	}
    }

    for (i = 0; i < npools; i++)
      free (pools[i]);
    free (pools);
  }

  status = write_flush (out);
  if (status < 0) return status;
  status = fflush (out);
  return status;
}



/* Input. */

int
read_dadadodo_file (FILE *in)
{
  int i = 0;
  unsigned char *s = 0;
  unsigned char **as = 0, **as2 = 0;
  word *aw = 0;
  word_link *al = 0, *al_l = 0;
  long tw = 0, tl = 0, sb = 0;
  unsigned char **spools = 0;
  int nspools = 0;
  int nstarters = 0;
  int ts = 0;
  int *st = 0;
  int fv = 0;

  void *buf = 0;
  int buf_size = 0;
  int buf_elt_size = 0;
  u_short scratch_ushort = 0;

  s = fgets (write_buf, 100, in);
  if (!s || strncmp(s, DADADODO_MAGIC, strlen(DADADODO_MAGIC)))
    {
      fprintf (stderr, "not a DadaDodo Data file\n");
      goto FAIL;
    }

  /* Write file version. */
  if (4 != fread ((void *) &fv, 1, 4, in))
    goto FAIL;
  fv = (long) ntohl (fv);
  if (fv != FILE_VERSION)
    {
      fprintf (stderr,
	       "incompatible dadadodo file version: %d instead of %d\n",
	       fv, FILE_VERSION);
      goto FAIL;
    }

  if (4 != fread ((void *) &tw, 1, 4, in))
    goto FAIL;
  tw = (long) ntohl (tw);
  if (!tw) goto FAIL;

  if (4 != fread ((void *) &tl, 1, 4, in))
    goto FAIL;
  tl = (long) ntohl (tl);
  if (!tl) goto FAIL;

  if (4 != fread ((void *) &sb, 1, 4, in))
    goto FAIL;
  sb = (long) ntohl (sb);
  if (!sb) goto FAIL;


  nspools = (sb + STRING_POOL_COUNT - 1) / STRING_POOL_COUNT;
  spools = (unsigned char **) malloc (nspools * sizeof(*spools));
  if (!spools) goto FAIL;
  memset (spools, 0, nspools * sizeof(*spools));
  for (i = 0; i < nspools; i++)
    {
      if (i == nspools-1)
	spools[i] = (unsigned char *) malloc (sb % STRING_POOL_COUNT);
      else
	spools[i] = (unsigned char *) malloc (STRING_POOL_COUNT);
      if (!spools[i])
	goto FAIL;
    }

  as = (unsigned char **) malloc (tw * sizeof(*as));
  if (!as) goto FAIL;

  aw = (word *) malloc (tw * sizeof(*aw));
  if (!aw) goto FAIL;

  al = (word_link *) malloc (tl * sizeof(*al));
  if (!al) goto FAIL;


  /* Read in the string data */

  as2 = as;
  for (i = 0; i < nspools; i++)
    {
      unsigned char *s, *s2;
      int bytes_to_read = (i == nspools-1
			   ? (sb % STRING_POOL_COUNT)
			   : STRING_POOL_COUNT);
      int this_time = fread (spools[i], 1, bytes_to_read, in);
      if (this_time != bytes_to_read)
	{
	  perror("read error");
	  goto FAIL;
	}

      /* Fill in all_strings. */
      s = spools[i];
      s2 = s + bytes_to_read;
      while (s < s2)
	{
	  *as2++ = s;
	  s += strlen(s) + 1;
	  while (s < s2 && !*s)
	    s++;
	}
    }


  buf_size = 50;
  buf = (void *) malloc (buf_size * sizeof(u_long));
  if (!buf) goto FAIL;


  /* Decide on a word-length. */
  if (tw > 0xFFFF)
    buf_elt_size = 4;
  else if (tw > 0xFF)
    buf_elt_size = 2;
  else
    buf_elt_size = 1;

  /* Read in the word data */

  al_l = al;
  for (i = 0; i < tw; i++)
    {
      word *w = &aw[i];
      int j = fread (buf, buf_elt_size, 9, in);
      if (j != 9)
	{
	  perror("short read");
	  goto FAIL;
	}
      w->string      = i;

      switch (buf_elt_size)
	{
	case 4:
	  w->count       = ntohl(((u_long *) buf) [0]);
	  w->start       = ntohl(((u_long *) buf) [1]);
	  w->cap         = ntohl(((u_long *) buf) [2]);
	  w->comma       = ntohl(((u_long *) buf) [3]);
	  w->period      = ntohl(((u_long *) buf) [4]);
	  w->quem        = ntohl(((u_long *) buf) [5]);
	  w->bang        = ntohl(((u_long *) buf) [6]);
	  w->succ_length = ntohl(((u_long *) buf) [7]);
	  w->pred_length = ntohl(((u_long *) buf) [8]);
	  break;
	case 2:
	  w->count       = ntohs(((u_short *) buf) [0]);
	  w->start       = ntohs(((u_short *) buf) [1]);
	  w->cap         = ntohs(((u_short *) buf) [2]);
	  w->comma       = ntohs(((u_short *) buf) [3]);
	  w->period      = ntohs(((u_short *) buf) [4]);
	  w->quem        = ntohs(((u_short *) buf) [5]);
	  w->bang        = ntohs(((u_short *) buf) [6]);
	  w->succ_length = ntohs(((u_short *) buf) [7]);
	  w->pred_length = ntohs(((u_short *) buf) [8]);
	  break;
	default:
# define NTOHC(C) ((scratch_ushort = ntohs((C))), \
		   (scratch_ushort & 0xFF \
		    ? scratch_ushort & 0xFF \
		    : scratch_ushort >> 8))

	  w->count       = NTOHC(((unsigned char *) buf) [0]);
	  w->start       = NTOHC(((unsigned char *) buf) [1]);
	  w->cap         = NTOHC(((unsigned char *) buf) [2]);
	  w->comma       = NTOHC(((unsigned char *) buf) [3]);
	  w->period      = NTOHC(((unsigned char *) buf) [4]);
	  w->quem        = NTOHC(((unsigned char *) buf) [5]);
	  w->bang        = NTOHC(((unsigned char *) buf) [6]);
	  w->succ_length = NTOHC(((unsigned char *) buf) [7]);
	  w->pred_length = NTOHC(((unsigned char *) buf) [8]);
	  break;
	}


      /* Make sure buf is big enough */
      {
	int L = (w->succ_length > w->pred_length
		 ? w->succ_length : w->pred_length);
	if (buf_size < buf_elt_size * L * 2)
	  {
	    int s = buf_elt_size * L * 2 * 2;
	    void *b2 = (void *) realloc (buf, s);
	    if (!b2) goto FAIL;
	    buf_size = s;
	    buf = b2;
	  }
      }


      if (!w->succ_length)
	w->succ = 0;
      else
	{
	  j = fread (buf, buf_elt_size, w->succ_length * 2, in);
	  if (j != (w->succ_length * 2))
	    {
	      perror("short read");
	      goto FAIL;
	    }

	  w->succ = al_l;
	  for (j = 0; j < w->succ_length; j++)
	    {
	      switch (buf_elt_size)
		{
		case 4:
		  al_l->count = ntohl(((u_long *) buf) [j * 2]);
		  al_l->word  = ntohl(((u_long *) buf) [j * 2 + 1]);
		  break;
		case 2:
		  al_l->count = ntohs(((u_short *) buf) [j * 2]);
		  al_l->word  = ntohs(((u_short *) buf) [j * 2 + 1]);
		  break;
		default:
		  al_l->count = NTOHC(((unsigned char *) buf) [j * 2]);
		  al_l->word  = NTOHC(((unsigned char *) buf) [j * 2 + 1]);
		}

	      al_l++;
	    }
	}

      if (!w->pred_length)
	w->pred = 0;
      else
	{
	  j = fread (buf, buf_elt_size, w->pred_length * 2, in);
	  if (j != (w->pred_length * 2))
	    {
	      perror("short read");
	      goto FAIL;
	    }

	  w->pred = al_l;
	  for (j = 0; j < w->pred_length; j++)
	    {
	      switch (buf_elt_size)
		{
		case 4:
		  al_l->count = ntohl(((u_long *) buf) [j * 2]);
		  al_l->word  = ntohl(((u_long *) buf) [j * 2 + 1]);
		  break;
		case 2:
		  al_l->count = ntohs(((u_short *) buf) [j * 2]);
		  al_l->word  = ntohs(((u_short *) buf) [j * 2 + 1]);
		  break;
		default:
		  al_l->count = NTOHC(((unsigned char *) buf) [j * 2]);
		  al_l->word  = NTOHC(((unsigned char *) buf) [j * 2 + 1]);
		}

	      al_l++;
	    }
	}
    }

  if (buf) free (buf);
  buf = 0;
  buf_size = 0;

  if (4 != fread ((void *) &ts, 1, 4, in))
    goto FAIL;
  ts = (long) ntohl (ts);
  if (4 != fread ((void *) &nstarters, 1, 4, in))
    goto FAIL;
  nstarters = (long) ntohl (nstarters);

  st = (int *) malloc ((nstarters + 1) * sizeof(*st));
  if (!st) goto FAIL;

  switch (buf_elt_size)
    {
    case 4:
      buf = st;
      break;
    case 2:
      buf_size = (nstarters + 1) * buf_elt_size;
      buf = (void *) malloc (buf_size);
      if (!buf) goto FAIL;
      break;
    default:
      buf_elt_size = sizeof(char);
      buf_size = (nstarters + 1) * buf_elt_size;
      buf = (void *) malloc (buf_size);
      if (!buf) goto FAIL;
      break;
    }

  {
    int bytes_to_read = nstarters * buf_elt_size;
    char *b2 = (unsigned char *) buf;
    do {
      int bytes_read = fread (b2, 1, bytes_to_read, in);
      if (bytes_read <= 0)
	{
	  if (buf == st)
	    buf = 0;
	  goto FAIL;
	}
      bytes_to_read -= bytes_read;
      b2 += bytes_read;
    } while (bytes_to_read > 0);
  }

  if (buf == st)
    {
      for (i = 0; i < nstarters; i++)
        st[i] = ntohl (((u_long *)buf) [i]);
      buf = 0;
    }
  else if (buf_elt_size == sizeof(u_short))
    for (i = 0; i < nstarters; i++)
      st[i] = ntohs (((u_short *)buf) [i]);
  else
    for (i = 0; i < nstarters; i++)
      st[i] = NTOHC (((unsigned char *)buf) [i]);

# undef NTOHC

  if (buf)
    free (buf);
  total_words = tw;
  all_words = aw;
  all_strings = as;
  total_starters = ts;
  starters = st;
  return 0;

 FAIL:
  if (buf) free (buf);
  if (as) free (as);
  if (aw) free (aw);
  if (al) free (al);
  if (st) free (st);
  if (spools)
    {
      for (i = 0; i < nspools; i++)
	if (spools[i]) free(spools[i]);
      free (spools);
    }
  return -1;
}
