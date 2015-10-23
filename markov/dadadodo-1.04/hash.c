/* hash.c --- simple hash tables.
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

#include <stdlib.h>
#include <memory.h>
#include <stdio.h>
#include <ctype.h>
#include "hash.h"

typedef struct bucket bucket;

struct hash_table {
  unsigned long size;
  unsigned long count;
  long (*hash) (const void *);
  int (*compare) (const void *, const void *);
  bucket *buckets;
};

struct bucket {
  const void *key;
  void *value;
};

static const unsigned long primes[] = {
  /* 3, 7, 11, 13, 29, 37, 47, 59, 71, 89, */ 107, 131, 163, 197, 239, 293,
  353, 431, 521, 631, 761, 919, 1103, 1327, 1597, 1931, 2333, 2801, 3371,
  4049, 4861, 5839, 7013, 8419, 10103, 12143, 14591, 17519, 21023, 25229,
  30293, 36353, 43627, 52361, 62851, 75431, 90523, 108631, 130363, 156437,
  187751, 225307, 270371, 324449, 389357, 467237, 560689, 672827, 807403,
  968897, 1162687, 1395263, 1674319, 2009191, 2411033, 2893249 };

static unsigned long
toprime (unsigned long size)
{
  unsigned int i;
  static unsigned int s = (sizeof (primes) / sizeof (*primes)) - 1;
  for (i = 0; i < s; i++)
    if (size <= primes[i])
      return primes[i];
  return primes[s-1];
}


hash_table *
make_hash_table (long size,
		 long (*hash) (const void *),
		 int (*compare) (const void *, const void *))
{
  hash_table *t = (hash_table *) malloc(sizeof(*t));
  if (!t) return 0;
  size = toprime ((13 * size) / 10);
  t->hash = hash;
  t->compare = compare;
  t->size = size;
  t->count = 0;
  t->buckets = (bucket *) calloc(t->size, sizeof(*t->buckets));
  if (!t->buckets)
    {
      free(t);
      return 0;
    }
  return t;
}

void
clrhash (hash_table *table)
{
  table->count = 0;
  memset (table->buckets, 0, table->size * sizeof(*table->buckets));
}

void
free_hash_table (hash_table *table)
{
  free (table->buckets);
  free (table);
}


static int
growhash (hash_table *table, unsigned long new_size)
{
  unsigned long i;
  unsigned long old_size = table->size;
  bucket *old_buckets = table->buckets;
  bucket *new_buckets;

  new_size = toprime (new_size);
  new_buckets = (bucket *) calloc(new_size, sizeof(*table->buckets));

  if (!new_buckets)
    return -1;

  table->size = new_size;
  table->buckets = new_buckets;
  table->count = 0;

  for (i = 0; i < old_size; i++)
    if (old_buckets[i].key)
      puthash (table, old_buckets[i].key, old_buckets[i].value);

  free (old_buckets);
  return 0;
}

int
puthash (hash_table *table, const void *key, void *value)
{
  unsigned long hash = table->hash (key);
  unsigned long size = table->size;
  unsigned long n;
  bucket *b;

  if (size < (1 + ((13 * table->count) / 10)))
    {
      int status;
/*      printf("\nrehashing (%d for %d)\n", table->size, table->count);*/
      status = growhash (table, size + 1);
      if (status < 0) return status;
      size = table->size;
    }

  n = hash % size;
  b = &table->buckets[n];

  if (b->key &&
      key != b->key &&
      table->compare (key, b->key) != 0)
    {
      unsigned long h2 = size - 2;
      unsigned long i = (hash % h2) + 1;
      do
	{
	  n += i;
	  if (n >= size) n -= size;
	  b = &table->buckets [n];
	}
      while (b->key &&
	     key != b->key &&
	     table->compare (key, b->key) != 0);
    }

  if (!b->key)
    table->count++;

  b->key = key;
  b->value = value;
  return 0;
}


void *
gethash (hash_table *table, const void *key, void *def)
{
  unsigned long hash = table->hash (key);
  unsigned long size = table->size;
  unsigned long n = hash % size;
  bucket *b = &table->buckets[n];

  if (b->key &&
      key != b->key &&
      table->compare (key, b->key) != 0)
    {
      unsigned long h2 = size - 2;
      unsigned long i = (hash % h2) + 1;
      do
	{
	  n += i;
	  if (n >= size) n -= size;
	  b = &table->buckets [n];
	}
      while (b->key &&
	     key != b->key &&
	     table->compare (key, b->key) != 0);
    }

  return b->value;
}


int
maphash (hash_table *table,
	 int (*mapper) (const void *key, const void *value, void *arg),
	 void *arg)
{
  unsigned long i;
  int status;
  if (table)
    for (i = 0; i < table->size; i++)
      if (table->buckets[i].key)
	{
	  status = mapper (table->buckets[i].key, table->buckets[i].value,arg);
	  if (status != 0) return status;
	}
  return 0;
}


unsigned long
string_hash (const unsigned char *x)
{ 
  unsigned long g, h = 0;
  if (!x) return 0;
  while (*x != 0)
    {
      h = (h << 4) + *x++;
      if ((g = h & 0xf0000000) != 0)
	h = (h ^ (g >> 24)) ^ g;
    }
  return h;
}

unsigned long
string_case_hash (const unsigned char *x)
{ 
  unsigned long g, h = 0;
  if (!x) return 0;
  while (*x != 0)
    {
      h = (h << 4) + toupper(*x++);
      if ((g = h & 0xf0000000) != 0)
	h = (h ^ (g >> 24)) ^ g;
    }
  return h;
}



/* english-centric phoenetic hashing */

#ifdef SOUNDEX

static const unsigned char soundex[] = {
  7,1,2,3,7,1,2,7,7,2,2,4,5,5,7,1,2,6,2,3,7,1,7,2,7,2,0 };

unsigned long
soundex_hash (const unsigned char *string)
{
  unsigned long result = 0;
  const unsigned char *s;
  unsigned char last_letter = 0;
  unsigned char last_class = 0;
  unsigned char this_letter = 0;
  unsigned char this_class = 0;

  for (s = string; *s; s++)
    {
      this_letter = toupper(*s);
      if (this_letter < 'A' || this_letter > 'Z')
	continue;
      this_letter -= 'A';

      this_class = soundex[(int) this_letter];

      if (this_class != 7)
	result = (result << 3) | this_class;
      else
	{
	  /* could be H or W separating like letters */
	  if (this_letter == 'H' || this_letter == 'W')
	    {
	      unsigned char next_letter = toupper(s[1]);
	      if (next_letter >= 'A' && next_letter <= 'Z' &&
		  soundex[(int) (next_letter -= 'A')] == last_class)
		{
		  s++;
		  this_letter = next_letter;
		  this_class = soundex[(int) next_letter];
		  if (!*s) break;
		}
	    }
	}
      last_letter = this_letter;
      last_class = this_class;
    }
  return result;
}

#endif
