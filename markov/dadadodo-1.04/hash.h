/* hash.c --- simple hash tables.
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

#ifndef __DADADODO_HASH_H__
#define __DADADODO_HASH_H__

typedef struct hash_table hash_table;

hash_table *make_hash_table (long size,
			     long (*hash) (const void *),
			     int (*compare) (const void *, const void *));

void free_hash_table (hash_table *table);

int puthash (hash_table *table, const void *key, void *value);
void *gethash (hash_table *table, const void *key, void *default_value);
void clrhash (hash_table *table);
int maphash (hash_table *table,
	     int (*mapper) (const void *key, const void *value, void *arg),
	     void *arg);

unsigned long string_hash (const unsigned char *string);
unsigned long string_case_hash (const unsigned char *string);

#endif /* __DADADODO_HASH_H__ */
