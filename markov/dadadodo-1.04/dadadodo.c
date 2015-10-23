/* DadaDodo, Copyright (c) 1997, 1998 Jamie Zawinski <jwz@jwz.org>
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation.  No representations are made about the suitability of this
 * software for any purpose.  It is provided "as is" without express or 
 * implied warranty.
 */

#include "version.h"

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <time.h>
#include <math.h>

#include "hash.h"
#include "dadadodo.h"
#include "parse.h"
#include "files.h"
#include "generate.h"
#include "yarandom.h"

typedef struct {
  const char *string;
  unsigned char latin1_char;
  unsigned char length;
} entity;

static entity entities[] = {
  {"lt", '<', 2},
  {"LT", '<', 2},
  {"gt", '>', 2},
  {"GT", '>', 2},
  {"amp", '&', 3},
  {"AMP", '&', 3},
  {"quot", '\"', 4},
  {"QUOT", '\"', 4},
  {"nbsp", ' ', 4},	/* \240 */
  {"reg", '\256', 3},
  {"REG", '\256', 3},
  {"copy", '\251', 4},
  {"COPY", '\251', 4},

  {"iexcl", '\241', 5},
  {"cent", '\242', 4},
  {"pound", '\243', 5},
  {"curren", '\244', 6},
  {"yen", '\245', 3},
  {"brvbar", '\246', 6},
  {"sect", '\247', 4},
  
  {"uml", '\250', 3},
  {"ordf", '\252', 4},
  {"laquo", '\253', 5},
  {"not", '\254', 3},
  {"shy", '\255', 3},
  {"macr", '\257', 4},

  {"deg", '\260', 3},
  {"plusmn", '\261', 6},
  {"sup2", '\262', 4},
  {"sup3", '\263', 4},
  {"acute", '\264', 5},
  {"micro", '\265', 5},
  {"para", '\266', 4},
  {"middot", '\267', 6},

  {"cedil", '\270', 5},
  {"sup1", '\271', 4},
  {"ordm", '\272', 4},
  {"raquo", '\273', 5},
  {"frac14", '\274', 6},
  {"frac12", '\275', 6},
  {"frac34", '\276', 6},
  {"iquest", '\277', 6},

  {"Agrave", '\300', 6},
  {"Aacute", '\301', 6},
  {"Acirc", '\302', 5},
  {"Atilde", '\303', 6},
  {"Auml", '\304', 4},
  {"Aring", '\305', 5},
  {"AElig", '\306', 5},
  {"Ccedil", '\307', 6},

  {"Egrave", '\310', 6},
  {"Eacute", '\311', 6},
  {"Ecirc", '\312', 5},
  {"Euml", '\313', 4},
  {"Igrave", '\314', 6},
  {"Iacute", '\315', 6},
  {"Icirc", '\316', 5},
  {"Iuml", '\317', 4},

  {"ETH", '\320', 3},
  {"Ntilde", '\321', 6},
  {"Ograve", '\322', 6},
  {"Oacute", '\323', 6},
  {"Ocirc", '\324', 5},
  {"Otilde", '\325', 6},
  {"Ouml", '\326', 4},
  {"times", '\327', 5},

  {"Oslash", '\330', 6},
  {"Ugrave", '\331', 6},
  {"Uacute", '\332', 6},
  {"Ucirc", '\333', 5},
  {"Uuml", '\334', 4},
  {"Yacute", '\335', 6},
  {"THORN", '\336', 5},
  {"szlig", '\337', 5},

  {"agrave", '\340', 6},
  {"aacute", '\341', 6},
  {"acirc", '\342', 5},
  {"atilde", '\343', 6},
  {"auml", '\344', 4},
  {"aring", '\345', 5},
  {"aelig", '\346', 5},
  {"ccedil", '\347', 6},

  {"egrave", '\350', 6},
  {"eacute", '\351', 6},
  {"ecirc", '\352', 5},
  {"euml", '\353', 4},
  {"igrave", '\354', 6},
  {"iacute", '\355', 6},
  {"icirc", '\356', 5},
  {"iuml", '\357', 4},

  {"eth", '\360', 3},
  {"ntilde", '\361', 6},
  {"ograve", '\362', 6},
  {"oacute", '\363', 6},
  {"ocirc", '\364', 5},
  {"otilde", '\365', 6},
  {"ouml", '\366', 4},
  {"divide", '\367', 6},

  {"oslash", '\370', 6},
  {"ugrave", '\371', 6},
  {"uacute", '\372', 6},
  {"ucirc", '\373', 5},
  {"uuml", '\374', 4},
  {"yacute", '\375', 6},
  {"thorn", '\376', 5},
  {"yuml", '\377', 4},
};

static char
get_entity (const unsigned char *string, int length)
{
  int i;
  unsigned char c = *string;
  for (i = 0; i < (sizeof(entities)/sizeof(*entities)); i++)
    if (length == entities[i].length &&
	c == entities[i].string[0] &&
	!strncmp(string, entities[i].string, length))
      return entities[i].latin1_char;
  return 0;
}


#undef DEBUG_CONT
#undef DEBUG_CITE
#undef DEBUG_WROTE
#undef DEBUG_HEAD
#undef DEBUG_UU
#undef DEBUG_QP
#undef DEBUG_HTML
#undef DEBUG_SIG

static int
scan (FILE *file, hash_table *table, char *first_line)
{
  pword *prev = 0;
  int line_tick = 0;
  int line_count = 0;
  unsigned char buf [10240];
  unsigned char *s;
  int L;
  int mailbox_p = 0;
  int in_headers = 0;
  int in_binhex_p = 0;
  int contains_msg = 0;
  int inside_html_tag = 0;
  int in_comment_p = 0;
  int in_sig_p = 0;

  unsigned char *qp_wrap_hack = 0;
  unsigned char *qp_free_wrap_hack = 0;

  if (first_line)
    s = first_line;
  else
    {
      s = fgets (buf, sizeof(buf)-1, file);
      if (!s) return -1;
    }

  mailbox_p = (*s == 'F' && !strncmp(s, "From ", 5));

  do
    {
      if (qp_free_wrap_hack)
	{
	  free (qp_free_wrap_hack);
	  qp_free_wrap_hack = 0;
	}
      if (qp_wrap_hack)
	{
	  qp_free_wrap_hack = (unsigned char *)
	    malloc (strlen(s)+strlen(qp_wrap_hack)+1);
	  if (!qp_free_wrap_hack) return -1;
	  strcpy(qp_free_wrap_hack, qp_wrap_hack);
	  strcat(qp_free_wrap_hack, s);
	  free(qp_wrap_hack);
	  qp_wrap_hack = 0;
	  s = qp_free_wrap_hack;
	}

      L = strlen(s);
      if (L > 0 && (s[L-1] == '\r' || s[L-1] == '\n')) L--;
      if (L > 0 && (s[L-1] == '\r' || s[L-1] == '\n')) L--;

      line_count++;
      if (++line_tick == 200)
	{
	  fprintf (stderr, ".");
	  if ((line_count % (65 * line_tick)) == 0)
	    fprintf (stderr, " %d lines\n", line_count);
	  line_tick = 0;
	}

      /* If inside a binhex section, keep discarding it so long as the lines
	 are the proper length (binhex isn't as easily detectible as base64
	 and uuencode...)
       */
      if (in_binhex_p)
	{
	  if (L != 0 && L != 64)
	    in_binhex_p = 0;
	  continue;			/* swallow one last line */
	}

      /* If an HTML tag spanned multiple lines, keep discarding it. */
      if (inside_html_tag)
	{
#ifdef DEBUG_CONT
	  printf (" CONT: %s", s);
#endif
	  while (*s && *s != '>')
	    s++, L--;
	  if (*s)
	    {
	      s++;
	      L--;
	      inside_html_tag = 0;
	    }
	}
      else if (in_comment_p)
	{
#ifdef DEBUG_CONT
	  printf (" COMMENT CONT: %s", s);
#endif
	  while (*s && (s[0] != '-' || s[1] != '-' || s[2] != '>'))
	    s++, L--;
	  if (*s)
	    {
	      s += 3;
	      L -= 3;
	      in_comment_p = 0;
	    }
	}
      else if (in_sig_p)
	{
	  if (in_sig_p > 20)
	    in_sig_p = 0;
	  else if (*s == 'F' && !strncmp (s, "From ", 5))
	    in_sig_p = 0;
	  else
	    {
#ifdef DEBUG_SIG
	      printf (" SIG %d: %s", in_sig_p, s);
#endif
	      in_sig_p++;
	      continue;
	    }
	}


      if (mailbox_p)
	{

	  /* Strip off all lines from "-- \n" to the end of the message. */
	  if (s[0] == '-' && s[1] == '-' && s[2] == ' ' &&
	      (s[3] == '\n' || s[3] == '\r' || s[3] == 0))
	    {
#ifdef DEBUG_SIG
	      printf ("\n SIG: %s", s);
#endif
	      in_sig_p = 1;
	      continue;
	    }

	  /* Strip off those FUCKING VCARDS too. */
	  if ((*s == 'b' || *s == 'B') && !strncasecmp(s, "begin:", 6))
	    {
	      unsigned char *s2 = s+6;
	      while (isspace(*s2)) s2++;
	      if (!strncasecmp(s2, "vcard", 5))
		{
#ifdef DEBUG_SIG
		  printf ("\n VCARD: %s", s);
#endif
		  in_sig_p = 1;
		  continue;
		}
	    }


	  /* Strip off anything that looks like a citation prefix. */
	  {
	    int got_some = 0;
	    unsigned char *s2 = s;
	    while (isspace(*s2))
	      s2++;
	    while (*s2 == '>' || *s2 == ']' || *s2 == '}' ||
		   *s2 == '|' || *s2 == ':')
	      {
		got_some = 1;
		s2++;
		while (isspace(*s2))
		  s2++;
	      }

#ifdef DEBUG_CITE
	    if (got_some)
	      printf("CITE: %s", s);
#endif

	    if (got_some)
	      {
		s = s2;
		L = strlen(s);
	      }
	  }

	  /* Strip off anything that looks like a citation heading. */
	  {
	    const unsigned char *w1 = "wrote:";
	    const unsigned char *w2 = "writes:";
	    if (L > 10 && (strstr(s+L-10, w1) || strstr(s+L-10, w2)))
	      {
		int got_it = 0;
		/* Ends with "wrote:".  Nuke it if:
		   o  the line contains "<", or
		   o  the line begins with "In " or "On "; or
		   o  the line contains "@"; or
		   o  the line contains less than 4 spaces.
		 */
		unsigned char *s2 = s;
		while (*s2 == ' ') s2++;
		if (!strncmp(s2, "In ", 3) || !strncmp(s2, "On ", 3))
		  got_it = 1;
		else if (strchr(s2, '@'))
		  got_it = 1;
		else
		  {
		    int i = 0;
		    while (*s2)
		      if (*s2++ == ' ')
			i++;
		    if (i < 4)
		      got_it = 1;
		  }
		if (got_it)
		  {
#ifdef DEBUG_WROTE
		    printf("WROTE: %s", s);
#endif
		    L = 0;
		    *s = 0;
		  }
		else
		  {
#ifdef DEBUG_WROTE
		    printf("NOT WROTE: %s", s);
#endif
		  }
	      }
	  }


	  /* Envelope lines are definitely the start of headers. */
	  if (*s == 'F' && !strncmp (s, "From ", 5))
	    in_headers = 2;

	  /* Guess that lines beginning with dashes, or lines beginning
	     with common header fields are probably headers. */
	  else if (!in_headers &&
		   ((*s == '-' && s[1] == '-') ||
		    ((*s == 'F'||*s == 'f') && !strncmp(s,"From: ",6)) ||
		    ((*s == 'D'||*s == 'd') && !strncmp(s,"Date: ",6)) ||
		    ((*s == 'P'||*s == 'p') && !strncmp(s,"Path: ",6)) ||
		    ((*s == 'S'||*s == 's') && !strncmp(s,"Subject: ",9)) ||
		    ((*s == 'R'||*s == 'r') && !strncmp(s,"Received: ",10)) ||
		    ((*s == 'M'||*s == 'm') && !strncmp(s,"Message-ID: ",12))||
		    ((*s == 'R'||*s == 'r') && !strncmp(s,"Return-Path: ",13))
		    ))
	    in_headers = 1;

	  else if (in_headers)
	    {
	      /* Blank lines always mean end of headers.
		 Unless these headers describe a forwarded message, in which
		 case we should swallow one blank line.
	       */
	      if (*s == '\r' || *s == '\n')
		{
		  if (contains_msg)
		    contains_msg = 0;
		  else
		    in_headers = 0;
		}

	      /* If we're not totally sure we're in headers, then be
		 heuristic about end-of-headers. */
	      else if (in_headers == 1)
		{
		  /* Lines beginning with whitespace don't mean end of
		     headers. */
		  if (*s == ' ' || *s == '\t')
		    ;
		  else
		    {
		      const unsigned char *s2;
		      /* Lines that look like they begin with a header field
			 (match "^[^ \t\n]+:") don't mean end of headers. */
		      for (s2 = s; *s2 && *s2 != ':' && !isspace(*s2); s2++)
			;
		      /* But all others do. */
		      if (*s2 != ':')
			in_headers = 0;
		    }

		  if (in_headers && !contains_msg &&
		      (*s == 'C' || *s == 'c') &&
		      (!strncasecmp (s, "Content-Type: message/rfc822", 28) ||
		       !strncasecmp (s, "Content-Type: message/news", 26)))
		    contains_msg = 1;
		}
	    }

#ifdef DEBUG_HEAD
	  if (in_headers)
	    {
	      if (L > 72)
		strcpy(s+69, "...\n");
	      printf ("HEAD: %s", s);
	    }
#endif /* 0 */

	  if (in_headers)
	    {
	      prev = 0;
	      continue;
	    }


	  /* If the line is exacty 61 characters long and begins with M,
	     it might be uuencoded data.  Go look at each character and
	     see if it fits the profile.
	   */
	  if (L == 61 && *s == 'M')
	    {
	      int uue_p = 1;
	      const unsigned char *s2;
	      for (s2 = s; *s2 && *s2 != '\n' && *s2 != '\r'; s2++)
		if (*s2 < ' ' || *s2 > '`')
		  {
		    uue_p = 0;
		    break;
		  }
#ifdef DEBUG_UU
	      if (uue_p) printf("UUE: %s", s);
#endif
	      if (uue_p)
		continue;
	    }

	  /* If the line is more than 60 characters, or ends with "=", then
	     it might be base64 data.  Go look at each character and see if
	     it fits the profile.
	   */

	  if (L >= 60 || (L > 3 && s[L-1] == '='))
	    {
	      int b64_p = 1;
	      const unsigned char *s2;
	      for (s2 = s; *s2 && *s2 != '\n' && *s2 != '\r'; s2++)
		if (! ((*s2 >= 'A' && *s2 <= 'Z') ||
		       (*s2 >= 'a' && *s2 <= 'z') ||
		       (*s2 >= '0' && *s2 <= '9') ||
		       *s2 == '+' ||
		       *s2 == '/' ||
		       *s2 == '='))
		  {
		    b64_p = 0;
		    break;
		  }
#ifdef DEBUG_UU
	      if (b64_p) printf("B64: %s", s);
#endif
	      if (b64_p)
		continue;
	    }

	  /* If the line begins with the magic BinHex string, then go into
	     "binhex-skipping-mode."  (Handled at the start of the loop.)
	   */
	  if (s[0] == '(' && s[1] == 'T' &&
	      !strncmp(s, "(This file must be converted with BinHex 4.0)", 45))
	    {
	      in_binhex_p = 1;
	      continue;
	    }

	  /* Failing that, any line that is longer than 60 characters but
	     doesn't contain any spaces is fucked up in some way, so give
	     up on it.
	   */
	  if (L > 60 && !strchr (s, ' '))
	    {
#ifdef DEBUG_UU
	      printf("CRAP: %s", s);
#endif
	      continue;
	    }
	}


      /* Decode anything that looks a little bit like quoted-unreadable. */
      {
#ifdef DEBUG_QP
	int got_any = 0;
	unsigned char *o = strdup(s);
#endif
	unsigned char *s2 = s;
	while ((s2 = strchr(s2, '=')))
	  {
	    if (!isxdigit(s2[1]) || !isxdigit(s2[2]))
	      s2++;
	    else if (*s2)
	      {
		unsigned char *s3 = s2 + 1;
		const unsigned char *s4 = s2 + 3;
		s2[0] = ((((s2[1] >= '0' && s2[1] <= '9')
			   ? s2[1] - '0'
			   : ((s2[1] >= 'A' && s2[1] <= 'F')
			      ? s2[1] - ('A' - 10)
			      : s2[1] - ('a' - 10))) << 4) |
			 (((s2[2] >= '0' && s2[2] <= '9')
			   ? s2[2] - '0'
			   : ((s2[2] >= 'A' && s2[2] <= 'F')
			      ? s2[2] - ('A' - 10)
			      : s2[2] - ('a' - 10))) << 4));
		L -= 2;
		while (*s4)
		  *s3++ = *s4++;
		*s3 = 0;
#ifdef DEBUG_QP
		got_any = 1;
#endif
	      }
	  }
#ifdef DEBUG_QP
	if (got_any)
	  {
	    printf ("LINE1: %s", o);
	    printf ("LINE2: %s", s);
	  }
	free(o);
#endif
      }


      /* If the line ends with "=", then this might also be quoted-unreadable.
	 If the character before the = was alphanumeric, then a word was split.
	 Truncate the line before that word, and remember the word for next
	 time around.
       */
      if (L > 1 &&
	  s[L-1] == '=' &&
	  isalnum(s[L-2]))
	{
	  unsigned char *s3 = s+L-2;
	  s[L-1] = 0;
	  while (isalnum(*s3) && s3 > s)
	    s3--;
	  *s3 = 0;
	  qp_wrap_hack = strdup(s3+1);
	  L = strlen(s);
	}


      /* Strip out anything that looks like an HTML tag. */
      {
#ifdef DEBUG_HTML
	int got_any = 0;
	unsigned char *o = strdup(s);
#endif

	const unsigned char *last = s;
	unsigned char *s2;
	while ((s2 = strchr(last, '<')))
	  {
	    const unsigned char *s3 = s2+1;
	    int close_p = 0;

	    last = s3;

	    /* Multiple <<< in a row disqualifies it from being a tag. */
	    if (*last == '<')
	      {
		while (*last == '<')
		  last++;
		continue;
	      }

	    /* To qualify as a tag, it must match "</?[a-z]+" */
	    if (*s3 == '/')
	      close_p = 1, s3++;
	    while (isalnum(*s3))
	      s3++;

	    /* And the name must be >0 and <20 characters long. */
	    if (s3 > s2+1+close_p &&
		s3 < s2+20 &&
		(*s3 == 0 ||
		 *s3 == '>' ||
		 isspace(*s3)))
	      {
		while (*s3 && *s3 != '>')
		  s3++;
		if (!*s3)
		  {
		    last = s3;
		    inside_html_tag = 1;
		    s2[0] = 0;
		    s2[1] = 0;
#ifdef DEBUG_HTML
		    got_any = 1;
#endif
#ifdef DEBUG_HTML
		    printf(" HTML: %s", s2);
#endif
		  }
		else
		  {
		    unsigned char *out = s2;

#ifdef DEBUG_HTML
		    unsigned char b[255];
		    strncpy(b, s2, s3+1-s2);
		    b[s3+1-s2]=0;
		    printf ("HTML: %s\n", b);
#endif
#ifdef DEBUG_HTML
		    got_any = 1;
#endif

		    s3++;
		    *out++ = ' ';
		    while (*s3)
		      *out++ = *s3++;
		    *out = 0;
		    last = s2;
		  }
	      }
	    else if (s3[0] == '!' && s3[1] == '-' && s3[2] == '-')
	      {
		unsigned char *out = s2;
		in_comment_p = 1;
		s2[0] = 0;
		s2[1] = 0;
#ifdef DEBUG_HTML
		got_any = 1;
#endif
		s3 += 3;

		while (*s3 && (s3[0] != '-' || s3[1] != '-' || s3[2] != '>'))
		  s3++;
		if (*s3)
		  {
		    in_comment_p = 0;
		    s3 += 3;
		  }

		*out++ = ' ';
		while (*s3)
		  *out++ = *s3++;
		*out = 0;
		last = s2;
	      }
	  }

#ifdef DEBUG_HTML
	if (got_any)
	  {
	    printf ("LINE1: %s", o);
	    printf ("LINE2: %s", s);
	  }
	free(o);
#endif

      }


      /* Remap anything that looks like an HTML character entity. */
      {
	const unsigned char *last = s;
	unsigned char *s2;
	while ((s2 = strchr(last, '&')))
	  {
	    unsigned char *s3 = s2+1;
	    last = s3;
	    while (*s3 && *s3 != ';' && !isspace(*s3) && s3 < last+10)
	      s3++;
	    if (*s3 == ';' || isspace(*s3))
	      {
		unsigned char e = get_entity(last, s3-last);
		if (!e) continue;
		*s2++ = e;
		if (*s3) s3++;
		while (*s3)
		  *s2++ = *s3++;
		*s2 = 0;
	      }
	  }
      }

      prev = scan_line (s, table, prev);
    }
  while ((s = fgets(buf, sizeof(buf)-1, file)));

  if (qp_free_wrap_hack)
    free (qp_free_wrap_hack);

  fprintf (stderr, " %d lines\n", line_count);

  return 0;
}


static void
usage (const char *av0)
{
  char *s = strdup(version+4);
  char *s2 = strchr(s, '(');
  *s2 = '<';
  s2 = strchr(s, ')');
  *s2 = '>';
  fprintf (stderr, "%s\n", s);
  free (s);
  fprintf (stderr, "\nusage: %s [ options ] [ input-files ]\n", av0);
  fprintf (stderr, "\n\
This program analyses text files and generates markov chains of word\n\
frequencies; it can then generate random sentences based on that data.\n\
Options include:\n\
\n\
        -h or -help             this message\n\
        -o or -output <file>    file to save compiled data in (- for stdout)\n\
        -l or -load <file>      file of compiled data to load (- for stdin)\n\
        -c or -count <n>        how many sentences to generate (0 = inf)\n\
        -p or -pause <seconds>  delay between paragraphs\n\
        -html                   output HTML instead of plain-text.\n\
\n\
Remaining arguments are input files; these should be text files, but may\n\
be mail folders or HTML.  (MIME messages are also handled sensibly.)\n\
\n\
When no output file is specified, sentences will be generated from the input\n\
data directly; however, loading a saved file is far faster than re-parsing\n\
the text files each time.\n\n");
}

extern int *starters;
extern int total_starters;
extern word *all_words;
extern unsigned char **all_strings;

int
main (int argc, char **argv)
{
  int status;
  int i;
  int n_input = 0;
  int count = -1;
  int pause = 1;
  int html_p = 0;
  const char ** input = (const char **) malloc (argc * sizeof(*input));
  char *output = 0;
  char *load = 0;
  char *stat_words = 0;
  FILE *tmp_file = 0;

  for (i = 1; i < argc; i++)
    {
      const char *sw = argv[i];
      if (sw[0] == '-' && sw[1] == '-')
	sw++;

      if (!strcmp(sw, "-h") || !strcmp(argv[i], "-help"))
	{
	  usage(argv[0]);
	  exit(0);
	}
      else if (!strcmp(sw, "-o") || !strcmp(sw, "-output"))
	{
	  output = argv[++i];
	}
      else if (!strcmp(sw, "-l") || !strcmp(sw, "-load"))
	{
	  load = argv[++i];
	}
      else if (!strcmp(sw, "-c") || !strcmp(sw, "-count"))
	{
          if (i+1 >= argc)
            {
              usage(argv[0]);
              exit(1);
            }
	  count = atoi(argv[++i]);
	}
      else if (!strcmp(sw, "-p") || !strcmp(sw, "-pause"))
	{
          if (i+1 >= argc)
            {
              usage(argv[0]);
              exit(1);
            }
	  pause = atoi(argv[++i]);
	}
      else if (!strcmp(sw, "-html"))
	{
	  html_p = 1;
	}
      else if (!strcmp(sw, "-stats"))
	{
	  if (argc > i+1 && argv[i+1][0] != '-')
	    stat_words = argv[++i];
	  else
	    stat_words = strdup("");
	}
      else if (sw[0] == '-' && sw[1])
	{
	  usage(argv[0]);
	  exit(1);
	}
      else
	{
	  input[n_input++] = sw;
	}
    }

  if (n_input == 0 && !output && !load)
    {
      usage(argv[0]);
      exit(1);
    }

  if (load && n_input)
    {
      fprintf(stderr, "%s: can't load and parse files at the same time.\n",
	      argv[0]);
      usage(argv[0]);
      exit(1);
    }

  if (!output && !stat_words && count == -1)
    count = 0;

  if (n_input)
    {
      FILE *out;
      hash_table *table =
	make_hash_table (20000,
			 (long (*) (const void *)) string_case_hash,
			 (int (*) (const void *, const void *)) strcasecmp);

      for (i = 0; i < n_input; i++)
	{
	  unsigned char buf[1024];
	  unsigned char *s;
	  FILE *f;
	  if (!strcmp(input[i], "-"))
	    f = stdin;
	  else
	    {
	      f = fopen (input[i], "r");
	      if (!f)
		{
		  sprintf(buf, "%s: opening input file %s", argv[0], input[i]);
		  perror(buf);
		  exit(1);
		}
	    }

	  fprintf (stderr, "%s: reading %s...\n", argv[0],
		   (f == stdin ? "stdin" : input[i]));

	  s = fgets (buf, sizeof(buf)-1, f);
	  if (!s)
	    {
	      status = 0;  /* empty file */
	    }
	  else
	    {
	      if (!!strcmp(s, DADADODO_MAGIC))
		status = scan (f, table, buf);
	      else
		{
		  if (load)
		    {
		      fprintf(stderr,
		     "%s: can't load two saved files at once (%s and %s)\n",
			      argv[0], load, input[i]);
		      exit(-1);
		    }
		  else
		    {
		      fprintf (stderr,
			    "%s: saved files must be loaded with -load: %s\n",
			       argv[0], input[i]);
		      exit(-1);
		    }
		}
	    }

	  if (f != stdin)
	    fclose (f);

	  if (status < 0)
	    {
	      fprintf (stderr, "%s: out of memory\n", argv[0]);
	      exit(1);
	    }
	}
      free_hash_table (table);
      free (input);
      input = 0;

      if (!output)
	{
	  char *tmp = getenv("TMPDIR");
	  char *b;
	  if (!tmp) tmp = strdup("/tmp");
	  b = (char *) malloc(strlen(tmp) + 40);
	  strcpy(b, tmp);
	  if (b[strlen(b)-1] != '/')
	    strcat(b, "/");
	  sprintf(b+strlen(b), "dadadodo.%d", getpid());

	  tmp_file = fopen(b, "wb+");
	  if (!tmp_file)
	    {
	      char buf[255];
	      sprintf(buf, "%s: opening %s", argv[0], b);
	      perror(buf);
	      exit (-1);
	    }
	  unlink (b);
	  free (b);
	  out = tmp_file;
	}
      else if (!strcmp(output, "-"))
	out = stdout;
      else
	{
	  out = fopen(output, "wb");
	  if (!out)
	    {
	      char buf[255];
	      sprintf(buf, "%s: opening output file %s", argv[0], output);
	      perror(buf);
	      exit(1);
	    }
	}

      status = write_dadadodo_file (out, output);

      if (out == tmp_file)
	fflush (out);
      else if (out != stdout)
	fclose (out);

      if (status < 0)
	{
	  char buf[255];
	  sprintf(buf, "%s: writing output file %s", argv[0], output);
	  perror(buf);
	  exit(1);
	}
    }


  if (count >= 0 || stat_words)
    {
      FILE *f;

      if (load)
	{
	  f = fopen(load, "rb");
	  if (!f)
	    {
	      fprintf (stderr, "%s: can't open input file %s\n",
		       argv[0], load);
	      exit (-1);
	    }
	}
      else if (output)
	{
	  f = fopen(output, "rb");
	  if (!f)
	    {
	      fprintf (stderr, "%s: can't open output file %s\n",
		       argv[0], load);
	      exit (-1);
	    }
	}
      else
	{
	  f = tmp_file;
	  fseek (f, 0, 0);
	}

      status = read_dadadodo_file (f);
      if (status < 0)
	{
	  perror("reading file");
	  exit (-1);
	}
      fclose(f);
    }

  if (stat_words)
    {
      if (!*stat_words)
	stats (stdout);
      else if (!strcmp(stat_words, "starters"))
	{
	  int done_once = 0;
	  int *s = starters;
	  int i = 0;
	  printf("\nStarters:");
	  while (i < total_starters)
	    {
	      i += all_words[*s].start;
	      string_stats (stdout, all_strings[all_words[*s].string],
			    !done_once);
	      done_once = 1;
	      s++;
	    }
	}
      else
	{
	  unsigned char *s = strtok (stat_words, ",; ");
	  int done_once = 0;
	  do
	    {
	      string_stats (stdout, s, !done_once);
	      done_once = 1;
	    }
	  while ((s = strtok (0, ",; ")));
	}
    }

  if (count >= 0)
    {
      int column = 0;
      int words = 0;
      int n = 0;
      int indent = 0;
      int fill_column = 72;
      int sidebar_p = 0;
      int sidebar_words = 0;
      int number_p = 0;
      FILE *out = stdout;

      ya_rand_init(0);

      while (count > 0 ? n < count : 1)
	{
	  /* Break paragraph. */
	  if (words == 0 ||
	      words > 90 ||
	      sidebar_words < 0 ||
	      (column > 0 && (RAND(4)) == 0))
	    {
	      int old_indent = indent;
	      if (RAND(3) == 0)
		indent = RAND(4) * 4;

	      fill_column = 72;
	      if (indent && (RAND(2) == 0))
		fill_column -= indent;

	      if (sidebar_p)
		{
		  fputs ("</TD></TR></TABLE>\n", out);
		  sidebar_p = 0;
		}

	      if (indent == 0)
		number_p = 0;

	      if (html_p && indent != old_indent)
		{
		  int i;
		  if (indent > old_indent)
		    {
		      for (i = old_indent; i < indent; i += 4)
			{
			  switch (RAND(4)) {
			  case 0: case 1:
			    fputs ("<BLOCKQUOTE>", out);
			    break;
			  case 2:
			    fprintf (out, "<OL START=%d>", RAND(100));
			    break;
			  default:
			    fputs ("<UL>", out);
			    break;
			  }
			}
		      number_p = (RAND(5) == 0);
		    }
		  else
		    for (i = indent; i < old_indent; i += 4)
		      fputs ("</UL>", out);
		  fputs("\n", out);
		}

	      if (words > 0)
		{
		  if (html_p)
		    {
		      if (number_p && RAND(3) != 0)
			fputs ("<P ALIGN=RIGHT><LI>", out);
		      else if (RAND(10) == 0)
			fputs ("<P ALIGN=RIGHT>", out);
		      else
			fputs ("<P>", out);
		    }
		  fputs ("\n\n", out);
		  fflush (out);
		  column = 0;
		  words = 0;
		  if (pause)
		    sleep (pause);
		}

	      /* after there are more words out of the sidebar than in,
		 close then reopen any <blockquotes> to avoid a floating
		 table causing the whitespace on the right to never ever
		 be reclaimed...
	      */
	      if (sidebar_words < 0 || sidebar_words == 1)
		{
		  int i;
		  sidebar_words = 0;
		  for (i = 0; i < indent; i += 4) fputs("</UL>", out);
		  for (i = 0; i < indent; i += 4) fputs("<UL>", out);
		}

	      if (html_p &&
		  !sidebar_p &&
		  sidebar_words <= 0 &&
		  RAND(100) == 0)
		{
		  int width = 30 + RAND(30);
		  fprintf (out,
			   "<TABLE WIDTH=\"%d%%\" ALIGN=RIGHT "
			   "BORDER=4 CELLPADDING=10 CELLSPACING=0><TR><TD>\n",
			   width);
		  sidebar_p = 1;
		  sidebar_words = 100;
		}
	    }

	  {
	    int sw = random_sentence (out, &column, indent, fill_column,
				      html_p);
	    fflush (out);
	    words += sw;
	    n++;

	    if (sidebar_p)
	      sidebar_words += sw;
	    else if (sidebar_words > 1)
	      {
		sidebar_words -= sw;
		if (sidebar_words == 0)
		  sidebar_words = 1;
	      }
	  }
	}

      if (sidebar_p)
	fputs ("</TD></TR></TABLE>", out);
      if (html_p)
	fputs ("<P>\n", out);
      else
	fputs ("\n", out);
    }

  exit (0);
}
