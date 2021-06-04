#include <memory.h>
#include <stdio.h>
#include <stdlib.h>

int ERR_RUNTIME = 101;

long *initArray(long, long) asm("initArray");
long stringEqual(char *, char *) asm("stringEqual");
void print(char *) asm("print");
void flush() asm("flush");
char *get_char() asm("get_char");
long ord(char *) asm("ord");
char *chr(long) asm("chr");
long size(char *) asm("size");
char *substring(char *, long, long) asm("substring");
char *concat(char *, char *) asm("concat");
char *string_of_int(long) asm("string_of_int");
long not(long) asm("not");

// A Tiger string is of form
//   |  size   | chars...  |
//     8 bytes   size byes

char *mk_string(long size) {
  char *s = malloc(sizeof(long) + size * sizeof(char));
  *((long *)s) = size;
  return s;
}

char *empty_string = "\0\0\0\0\0\0\0\0";

long size_of_string(char *string) { return *((long *)string); }

char *chars_of_string(char *string) { return string + sizeof(long); }

long *initArray(long size, long init) {
  long *arr = (long *)malloc(size * sizeof(long));
  for (int i = 0; i < size; ++i) {
    arr[i] = init;
  }
  return arr;
}

long stringEqual(char *s, char *t) {
  if (s == t) return 1;

  long len_s = size_of_string(s), len_t = size_of_string(t);
  if (len_s != len_t) return 0;

  char *chs_s = chars_of_string(s), *chs_t = chars_of_string(t);
  for (int i = 0; i < len_s; ++i) {
    if (chs_s[i] != chs_t[i]) return 0;
  }
  return 1;
}

void print(char *s) {
  char *chars = chars_of_string(s);
  for (int i = 0; i < size_of_string(s); ++i) {
    putchar(chars[i]);
  }
}

void flush() { fflush(stdout); }

char *get_char() {
  int i = getc(stdin);
  if (i == EOF) {
    return empty_string;
  } else {
    char *s = mk_string(1);
    chars_of_string(s)[0] = (char)i;
    return s;
  }
}

long ord(char *s) {
  if (size_of_string(s) == 0) {
    return -1;
  } else {
    return chars_of_string(s)[0];
  }
}

char *chr(long i) {
  if (i < 0 || i >= 256) {
    fprintf(stderr, "chr(%ld) out of range\n", i);
    exit(ERR_RUNTIME);
  }
  char *s = mk_string(1);
  chars_of_string(s)[0] = (char)i;
  return s;
}

long size(char *s) { return size_of_string(s); }

char *substring(char *s, long start, long len) {
  long size = size_of_string(s);
  if (start < 0 || start + len > size) {
    fprintf(stderr, "substring([%ld], %ld, %ld) out of range\n", size, start,
            len);
    exit(ERR_RUNTIME);
  }
  char *chars = chars_of_string(s);
  char *sub = mk_string(len);
  for (int i = 0; i < len; ++i) {
    sub[i] = chars[start + i];
  }
  return sub;
}

char *concat(char *s, char *t) {
  long len_s = size_of_string(s), len_t = size_of_string(t);
  if (len_s == 0) {
    return t;
  }
  if (len_t == 0) {
    return s;
  }
  char *concat = mk_string(len_s + len_t);
  char *c = chars_of_string(concat);
  char *chs_s = chars_of_string(s);
  char *chs_t = chars_of_string(t);
  for (int i = 0; i < len_s; ++i) {
    c[i] = chs_s[i];
  }
  for (int i = 0; i < len_t; ++i) {
    c[len_s + i] = chs_t[i];
  }
  return concat;
}

char *string_of_int(long d) {
  int maxlen = 1;
  {
    long d = d;
    if (d < 0) {
      ++maxlen;
      d = -d;
    }
    while (d != 0) {
      ++maxlen;
      d /= 10;
    }
  }
  char *buf = malloc(maxlen * sizeof(char));
  long len = sprintf(buf, "%ld", d);
  char *str = mk_string(len);
  memcpy(chars_of_string(str), buf, len);
  free(buf);
  return str;
}

long not(long b) { return !b; }
