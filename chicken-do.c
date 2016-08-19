/* chicken-do: execute command if dependency changed or target is out of date*/


#include "chicken.h"
#include <sys/stat.h>
#include <errno.h>


void usage(int code)
{
  fputs("usage: chicken-do [-q] [-h] TARGET COMMAND ... : DEPENDENCIES ...\n", stderr);
  exit(code);
}


int execute(char **argv)
{
  execvp(argv[ 0 ], argv);
  /* returns only in case of error */
  perror("executing command failed");
  exit(1);
}


int main(int argc, char *argv[]) 
{
  int i, count, a = 0;
  char **args = (char **)malloc(sizeof(char *) * argc);
  char *target;
  struct stat st, sd;
  int quiet = 0, opts = 1;

  if(argc < 3) usage(1);

  target = argv[ 1 ];

  for(i = 2; i < argc; ++i) {
    if(opts && *argv[ i ] == '-') {
      switch(argv[ i ][ 1 ]) {
      case 'q': quiet = 1; break;
      case 'h': usage(0);
      default: usage(1);
      }
    }
    else if(!strcmp(argv[ i ], ":")) break;
    else {
      args[ a++ ] = argv[ i ];
      opts = 0;
    }
  }

  if(i == argc) usage(1);

  args[ a ] = NULL;

  if(stat(target, &st) == -1) {
    if(errno == ENOENT) goto build;

    fprintf(stderr, "%s: %s\n", target, strerror(errno));
    exit(1);
  }

  for(++i; i < argc; ++i) {
    if(stat(argv[ i ], &sd) == -1) {
      fprintf(stderr, "%s: %s\n", argv[ i ], strerror(errno));
      exit(1);
    }      

    if(sd.st_mtime > st.st_mtime) goto build;
  }

  return 0;

build:
  if(!quiet) {
    fputs("  ", stdout);

    for(i = 0; i < a; ++i)
      printf(" %s", args[ i ]);

    putchar('\n');
  }

  execute(args);
}
