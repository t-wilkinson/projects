#include <time.h>
#include <stdio.h>
#include <stdlib.h>

int
gettime(char (*argv)[]) {

    char outstr[200];
    time_t t;
    struct tm *tmp;

    t = time(NULL);
    tmp = localtime(&t);
    if (tmp == NULL) {
        perror("localtime");
        exit(EXIT_FAILURE);
    }

    if (strftime(outstr, sizeof(outstr), *argv, tmp) == 0) {
        fprintf(stderr, "strftime returned 0");
        exit(EXIT_FAILURE);
    }

    printf("\"%s\"\n", outstr);
    return EXIT_SUCCESS;

}
