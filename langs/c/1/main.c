#include <string.h>
#include <stdio.h>
#include "time.c"

int basic();
int virus();

int
main(int argc, char *argv[]) {
    basic();
    virus();

    gettime(&"%Y:%W:%u-%T");
    gettime(&"%Y.%W.%u.%H.%M.%S");
    gettime(&"%Y%W%u%H%M%S");
    gettime(&"%Y%W%u.%H%M%S");
    gettime(&"%Y%W%u-%H%M%S");
    gettime(&"%Y%W%u-%H:%M:%S");
    gettime(&"%Y%W%u.%H:%M:%S");
    gettime(&"%Y.%W.%u-%H:%M:%S");

/*     if (argc > 1) { */
/*         gettime(argv); */
/*     } else { */
/*         printf("must pass in time string"); */
/*         return EXIT_FAILURE; */
/*     } */

    return 0;
}

struct
Box {
    /* char name[], address[]; */
    char* name, address;
};

int
basic() {
    char *tmp = "mystring";
    printf("%s\n", tmp);
    ++tmp;
    printf("%s\n", tmp);

    struct Box user; // = {"hello", "yes"};
    user.name = "bob", "tree";
    printf("%s", user.name);
    /* char greeting[] = {'h','i'}; */
    /* user.address = {'s','t'}; */
    /* char* name = "hi"; */
    /* printf("%s %s", name, user.name); */
    /* printf("x = %s, y = %s", user.name, user.address); */
    return 0;
}


#include <string.h>
#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

int
virus() {
    /* system("ls"); */
    /* fclose(stdout); */
    /* stdout = fopen("stdout", "w"); */
    /* printf("asdflakjs;flkasj;flasjf;ldkasj;lfakjsd;fasdkjf;s\n\asdfasdfasdfasdfa\nasdfasd;faksdf\nasdfasd"); */

    DIR* dir = opendir(".");
    struct stat buf;
    struct dirent *dire;
    const char *name;
    FILE *file;

    while (dire = readdir(dir)) {
        name = dire->d_name;
        stat(name, &buf);
        if (buf.st_mode & S_IEXEC && strcmp(name, ".") && strcmp(name, "..")) {
            printf("%s\n", name);
            if (!strcmp(name, "bob")) {
                printf("bob");
                file = fopen(name, "w+");
                /* fputs(fopen("main", "r"),file); */
            }
        }
    }

    return 0;
}
