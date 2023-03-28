#include <stdio.h>
#include <regex.h>
#include <string.h>
#include <hiredis.h>

void
main () {
    char cur_title[] = "";

    regex title(R"(<title>(.*)</title>)");
    regex e(R"(\[\[([^#|]*?)\]\])"); // match links
    regex on_match(R"(^((?!.+:\S)|:?Category:)(.*)$)"); // find part of link we care about *1/ */
    /* regex on_match(R"(^:?(?!.+?:\S)(.*))"); // find part of link we care about *1/ */
    smatch sm1, sm2;

    redisContext *c = redisConnect("127.0.0.1", 6379);

    if (c == NULL || c->err) {
        if (c) printf("Error: %s\n", c->errstr);
        else printf("Can't allocate redis context\n");
    }

    for (string line; getline(cin, line);) {
        if (regex_search(line, sm1, title)) {
            cur_title = sm1[1];
        } else if (regex_search(line, sm1, e)) {
            /* tmp = sm1[1]; */
            /* regex_search(tmp, sm2, on_match); */
            /* if (sm2[1].length() == 0) continue; */
            /* if (cur_title < sm2[1]) redisAsyncCommand(c, NULL, NULL, "incr %s|%s", cur_title, sm2[1]); */
            /* else redisAsyncCommand(c, NULL, NULL, "incr %s|%s", sm2[1], cur_title); */
        }
    }
    }

    redisFree(c);

    return;
}


