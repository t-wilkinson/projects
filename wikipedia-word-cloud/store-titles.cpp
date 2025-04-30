#include <iostream>
#include <regex>
#include <string>
#include <sstream>
#include <redox.hpp>

using namespace std;

const char SEP = '|';

int
main(int argc, char* argv[]) {
    string cur_title = "";
    string tmp;
    ostringstream os;

    regex title(R"(<title>(.*)</title>)");
    regex e(R"(\[\[([^#|]*?)\]\])"); // match links
    regex on_match(R"(^:?(?!.+:\S)(?:Category:)(.*)$)"); // find part of link we care about *1/ */
    /* regex on_match(R"(^:?(?!.+?:\S)(.*))"); // find part of link we care about *1/ */
    smatch sm1, sm2;

    redox::Redox rdx;
    if (!rdx.connect("localhost", 6379)) return 1;

    for (string line; getline(cin, line);) {
        if (regex_search(line, sm1, title)) {
            cur_title = sm1[1];

        /* Find link page name and add it to database */
        } else if (regex_search(line, sm1, e)) {
            tmp = sm1[1];
            regex_search(tmp, sm2, on_match);
            if (sm2[1].length() == 0) continue;
            // key must be commutative
            if (cur_title < sm2[1]) os << cur_title << SEP << sm2[1];
            else os << sm2[1] << SEP << cur_title;

            // Update cmd with key and run it
            rdx.command({"incr", os.str()});
            os.str("");
        }
    }

    rdx.disconnect();

    return 0;
}
