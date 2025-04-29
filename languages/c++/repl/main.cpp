#include "ct.cpp"
#include "main.h"
#include <string>
#include <iostream>
#include <cmath>
#include <sstream>
#include <array>
#include <vector>
#include <exception>
#include <fstream>

using std::cout;
using std::string;
using std::endl;
using std::cin;
using std::ios;

// https://sites.ualberta.ca/~kbeach/comp_phys/cpp_review.html
// http://www.cplusplus.com/doc/tutorial/classes2/
// https://en.cppreference.com/w/cpp/header

/* Todos
- lambda
- concepts
*/

struct Rectangle  {
    private:
        int x,y;
    public:
        Rectangle(int a, int b) : x(a), y(b) {};
        Rectangle operator+ (const Rectangle&);
};

Rectangle Rectangle::operator+(const Rectangle &rect) {
    return Rectangle(x + rect.x, y + rect.y);
};

class Person {
    private:
        string name;
    public:
        Person(string n);
        ~Person();
};

Person::Person(string n) : name(n) {};
Person::~Person() {};

namespace types {
    struct names_t {
        string name;
        string address;
    } family[3];
}

template<class Showable> void show(Showable x) { cout << x << endl; }
template<class Aaa=int, class Bbb, int N=0> void show(Aaa x, Bbb b) { cout << N << endl; }

void
doIO (char *argv[]={}) {
    cout << argv[0] << endl;
    string a, b;
    cin >> a >> b;
    int x;
    std::stringstream(a) >> x;
    cout << a << b << x;

    { using namespace types;
        for (int i = 0; i <= 2; i++) getline(cin, family[i].name);
        for (int i = 0; i <= 2; i++) cout << family[i].name; cout << endl;
    }

    string line;
    getline(cin, line);
    for (auto c : line) { printf("[%c]", c); }

    string input = "helo there, COLOR", output;
    getline(std::stringstream(input), output);
    cout << output;

}

void
memoize (int &ret, int f (int)) { ret = f(5); }

void
pointers (int xs[]) {
    int a = 0; address(a);

    int **y = &xs;
    int *z = xs;
    printf("%d=%d=%d\n", *(xs + 1), *(*y + 1), xs[1]);
    cout << &xs[0] << endl << *y << endl << z << endl;
}

void
address(int &add) { add = 1; }

void
doMath() {
    cout << sin2(3.14, 10) << endl;
}

int
fac (int x) {
    return x == 0 ? 1 : x * fac(x - 1);
}

double
sin2(double x, int maxorder) {
    double sum = 0.0, term = x;
    for (int n = 1; n <= maxorder; sum += term, n += 2, term *= -x*x/(n*(n-1)));
    return sum;
}

void
memory() {
    char *ptr;
    ptr = (char *) malloc(8 * sizeof(char));
    free(ptr);
    ptr = new char[8];
    delete ptr;
}

void
data_types() {
    union Product {
        int32_t p_one;
        uint16_t p_two[2];
    } product;
    enum Days { mon, tue, wed, thu, fri, sat, sun } this_week;
    /* enum class Days : char { mon, tue, wed, thu, fri, sat, sun } this_week; */
    /* enum class Colors { white, blue, green, yellow, red } colors; // Colors::white etc. */

    typedef char Char1;
    using Char2 = char;

    std::vector<int> ages = {0,1,2,3};
    cout << ages.size() << " - " << ages.capacity();
    ages.push_back(4);
    cout << ages[0] << endl;;
}

void
exceptions() {
    try {
        int *myarray = new int[10000000000];
    } catch (std::exception e) {
        cout << "err";
    }
}

void
files() {
    std::ifstream myfile ("test.txt", ios::in | ios::out);
    string line;
    getline(myfile, line);
    cout << line << endl;;
    /* myfile.open("test.txt", out | ate); */
    /* myfile << "hi" << "bye"; */
    myfile.close();
}

/* void */
/* preprocess() { */
/* #define COLOR "red" */
/* #line 20 "testing" */
/*     cout << COLOR << err; */
/* #undef COLOR */
/* } */

int
main (int argc, char *argv[]) {
    /* int xs[5] = {1,2,3}; */
    /* pointers(xs); */
    /* int ret = 0; memoize(ret, [](int x) {return x*x;}); */
    ct();

    return 0;
}
