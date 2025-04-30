#include <string>
using namespace std;

template<class First, class Second>
struct Product {
    First fst = NULL;
    Second snd = NULL;
};

template<class T> using Ref = T (T);
template<class T> T mempty() = delete;
template<class T> T mappend(T, T) = delete;
template<class M>
concept Monoid = requires (M m) {
    { mempty<M>() } -> M;
    { mappend(m, m) } -> M;
};

template<> string mempty<string>() { return ""; };
template<> string mappend<string>(string a, string b) { return a + b; };


int ct() {
    return 0;
}

int main () {
    return 0;
}
