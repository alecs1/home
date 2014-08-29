#include <stdio.h>

int main() {

    auto func = [] () { printf("inside anonymous func\n"); };
    func();

    auto func2 = [] () {  printf("inside anonymous func2\n"); };
    func2();
    func2();
}
