#include <stdio.h>

#include <boost/bind.hpp>

void print3(int a, int b, int c) {
    printf("a=%d, b=%d, c=%d\n", a, b, c);
}

int plus2(int a, int b) {
    return a + b;
}

int plus3(int a, int b, int c) {
    return a + b + c;
}

int main() {
    auto b1 = boost::bind(plus2, 7, 6);
    auto retVal = b1();
    printf("retVal=%d\n", retVal);

    
    auto b2 = boost::bind(print3, 1, 2, 3);
    b2();

    auto b3 = boost::bind(print3, _1, _2, _3);
    b3(5, 6, 7);

    auto b4 = boost::bind(print3, _3, _2, _1);
    b4(5, 6, 7);

    auto b5 = boost::bind(print3, _1, 2, 3);
    b5(13);

    auto b6 = boost::bind(print3, 1, 2, _1);
    b6(17);

    auto b7 = boost::bind(print3, 1, 2, _2);
    b7(17, 19); //will use 19, not 17

    auto b8 = boost::bind(print3, 1, 2, _4);
    b8(19, 23, 29, 31, 37); //works and will use 31, the fourth argument

    auto b9 = boost::bind(print3, 1, _2, _2);
    b9(5, 7, 7, 7, 7);

    return 0;
}
