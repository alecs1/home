#include <stdio.h>
#include <iostream>

auto LCall() {
    return [] () { printf("lambda from LCall\n"); };
}

auto LCall2() {
    [] () { printf("lambda from LCall2\n"); }; //does nothing but define that lambda function, does not call or return it in any way
}

auto LCall3() {
    [] () { printf("lambda from LCall3\n"); }();
}

auto LCall4() {
    [] () { printf("lambda from LCall4, ret=13\n"); return 13; } (); //function call with no effect
}

auto LCall5() {
    return [] () { printf("lambda from LCall4, ret=13\n"); return 13; }();  //returns the value from the function call
}

int main() {

    auto func = [] () { printf("inside anonymous func\n"); };
    func();

    auto func2 = [] () {  printf("inside anonymous func2\n"); };
    func2();
    func2();

    [] () { printf("inside anonymous - 3\n"); }();

    auto func3 = LCall();
    printf("func3: ");
    func3();
    
    printf("LCall(): ");
    LCall();
    printf("\nLCall() - 2: ");
    LCall()();

    printf("\nLCall2(): ");
    LCall2();

    printf("\nLCall3(): ");
    LCall3();

    //auto retVal4 = LCall4();  //compile error-incomplete type
    //std::cout << "LCall4 ret=" << retVal4;

    auto retVal5 = LCall5();
    std::cout << "LCall5 ret=" << retVal5;  
    
}
