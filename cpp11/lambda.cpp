#include <stdio.h>
#include <iostream>

/*lambda functions
Syntax:
[capture](parameters) -> return_type { function_body }

Example:
[](int x, int y) -> int { return x + y; }
*/



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


    auto add = [] (int x, int y) -> int { return x+y; };
    printf("Addition: %d+%d=%d\n", 13, 17, add(13, 17));

    //closures
    int x1 = 13;
    int x2 = 17;
    int x3 = [x1, x2] (bool doAdd) {
        printf("closure 1\n");
        int local;
        if (doAdd)
            local = x1+x2;
        else
            local = x1-x2;
        //x1 = local; //illegal, not treated as passed parameters
        //x2 = local;
        return local;
    } (false);
    printf ("closure res x3=%d\n", x3);


    //capturing by reference
    printf("before lambda with closure, x1=%d, x2=%d, x3=%d\n", x1, x2, x3);
    //everything by reference, x1 by value, x2 by reference
    int x4 = [&, x1, &x2] (bool doAdd) {
        printf("closure 1\n");
        int local;
        if (doAdd)
            local = x1+x2;
        else
            local = x1-x2;
        x2 = local;
        x3 = local;
        return local+x1;
    } (true);
    printf("closure res x4=%d\n", x4);
    printf("after lambda with closure, x1=%d, x2=%d, x3=%d\n", x1, x2, x3);
}
