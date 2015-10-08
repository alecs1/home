#include <stdio.h>
#include <vector>
#include <string>
#include <algorithm>
#include <functional>

class MyClass {
public:
    MyClass(std::string const & aVal) :
        val(aVal) {}
public:
    std::string val;
};

//TODO - combine decltype with templates to implement printlist from any type of container

int main() {

    auto printStr = [] (std::string str) { printf("%s\n", str.c_str()); };
    auto printStringList = [&printStr] (std::vector<std::string> list) {
        std::for_each(list.begin(), list.end(), printStr);
    };
    auto extractVal = [] (MyClass obj) { return obj.val; };

    //std::function<int()> ex1 = std::bind(f1, 10, 5, 10);
    //std::function<void(int x)> ex2 = std::bind(f2, 10, 5, _1, "dfssd");
	
    //    auto applyCombined = [] (std::function<void (std::function<std::string(MyClass)>)> f1, std::function<std::string(MyClass)> f2, std::vector<MyClass> objList) {	
    auto applyCombined = [] (std::function<void(std::string)> f1, std::function<std::string(MyClass)> f2, std::vector<MyClass> objList) {
        auto fCombine = [&f1, &f2] (MyClass const &obj) { f1(f2(obj)); };
        std::for_each(objList.begin(), objList.end(), fCombine);
    };

    //how should a partial function look like?
    //    auto printExtract = [] (std::function fPrint, std::function fExtract) { fPrint(fExtract)); } (printStr, extractVal);

    std::vector<MyClass> vMC;
    vMC.push_back(MyClass("13"));
    vMC.push_back(MyClass("17"));
    vMC.push_back(MyClass("19"));

    printf("vMC:\n");
    applyCombined(printStr, extractVal, vMC);


    printf("\n\nvMC2:\n");
    auto vMC2 = vMC;
    applyCombined(printStr, extractVal, vMC2);


    printf("\n\nvMC3:\n");
    decltype(vMC) vMC3 = vMC;
    applyCombined(printStr, extractVal, vMC3);

    
}
