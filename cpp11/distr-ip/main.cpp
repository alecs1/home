#include <iostream>

//using namespace std;

int main()
{
    cout << "Hello World!" << endl;
    return 0;
}

enum class OpDef {
    BW,
    Sharpen,
    Smoothen
};

struct TaskDef {
    std::string filePath;
    OpDef op;
    bool done; //done when the output file is confirmed to be written
};

struct WorkBatchDef {
    int rangeStart;
    int rangeEnd;
    std::vector
};
