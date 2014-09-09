#include <iostream>
#include <vector>

//using namespace std;

int PORT = 8123;



//no kernel since that requires serialisation
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
    std::vector<TaskDef> &work;
};

struct ConnDef {
    boost::asio::op::tcp::socket sock;
};


void mainLoop() {
    //read work definition
    
    //init acceptor and spawn one worker per new connection
    boost::asio::io_service io_service;
    boost::asio::ip::tcp::acceptor acceptor(io_service,
                                            boost::asio::ip::tcp::endpoint(boost::asio::ip::tcp::v4(), PORT));

    bool stop = false;

    while (!stop) {
        
    }
}

int main()
{
    std::cout << "Hello World!" << "\n";
    mainLoop();
    return 0;
}
