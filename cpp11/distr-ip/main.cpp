#include <iostream>
#include <vector>
#include <memory>

#include <boost/asio.hpp>


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
public:
    ConnDef(/*boost::asio::ip::tcp::socket* aSock*/) :
        sock(new boost::asio::ip::tcp::socket)
    { }

    std::shared_ptr<boost::asio::ip::tcp::socket> sock;
};

void acceptConn(std::shared_ptr<ConnDef> conn, const boost::system::error_code& error /*, WorkBatchDef */) {

}


void mainLoop() {
    //read work definition
    
    //init acceptor and spawn one worker per new connection
    boost::asio::io_service io_service;
    boost::asio::ip::tcp::acceptor acceptor(io_service,
                                            boost::asio::ip::tcp::endpoint(boost::asio::ip::tcp::v4(), PORT));

    bool stop = false;

    while (!stop) {
        std::shared_ptr<ConnDef> newConn(new ConnDef);
        acceptor.async_accept(newConn->sock,
                              boost::bind(&acceptConn, newConn, boost::asio::placeholders::error));
    }
}

int main()
{
    std::cout << "Hello World!" << "\n";
    mainLoop();
    return 0;
}
