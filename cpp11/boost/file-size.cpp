//boost standard exanples
#include <iostream>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

int main(int argc, char* argv[]) {

    if (argc < 2) {
        std::cout << "usage program <path>\n";
        return 1;
    }

    std::cout << argv[1] << " " << boost::filesystem::file_size(argv[1]) << "\n";
    return 0;
}
