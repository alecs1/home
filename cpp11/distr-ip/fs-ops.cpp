//boost standard exanples
#include <iostream>
#include <iterator>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>



int main(int argc, char* argv[]) {
    boost::filesystem::path path(argv[1]);

    try {
        if (boost::filesystem::exists(path)) {
            if (boost::filesystem::is_regular_file(path)) {
                std::cout << path << " size: " << boost::filesystem::file_size(path) << "\n";
            }
            else if (boost::filesystem::is_directory(path)) {
                std::cout << path << " is dir, contains:\n";

                //std::copy
                //Copies the elements in the range, defined by [first, last), to another range beginning at d_first. The order of the elements that are not removed is preserved. 
                /*
                  //this crashes with Cygwin
                std::copy(boost::filesystem::directory_iterator(path),
                     boost::filesystem::directory_iterator(),
                     std::ostream_iterator<boost::filesystem::directory_entry>(std::cout, "\n"));
                */

                /*
                  this crashes too with Cygwin
                std::vector<boost::filesystem::path> v;
                std::copy(boost::filesystem::directory_iterator(path),
                          boost::filesystem::directory_iterator(),
                          std::back_inserter(v));
                */

                //still crashing with Cygwin
                for(auto iter = boost::filesystem::directory_iterator(path);
                    iter != boost::filesystem::directory_iterator();
                    iter++) {
                    std::cout << iter->path() << "\n";
                }
                
            }
            else {
                std::cout << path << " is neither regular file nor dir\n";
            }
        }
        else {
            std::cout << path << " does not exist\n";
        }
    }
    catch (const boost::filesystem::filesystem_error& ex) {
        std::cout << ex.what() << "\n";
    }

    std::cout << "Done\n";
    return 0;
}
