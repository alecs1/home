#include <stdio.h>
#include <string>
#include <sstream>
#include <memory>

auto printString = [] (std::string const &str) { printf("%s\n", str.c_str()); };

class Resource {
public:
    int id;
    std::string strId;

    Resource() {
        id = next_id;
        next_id += 1;
        
        std::stringstream s;
        s << id;
        strId = s.str();
        printf("Resource(), id=%s\n", strId.c_str());
    }

    ~Resource() {
        printf("~Resource(), id=%s\n", strId.c_str());
    }

private:
    static int next_id;
};

int Resource::next_id = 0;

int main() {
    Resource* unM0 = new Resource();
    Resource s1;
    Resource s2;

    Resource* m3 = new Resource();
    std::unique_ptr<Resource> uP3(m3);
    
    std::unique_ptr<Resource> uP4(new Resource());


    //std::unique_ptr<Resource> uP5 = uP4; //invalid - lvalue
    std::unique_ptr<Resource> uP5;
    uP5 = std::unique_ptr<Resource> (new Resource);

    {
        Resource* m6 = new Resource();
        Resource* unM7 = new Resource();
        std::unique_ptr<Resource> uP6(m6);
    }

    //using std::move
    printf("\n\n");
    std::unique_ptr<Resource> uP8(new Resource);
    std::unique_ptr<Resource> uP9(new Resource);
    printf("uP8=%p, uP9=%p\n", uP8.get(), uP9.get());
    printf("uP8=std::move(uP9)\n");
    uP8 = std::move(uP9); //uP8 destructed right away
    printf("uP8=%p, uP9=%p\n", uP8.get(), uP9.get());
    printf("\n\n");


}
