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
        printf("Resource(), id=%d, strId=%s\n", id, strId.c_str());
    }

    ~Resource() {
        printf("~Resource(), id=%d, strId=%s\n", id, strId.c_str());
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


    //weak_ptr
    printf("\n\nweak_ptr\n");

    std::shared_ptr<Resource> sP10(new Resource);
    auto sP10a = sP10;
    
    std::weak_ptr<Resource> wP10 = sP10;
    std::weak_ptr<Resource> wP10a = sP10;
    auto wP10b = wP10;

    printf("sP10=%p, ->strId=%s, .use_count=%d; wP10.use_count=%d; wP10a.use_count=%d, wP10b.use_count=%d\n",
           sP10.get(), sP10->strId.c_str(), sP10.use_count(),
           wP10.use_count(),
           wP10a.use_count(),
           wP10b.use_count());

    auto sP10b = wP10.lock();
    printf("auto sP10b = wP10.lock();\n");
    printf("sP10=%p, ->strId=%s, .use_count=%d; wP10.use_count=%d; wP10a.use_count=%d, wP10b.use_count=%d\n",
           sP10.get(), sP10->strId.c_str(), sP10.use_count(),
           wP10.use_count(),
           wP10a.use_count(),
           wP10b.use_count());

    //delete instance
    sP10b->strId = "modified via sP10b\n";
    std::shared_ptr<Resource> *aux10 = new std::shared_ptr<Resource>;
    swap(sP10a, *aux10);
    delete aux10;
    printf("swap(sP10a, aux10), delete aux10\n");
    printf("sP10=%p, ->strId=%s, .use_count=%d; sP10b.use_count=%d, wP10.use_count=%d; wP10a.use_count=%d, wP10b.use_count=%d\n",
           sP10.get(), sP10->strId.c_str(), sP10.use_count(),
           sP10b.use_count(),
           wP10.use_count(),
           wP10a.use_count(),
           wP10b.use_count());

    //delete instance
    aux10 = new std::shared_ptr<Resource>;
    swap(sP10b, *aux10);
    delete aux10;
    printf("swap(sP10b, aux10), delete aux10\n");
    printf("sP10=%p, ->strId=%s, .use_count=%d; sP10b.use_count=%d, wP10.use_count=%d; wP10a.use_count=%d, wP10b.use_count=%d\n",
           sP10.get(), sP10->strId.c_str(), sP10.use_count(),
           sP10b.use_count(),
           wP10.use_count(),
           wP10a.use_count(),
           wP10b.use_count());


    //delete last instance
    aux10 = new std::shared_ptr<Resource>;
    swap(sP10, *aux10);
    delete aux10;
    printf("swap(sP10b, aux10), delete aux10\n");
    printf("sP10=%p, .use_count=%d; sP10b.use_count=%d, wP10.use_count=%d; wP10a.use_count=%d, wP10b.use_count=%d\n",
           sP10.get(), sP10.use_count(),
           sP10b.use_count(),
           wP10.use_count(),
           wP10a.use_count(),
           wP10b.use_count());

    printf("\n\nmain()-exit\n\n");
    return 0;
}
