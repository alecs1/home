#include <memory>
#include <vector>
#include <stdio.h>

class ResArg {
public:
    std::string name;
    int val;
public:
    ResArg(std::string aName):
        name(aName)
    {
        printf("ResArg(name=%s), this=%p\n", name.c_str(), this);
    }
    /*
    ResArg(ResArg const & other):
        name(other.name)
    {
        printf("ResArg(other=%p), name=%s\n", &other, name.c_str());
    }
    */
    ~ResArg() {
        printf("~ResArg(), name=%s, this=%p\n", name.c_str(), this);
    }
};

ResArg DefaultResArg() {
    return ResArg("DefaultResArg");
}

class Resource {
    static int init_id;
public:
    static std::vector<int> instance_count;
    std::string name;
    int id;

public:
    Resource(ResArg const & arg) {
        id = init_id;
        name = arg.name;
        init_id += 1;
        instance_count.push_back(1);
        printf("Resource(), id=%d, name=%s, this=%p\n", id, name.c_str(), this);
    }
    //Resource(const Resource& r)
    ~Resource() {
        printf("~Resource(), id=%d, name=%s, this=%p\n", id, name.c_str(), this);
        instance_count[id] -= 1;
    }
};
int Resource::init_id = 0;
std::vector<int> Resource::instance_count;

//the only special thing is: arg cannot be modified now
template <typename T, typename Arg>
std::shared_ptr<T> factory(Arg const & arg) {
    //arg.val = 5;
    return std::shared_ptr<T>(new T(arg));
}

//the special thing here is that can be both modified and constant :))
template <typename T, typename Arg>
std::shared_ptr<T> factory2(Arg&& arg) {
    //    arg.val = 5;
    return std::shared_ptr<T>(new T(std::forward<Arg>(arg)));
}

int main() {
    //on temporary
    std::shared_ptr<Resource> sp0 = factory<Resource>(ResArg("R0"));
    printf("temporary - done\n\n");


    //return by value
    std::shared_ptr<Resource> sp1 = factory<Resource>(DefaultResArg());
    printf("return by value function - done\n\n");

    //lvalue
    ResArg rA2("R2");
    std::shared_ptr<Resource> sp2 = factory<Resource>(rA2); //one ResArg copy constructor here
    printf("lvalue - done\n\n");
    
    printf("main() - done\n\n");
    return 0;
}
