#include <memory>
#include <stdio.h>
#include <vector>

class Resource {
	static int init_id;
public:
	static std::vector<int> instance_count;
public:
	int id;
	void print() const {
		printf("Resource.id=%d, this=%p\n", id, this);
	}
	
public:
	Resource() {
		id = init_id;
		init_id += 1;
		instance_count.push_back(1);
		printf("Resource constructor, id=%d, this=%p\n", id, this);
	}
	//Resource(const Resource& r)
	~Resource() {
		printf("Resource destructor, id=%d, this=%p\n", id, this);
		instance_count[id] -= 1;
	}
};
int Resource::init_id = 0;
std::vector<int> Resource::instance_count;

__attribute__((destructor))
void AllocCheck() {
	printf("Not deleted: ");
	for (auto i = 0; i < Resource::instance_count.size(); i++) {
		if (Resource::instance_count[i] > 0)
			printf("%d, ", i);
	}
	printf("\n");
}


int main() {
	Resource* managed0 = new Resource;
	Resource* m1 = new Resource;
	Resource* unmanaged = new Resource;
	
	std::shared_ptr<Resource> sp0(managed0);
	//std::shared_ptr<auto> sp1(m1); //asta gluma nu mergeaiet
	std::shared_ptr<Resource> sp2(new Resource);
	
	//use_count and deletion at scope exit
	std::shared_ptr<Resource> sp3;
	{
		std::shared_ptr<Resource> sp4(new Resource);
		std::shared_ptr<Resource> sp5(new Resource);
		sp3 = sp4;
		printf("sp3.get()=%p, sp4.get()=%p, sp3.use_count()=%d, sp4.use_count()=%d\n",
			sp3.get(), sp4.get(), sp3.use_count(), sp4.use_count());
	}
	printf("sp3.get()=%p, sp3.use_count()=%d\n",
			sp3.get(), sp3.use_count());
	printf("use count and scope - done\n\n");
	
	//operator= behaviour
	Resource* m6 = new Resource;
	Resource* m7 = new Resource;
	std::shared_ptr<Resource> sp6(m6);
	std::shared_ptr<Resource> sp7(m7);
	sp6 = sp7; //m6 is deleted right now
	printf("operator= behaviour - done\n\n");
	
	//reset
	Resource* m8 = new Resource;
	std::shared_ptr<Resource> sp8(m8);
	sp8.reset(); //m8 is deleted right now
	printf("reset - done\n\n");
	
	//reset 2
	Resource* m9 = new Resource;
	Resource* m10 = new Resource;
	std::shared_ptr<Resource> sp9(m9);
	sp9.reset(m10); //m9 deleted here
	printf("reset 2 - done\n\n");
	
	//get
	Resource* m11 = new Resource;
	std::shared_ptr<Resource> sp11(m11);
	printf("m11=%p, *sp11.get()=%p\n", m11, sp11.get());
	printf("get - done\n\n");
	
	//swap
	Resource* m12 = new Resource;
	Resource* m13 = new Resource;
	std::shared_ptr<Resource> sp12(m12);
	std::shared_ptr<Resource> sp13(m13);
	printf("sp12: get()=%p, ->id=%d; sp13: get()=%p, ->id=%d\n",
		sp12.get(), sp12->id, sp13.get(), sp13->id);
		
	std::swap(sp12, sp13);
	printf("sp12: get()=%p, ->id=%d; sp13: get()=%p, ->id=%d\n",
		sp12.get(), sp12->id, sp13.get(), sp13->id);
		
	printf("swap - done\n\n");
	
	printf("main()-exit\n");
	return 0;
}