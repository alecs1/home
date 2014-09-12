//#include <map>
#include <string>
#include <stdio.h>


int& IncrPre(int& val) {
	return ++val;
}

/*
int& IncrPost(int& val) {
	return val++;
	//error: invalid initialization of non-const reference of type ‘int&’ from an rvalue of type ‘int’
}
*/

int IncrPost2(int& val) {
	return val++;
}

//RVO tests
class RVOTest {
public:
	static int count;
public:
	RVOTest() {
		printf("RVOTest constructor\n");
	}
	RVOTest(const RVOTest& rv) {
		count += 1;
		printf("RVOTest copy constructor\n");
	}
	~RVOTest() {
		printf("RVOTest destructor\n");
	}
};
int RVOTest::count = 0;

RVOTest FRVOTest() {
	return RVOTest();
}

class Resource {
	static int init_id;
public:
	int id;
	void print() const {
		printf("Resource.id=%d, this=%p\n", id, this);
	}
	
public:
	Resource() {
		id = init_id;
		init_id += 1;
		printf("Resource constructor, id=%d, this=%p\n", id, this);
	}
	//Resource(const Resource& r)
	~Resource() {
		printf("Resource destructor, id=%d, this=%p\n", id, this);
	}
};
int Resource::init_id = 0;

class RVOTestComplex {
public:
	static int count;
public:
	//Resource m_Resource;
	Resource* m_pResource;
public:
	RVOTestComplex() {
		m_pResource = new Resource();
		printf("RVOTestComplex constructor, this=%p, m_pResource->id=%d, m_pResource=%p\n",
			this, m_pResource->id, m_pResource);
	}
	RVOTestComplex(const RVOTestComplex& rv)
	{
		m_pResource = new Resource(*rv.m_pResource);
		count += 1;
		printf("RVOTestComplex copy constructor, this=%p, rv=%p, m_pResource->id=%d, m_pResource=%p, rv.m_pResource=%p\n",
			this, &rv, m_pResource->id, m_pResource, rv.m_pResource);
	}
	RVOTestComplex& operator=(RVOTestComplex const & rv) {
		printf("RVOTestComplex operator=, this=%p, rv=%p, m_pResource->id=%d, m_pResource=%p, rv.m_pResource=%p\n",
			this, &rv, m_pResource->id, m_pResource, rv.m_pResource);
		delete m_pResource;
		m_pResource = new Resource(*rv.m_pResource);
		printf("RVOTestComplex operator=, this=%p, new m_pResource=%p\n",
			this, m_pResource);
		return *this;
	}
	~RVOTestComplex() {
		printf("RVOTestComplex destructor, this=%p, m_pResource->id=%d, m_pResource=%p\n",
			this, m_pResource->id, m_pResource);
		delete m_pResource;
	}
};
int RVOTestComplex::count = 0;

RVOTestComplex FRVOTestComplex() {
	printf("FRVOTestComplex-start\n");
	RVOTestComplex retVal = RVOTestComplex(); //copy constructor possibility at this point, RVOTestComplex to retVal
	printf("FRVOTestComplex, retVal.m_pResource->id=%d, retVal.m_pResource=%p\n", retVal.m_pResource->id, retVal.m_pResource);
	printf("FRVOTestComplex-end\n");
	return retVal; //copy constructor possibility at this point, from retVal (local variable) to the return value of this function
}

//move semantics tests
class MvSemTest {


};

int main() {

{
	//lvalue, rvalue
	int y = 13;
	int y1 = IncrPre(y);
	printf("y=%d, y1=%d\n", y, y1);
	
	int y2 = IncrPost2(y);
	printf("y=%d, y2=%d\n", y, y2);
	
	IncrPre(y) = -1;
	printf("y=%d\n", y);
	
	//IncrPost2(y) = -2;
	//printf("y=%d\n", y);
	
	int y3 = ++y;
	printf("y=%d, y3=%d\n", y, y3);
	
	int y4 = y++;
	printf("y=%d, y4=%d\n", y, y4);
	
	++y = y+15;
	printf("y=%d\n", y);
}
	
	
	//RVO (return value optimisation)
	printf("\n\n\nRVO tests:\n");
{
	RVOTest t;
	t = FRVOTest();
	if (t.count == 0) {
		printf("RVO 100%%\n");
	}
	else if (t.count == 1) {
		printf("RVO 50%%\n");
	}
	else {
		printf("No RVO performed\n");
	}
}	
	
	printf("\n\n");
	RVOTestComplex t2;
	printf("t2: this=%p\n", &t2);
	t2 = FRVOTestComplex();
	if (t2.count == 0) {
		printf("RVO 100%%\n");
	}
	else if (t2.count == 1) {
		printf("RVO 50%%\n");
	}
	else {
		printf("No RVO performed\n");
	}
	printf("t2.m_pResource->id=%d, t2.m_pResourc%p\n", t2.m_pResource->id, t2.m_pResource);
	
	//move semantics:
	printf("\n\n\nMove semantics tests\n");

	return 0;
}
