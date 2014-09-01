#include <stdio.h>
#include <string>
#include <vector>
#include <algorithm>


class AddressBook
{
public:
    template<typename Func>
    std::vector<std::string> findMatching (Func func) {
        std::vector<std::string> results;
        //std::for_each(m_addresses.begin(), m_addresses.end(), func);
        for (auto itr = m_addresses.begin(), end = m_addresses.end(); itr != end; itr++) {
            if (func(*itr)) {
                results.push_back(*itr);
            }
        }
        return results; //is RVO done here?
    }

    uint64_t add(std::string const & addr) {
        m_addresses.push_back(addr);
        return m_addresses.size(); //do we get a type warning here?
    }

    std::vector<std::string> allAddresses() const {
        return m_addresses;
    }

private:
    std::vector<std::string> m_addresses;
};

int main() {
    
    AddressBook ab;
    ab.add(std::string("Alecs <bibi@gmail.com>"));

    std::string myAddr("Eu <alex@eu.org>");
    ab.add(myAddr);

    std::string* myAddr2 = new std::string("Toteu Alex <eu@eu.org>");
    ab.add(*myAddr2);
    delete myAddr2;

    auto printString = [] (std::string const & str) { printf("%s\n", str.c_str()); };
    auto printStringList = [&printString] (std::vector<std::string> const& list) {
        std::for_each(list.begin(), list.end(), printString);
    };

    printf("All addresses:\n");
    printStringList(ab.allAddresses());
    printf("\n\n");

    //find all addresses containing "Alex"
    auto findAlex = [](std::string addr) {
        if(addr.find("Alex") != std::string::npos)
            return true;
        else
            return false;
    };

    auto alexes = ab.findMatching(findAlex);
    printf("%d addresses of Alex:\n", alexes.size());
    std::for_each(alexes.begin(), alexes.end(), 
                  [](std::string &addr){ printf("%s\n", addr.c_str());}
                  );
    printf("\n\n");

    //find all .org addresses
    auto dotOrgs = ab.findMatching(
                                   [] (std::string& addr) {
                                       if (addr.find(".org>") != std::string::npos)
                                           return true;
                                       else
                                           return false;
                                   }
                                   );

    printf("%d .org addresses:\n", dotOrgs.size());
    printStringList(dotOrgs);

    printf("\n\n");
}
