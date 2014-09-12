#include <map>
#include <string>
#include <stdio.h>

int main() {
	std::map<std::string, std::map<std::string, std::string> > aMap;

	aMap["something"]["key1"] = "val1";
	aMap["something"]["key2"] = "val2";
	
	auto sMap = aMap["something"];
	sMap["key3"] = "val3";
	
	printf("%s - %s - %s\n", sMap["key1"].c_str(), sMap["key2"].c_str(), sMap["key3"].c_str());
	return 0;
}
