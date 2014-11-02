#include <vector>
#include <iostream>

// using namespace std;


//linking to Qt for ease of use of file classes, rewrite using std
#include <QFile>
#include <QTextStream>

// causes a deep copy
std::vector<double> bubbleSort(std::vector<double> list) {
  bool sorted = false;
  while (! sorted) {
    sorted = true;
    for(int i = 0; i < list.size() - 1; i++)
      if (list.at(i) > list.at(i+1)) {
        sorted = false;
        double auxNumber = list.at(i);
        list.at(i) = list.at(i+1);
        list.at(i+1) = auxNumber;
      }
  }
  return list;
}


//argument 1: name of a file containing a list of unsorted numbers
int main(int argc, char* argv[]) {
    if (argc != 2) {
      std::cout << "Program takes exactly one 1 argument: fileName" << endl;
      return 1;
    }

    QFile file(argv[1]);

    if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
      return 1;

    std::vector<double> list;
    QTextStream inStream(&file);
    double auxNumber;
    while (!inStream.atEnd()) {
      inStream >> auxNumber;
//       std::cout << "new auxNumber: " << auxNumber << std::endl;
      list.push_back(auxNumber);
    }

    std::vector<double> sorted = bubbleSort(list);

    printf("%g", sorted.at(0));
    for (int i = 1; i < sorted.size(); i++)
      printf(", %g", sorted.at(i));
    printf("\n");

    return 0;
}