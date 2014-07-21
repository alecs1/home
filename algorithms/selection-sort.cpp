#include <stdio.h>

#include <vector>

std::vector<int> selSort(std::vector<int> v) {
    for(int i = 0; i < v.size()-1; i++) {
        int pos = i;
        int min = v[i];
        for(int j = i+1; j < v.size(); j++) {
            if (min > v[j]) {
                min = v[j];
                v[j] = v[pos];
                v[pos] = min;
            }
        }
    }
    return v;
}

int main(int argc, char* argv[]) {
    
    int array[] = {14, 45, 32, 2, -1, -13, 117, 0};
    std::vector<int> v(array, array + sizeof(array) / sizeof(array[0]));

    std::vector<int> result = selSort(v);

    for (int i = 0; i < v.size(); i++)
        printf("%d ", v[i]);
    printf("\n");

    for(int i = 0; i < result.size(); i++)
        printf("%d ", result[i]);
    printf("\n");
    
    return 0;

}
