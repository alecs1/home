#include <stdio.h>
#include <vector>

std::vector<int> insSort(std::vector<int> v) {

    for(int i = 0; i < v.size() - 1; i++) {
        if (v[i] > v[i+1]) {
            int val = v[i+1];
            int pos = i;
            for(; pos >= 0; pos--)
                if (v[pos] > val)
                    v[pos+1] = v[pos];
                else
                    break;
            v[pos+1] = val;
        }
        //printf("i=%d - ", i);
        for(int k = 0; k < v.size(); k++)
            printf("%d ", v[k]);
        printf("\n");
    }

    return v;
}

int main(int argc, char* argv[]) {

    int array [] = {14, 45, 32, 2, -1, -13, 117, 0};
    std::vector<int> v (array, array + sizeof(array)/sizeof(array[0]));

    std::vector<int> res = insSort(v);

    for(int i = 0; i < v.size(); i++)
        printf("%d ", v[i]);
    printf("\n");

    for(int i = 0; i < res.size(); i++)
        printf("%d ", res[i]);
    printf("\n");
}
