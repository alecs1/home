#include <stdio.h>
#include <vector>


//our heap property: parent >= children
template <typename T>
int checkHeapProperty(std::vector<T> v) {
    for(unsigned int i = 0; i < v.size(); i++) {
        if ( (2*i+1 < v.size()) && (v[2*i+1] > v[i]) )
            return i;
        if ( (2*i+2 < v.size()) && (v[2*i+2] > v[i]) )
            return i;
        //printf("v[%d]=%d, v[%d] = %d\n", i, v[i], 2*i+1, v[2*i+1]);
        //printf("v[%d]=%d, v[%d] = %d\n", i, v[i], 2*i+1, v[2*i+1]);
    }
    return -1;
}

//the vector is already a heap
template <typename T>
std::vector<T> addElement(std::vector<T> v, T el) {
    for(unsigned int i = 0; i < v.size(); i++)
        printf("%d ", v[i]);
    printf("%s - %d", __func__, el);
    printf("\n");

    unsigned int pos = v.size();
    v.push_back(el);
    unsigned int ppos = (pos-1)/2;
    while ( (pos>0) && (el > v[ppos]) ) {
        v[pos] = v[ppos];
        v[ppos] = el;
        pos = ppos;
        ppos = (pos-1)/2;
    }

    for(unsigned int i = 0; i < v.size(); i++)
        printf("%d ", v[i]);
    printf("\n");
    
    int res = checkHeapProperty(v);
    if (res != -1)
        printf("%s - error, new vector is not a heap\n", __func__);

    printf("\n\n");

    return v;
}

// a complicated two stage task
template <typename T>
std::vector<T> remElement(std::vector<T> v, unsigned int index) {

    for(unsigned int i = 0; i < v.size(); i++)
        printf("%d ", v[i]);
    printf("%s - v[%d]=%d\n", __func__, index, v[index]);

    if (index == v.size()-1) {
        v.pop_back();
        return v;
    }
    
    printf("%s - stage one - reorganise backwards\n", __func__);
    //bring the child and reorganise back
    unsigned int pos = index;
    unsigned int cpos1 = index*2 + 1;
    unsigned int cpos2 = index*2 + 2;
    unsigned int cpos = cpos1;
    while (cpos < v.size()) {
        cpos = v.size();
        //choose largest child, now cpos1 may exist without cpos2 existing
        if (v[cpos1] > v[pos])
            cpos = cpos1;
        if ( (cpos2 < v.size()) && (v[cpos2] > v[cpos1]) )
            cpos = cpos2;

        if (cpos < v.size()) {
            printf("v[%d] <- v[%d], %d <- %d\n", pos, cpos, v[pos], v[cpos]);
            v[pos] = v[cpos];
            pos = cpos;
        }
        else
            break;

        cpos1 = 2*pos+1;
        cpos2 = 2*pos+2;
        cpos = cpos1;
    }

    v[pos] = v[v.size()-1];
    T val = v[pos];
    v.pop_back();

    for(unsigned int i = 0; i < v.size(); i++)
        printf("%d ", v[i]);
    printf("\n");

    if (pos < v.size()) {
        printf("%s - stage two - reorganise forwards for v[%d]=%d\n", __func__, pos, v[pos]);


        unsigned int ppos = (pos-1)/2;
        while ( (pos>0) && (val > v[ppos]) ) {
            v[pos] = v[ppos];
            v[ppos] = val;
            pos = ppos;
            ppos = (pos-1)/2;
        }

        for(unsigned int i = 0; i < v.size(); i++)
            printf("%d ", v[i]);
        printf("\n");
    }

    int res = checkHeapProperty(v);
    if (res != -1)
        printf("%s - error, new vector is not a heap\n", __func__);

    printf("\n\n");

    return v;
}

template <typename T>
std::vector<T> heapify(std::vector<T> v) {
    for(unsigned int i = 0; i < v.size(); i++)
        printf("%d ", v[i]);
    printf(" %s ->\n", __func__);

    std::vector<T> aux;
    for(unsigned int i = 0; i < v.size(); i++)
        aux = addElement(aux, v[i]);

    for(unsigned int i = 0; i < v.size(); i++)
        printf("%d ", aux[i]);
    printf("\n");

    int res = checkHeapProperty(aux);
    if (res != -1)
        printf("%s - error, new vector is not a heap\n", __func__);

    return aux;
}

template <typename T>
std::vector<T> heapSort(std::vector<T> v) {

    for(unsigned int i = 0; i < v.size(); i++)
        printf("%d ", v[i]);
    printf(" %s ->\n", __func__);

    v = heapify(v);

    unsigned int heapSize = v.size();
    
    while(heapSize > 0) {
        T aux = v[heapSize-1];
        v[heapSize-1] = v[0];
        v[0] = aux;
        heapSize -= 1;

        //browse backwards and swap the new root element until the heap property is restored
        unsigned int pos = 0;
        unsigned int cpos1 = pos*2+1;
        unsigned int cpos2 = pos*2+2;
        while (cpos1 < heapSize) {
            unsigned int cpos = heapSize;
            if (v[cpos1] > v[pos])
                cpos = cpos1;
            if ( (cpos2 < heapSize) && (v[cpos2] > v[pos]) && (v[cpos2] > v[cpos1]) )
                cpos = cpos2;
            
            if (cpos < heapSize) {
                printf("swap v[%d]=%d - v[%d]= %d\n", pos, v[pos], cpos, v[cpos]);
                T aux = v[cpos];
                v[cpos] = v[pos];
                v[pos] = aux;
            }

            pos = cpos;
            cpos1 = pos*2+1;
            cpos2 = pos*2+2;
        }
        for (unsigned int i = 0; i < heapSize; i++)
            printf("%d ", v[i]);
        printf("\n");
    }

    for(unsigned int i = 0; i < v.size(); i++)
        printf("%d ", v[i]);
    printf(" - sorted\n");    

    return v;
}

int main(int argc, char* argv[]) {

    int array[] = {14, 45, 32, 2, -1, -13, 117, 0};
    std::vector<int> v (array, array + sizeof(array)/sizeof(array[0]));

    int res = checkHeapProperty(v);
    if (res == -1)
        printf("v respects heap property\n");
    else
        printf("v is not a heap\n");

    
    //respects heap property according to XoaX
    int heapArray[] = {81, 67, 78, 62, 64, 69, 61, 0, 58, 41, 5, 24, 45, 27, 34};
    std::vector<int> heapV (heapArray, heapArray + sizeof(heapArray)/sizeof(heapArray[0]));
    res = checkHeapProperty(heapV);
    if (res == -1)
        printf("heapV respects heap property\n");
    else
        printf("heapV is no a heap\n");

    heapV = addElement(heapV, 99);
    heapV = addElement(heapV, 63);

    std::vector<int> heapV2 = heapV;

    heapV = remElement(heapV, 7);
    heapV = remElement(heapV, 0);
    heapV = remElement(heapV, 0);
    heapV = remElement(heapV, 0);
    heapV = remElement(heapV, 0);
    heapV = remElement(heapV, 0);
    heapV = remElement(heapV, 0);
    heapV = remElement(heapV, 0);
    heapV = remElement(heapV, 0);
    heapV = remElement(heapV, 0);
    heapV = remElement(heapV, 0);
    heapV = remElement(heapV, 0);
    heapV = remElement(heapV, 0);

    std::vector<int> heapV3 = heapify(v);

    std::vector<int> sorted = heapSort(v);
    sorted = heapSort(heapV2);
    sorted = heapSort(heapV3);

    return 0;
}
