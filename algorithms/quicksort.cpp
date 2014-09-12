#include <stdio.h>
#include <stdlib.h>

template <typename T>
int quicksort(T array[], int first, int last) {

    if (last-first < 1)
        return 0;


    //choose the pivot, then split
    T m, p, p1, p2, p3;
    int piv;
    p1 = array[first];
    p2 = array[(first+last)/2];
    p3 = array[last];
    m = (p1 + p2 + p3) / 3;
    p = p1;
    piv = first;
    if ( abs(m-p2) < abs(m-p) ) {
        piv = (first+last)/2;
        p = p2;
    }
    if ( abs(m-p3) < abs(m-p) ) {
        piv = last;
        p = p3;
    }

    array[piv] = array[last];
    array[last] = p;

    int lindex = first;
    for(int i = first; i <= last-1; i++) {
        if (array[i] < p) {
            T aux = array[i];
            array[i] = array[lindex];
            array[lindex] = aux;
            lindex += 1;
        }
    }
    
    array[last] = array[lindex];
    array[lindex] = piv;

    quicksort(array, first, lindex-1);
    quicksort(array, lindex+1, last);

    return 0;
}

int main() {
    int arr[] = {19, 0, -12, 4, 4, 4, 6, 20, -13};
    int size = sizeof(arr) / sizeof(arr[0]);
    
    quicksort(arr, 0, size-1);

    for(int i = 0; i < size; i++)
        printf("%d ", arr[i]);
    printf("\n");


}
