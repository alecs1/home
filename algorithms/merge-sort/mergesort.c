#include <stdio.h>
#include <stdlib.h>

void merge(int* destArray, int* subArray1, int size1, int* subArray2, int size2);
int* mergeSort(int* array, int size);

int main(int argc, char* argv) {
    int arrayToSort[] = { 1, 0, -1, 10, 9, 8, 6, 7, 2, -20 };
    int arraySize = sizeof(arrayToSort)/sizeof(int);
    int* sortedArray = mergeSort(arrayToSort, arraySize);
    printf("\n Sorted array:\n");
    printf("sizeof(arrayToSort) = %d\n", arraySize);
    for(int i = 0; i < arraySize; i++)
        printf("%d ", sortedArray[i]);
    printf("\n\n");
    free(sortedArray);
    return 0;
}

//mergesort on integers
int* mergeSort(int* array, int size) {
    printf ("mergeSort, array of size=%d\n", size);
    for(int i = 0; i < size; i++)
        printf("%d ", array[i]);
    printf("\n");
    int *retArray = malloc(size*sizeof(int));

    if (size == 0)
        return NULL;
    if (size == 1) {
        retArray[0] = array[0];
    }
    else {
        int* subArray1 = mergeSort(array, size/2);
        int* subArray2 = mergeSort(array + size/2, size - size/2);
	merge(retArray, subArray1, size/2, subArray2, size - size/2);
	free(subArray1);
	free(subArray2);
    }

    return retArray;      
}

void merge(int* destArray, int* subArray1, int size1, int* subArray2, int size2) {
    printf("Merge, arrays of size %d and %d\n", size1, size2);
    for(int i = 0; i < size1; i++)
        printf("%d ", subArray1[i]);
    printf("\n");
    for(int i = 0; i < size2; i++)
        printf("%d ", subArray2[i]);
    printf("\n");
    int c1 = 0;
    int c2 = 0;
    int c = 0;
    while (c1 < size1 && c2 < size2) {
        if (subArray1[c1] <= subArray2[c2]) {
            destArray[c] = subArray1[c1];
            c1+=1;
        }
        else {
            destArray[c] = subArray2[c2];
            c2+=1;
        }
        c+=1;
    }
    while (c1 < size1) {
        destArray[c] = subArray1[c1];
        c+=1;
        c1+=1;
    }
    while (c2 < size2) {
        destArray[c] = subArray2[c2];
        c+=1;
        c2+=1;
    }
    for(int i = 0; i < c; i++)
        printf("%d ", destArray[i]);
    printf("\n\n");
}

