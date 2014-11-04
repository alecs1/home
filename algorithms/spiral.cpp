#include <stdio.h>

//8 lines, 3 columns proves the bug I guessed about this implementation, so does 3 lines, 8 columns

#define l 3
#define c 8
int main() {
    
    int v[l][c];
    int left = 0, right = c-1, up = 0, down = l-1;
    int val = 0;
    while ( (left<=right) && (up <= down)) {
        for(int i = left; i <= right; i++) {
            v[up][i] = val;
            val += 1;
        }
        up += 1;

        for(int i = up; i <= down; i++) {
            v[i][right] = val;
            val += 1;
        }
        right -= 1;

        for(int i = right; i >= left; i--) {
            v[down][i] = val;
            val += 1;
        }
        down -= 1;

        for(int i = down; i >= up; i--) {
            v[i][left] = val;
            val += 1;
        }
        left += 1;        
    }

    for(int i = 0; i < l; i++) {
        for (int j = 0; j < c; j++) {
            printf("%4d ", v[i][j]);
        }
        printf("\n");
    }

}
