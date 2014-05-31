#include <stdio.h>
#include <string.h>
#include <stdlib.h>


char ts0[] = "abeca";
char ts1[] = "abcbec";

char ts2[] = "qxyzabcdrina";
char ts3[] = "xyzabcdcrina2";


const char alphabet[] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOQPRSTUVWXYZ0123456789$";

//not standard c, but seems to work with gcc
const int alphabetLen = strlen(alphabet);

typedef struct snode {
    char c;
    int childCount;
    struct snode **children;

    //when this is a leaf, write in which string it terminates
    int containedCount;
    int* containedBy;

} Node;

int addElement(int *(*array), int element, int* size) {
    //bool found = false;
    for(int i = 0; i < *size; i++) {
        if ((*array)[i] == element) {
            //found = true;
            return *size;
        }
    }
    *size += 1;
    *array = realloc(*array, (*size) * sizeof(int));
    (*array)[(*size)-1] = element;
    return *size;
}


Node* createNode(char c) {
    Node* node = (Node*)malloc(sizeof(Node));
    node->c = c;
    node->childCount = 0; //seems to only be useful for telling if this is a leaf
    //int alphabetLen = strlen(alphabet);
    Node** children = (Node**)malloc (sizeof(Node*) * alphabetLen);
    node->children = children;
    for(int i = 0; i < alphabetLen; i++)
        node->children[i] = NULL;
    node->containedCount = 0;
    return node;
}

//TODO - doesn't care about memory cleanup
char* longestCommonSubstring(Node* startNode, int stringIndexes[], int stringCount, char* theString) {
    char* longestString = malloc(sizeof(char) * strlen(theString)+1);
    printf("longestString=%s\n", theString);
    sprintf(longestString, "%s", theString);
    for(int i = 0; i < alphabetLen; i++) {
        int matched = 0;
        if (startNode->children[i] != NULL) {
            printf("matching for %c in:\n", startNode->children[i]->c);
            //compare all the indexes
            for (int crtIndex = 0; crtIndex < stringCount; crtIndex++) {
                for (int contained = 0; contained < startNode->children[i]->containedCount; contained++) {
                    printf("%d, ", startNode->children[i]->containedBy[contained]);
                    if (stringIndexes[crtIndex] == startNode->children[i]->containedBy[contained]) {
                        matched += 1;
                        break;
                    }
                }
            }
            printf("matched=%d\n", matched);
            if (matched == stringCount) {
                //we can go recursively, since this one contains the string
                char* crtLongest = malloc(sizeof(char) * strlen(theString) + 2);
                sprintf(crtLongest, "%s%c", theString, alphabet[i]);
                crtLongest = longestCommonSubstring(startNode->children[i], stringIndexes, stringCount, crtLongest);
                if (strlen(crtLongest) > strlen(longestString)) {
                    free(longestString);
                    longestString = crtLongest;
                }
                else
                    free(crtLongest);
            }
        }
    }
    return longestString;
}


int main() {


    char *tstr0 = malloc(sizeof(char) * strlen(ts2) + 3);
    sprintf(tstr0, "%s$1", ts2);
    char *tstr1 = malloc(sizeof(char) * strlen(ts3) + 3);
    sprintf(tstr1, "%s$2", ts3);

    char* strings[2];
    int stringsCount = 2;
    strings[0] = tstr0;
    strings[1] = tstr1;

    //this is going to be k*(n**2/2), not yet clear that the space requirement is
    Node* root = createNode('\0');
    //root.c = "\0";
    //root.childCount = 0;
    //root.containedCount = 0;
    

    for(int i = 0; i < stringsCount; i++) {
        char *crtStr = strings[i];
        int length = strlen(crtStr);
        for(int j = 0; j < length; j++) {
            //add the first char to the root
            Node* crtNode = root;
            for(int k = j; k < length; k++) {
                char crtChar = crtStr[k];
                for(int charIndex = 0; charIndex < alphabetLen; charIndex++) {
                    if (crtChar == alphabet[charIndex]) {
                        if (crtNode->children[charIndex] == NULL) {
                            crtNode->children[charIndex] = createNode(crtChar);
                            crtNode->childCount += 1;
                        }
                        printf("%c->%c  ", crtNode->c, alphabet[charIndex]);
                        crtNode = crtNode->children[charIndex];
                        crtNode->c = alphabet[charIndex];
                        addElement(&crtNode->containedBy, i, &crtNode->containedCount);
                        break;
                    }
                }
            }
            printf("\n");
            //we're at the end of the string, is there anything left to do?
        }
    }


    int stringIndexes[2];
    stringIndexes[0] = 0;
    stringIndexes[1] = 1;
    char* longestSubstring = longestCommonSubstring(root, stringIndexes, 2, "");
    printf("longest substring of %s %s: %s\n", tstr0, tstr1, longestSubstring);

    return 0;
}
