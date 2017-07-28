// test file for exercise

#include <stdio.h>
#include <stdlib.h>

#include "freegraph.h"

struct A *mknode(struct A *ap, struct A *bp, struct A *cp, long n)
{
    struct A *p = malloc(sizeof(struct A));
    p->a = ap;
    p->b = bp;
    p->c = cp;
    p->n = n;
    return p;
}

int main(int argc, const char *argv[])
{
  struct A *p1, *p2, *p3, *p4, *p5, *p6;
    
    // construct a tree for testing: 
    
    p1 = mknode(NULL, NULL, NULL, 1);
    p2 = mknode(NULL, NULL, NULL, 10);
    p3 = mknode(NULL, NULL, NULL, 100);
    p4 = mknode(p1, p2, p3, 3000);
    p1 = mknode(NULL, NULL, NULL, 1);
    p2 = mknode(NULL, NULL, NULL, 10);
    p3 = mknode(NULL, NULL, NULL, 100);
    p5 = mknode(p1, p2, p3, 4000);
    p5 = mknode(p4, p5, NULL, 50000);
    p6 = mknode(p5, NULL, NULL, 100000);

    // to test only on trees, remove the following
    // assignments that turn the tree into a graph
    p1->a = p5;
    p2->b = p4;
    p2->c = p2;
    p6->b = p5;
    p6->c = p6; 

    if(sumA(p6) + sumA(p6) == 2 * 157222)
        printf("Yay. Test passed.\n");
    else
        printf("Grr. Test failed.\n");
    
    // valgrind should report no leaks after this:
    deallocateA(p6);

    return 0;
}



