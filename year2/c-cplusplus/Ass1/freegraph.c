#include "freegraph.h"
#include <stdlib.h>

struct Llist
{
    struct Llist *next;
    struct A *p;    
};

long sumOfA(struct Llist *list);
struct Llist *insert(struct A *p, struct Llist *list);
struct Llist *insertA(struct A *p, struct Llist *list);
int isIn(struct A *p, struct Llist *list);
void deallocateA(struct A *p);

void deallocateA(struct A *p)
{
    
}

long sumA(struct A *p)
{
    
    struct Llist *a = malloc(sizeof(struct Llist));
    a->next = NULL;
    a->p = NULL;
    a = insertA(p,a);
    return sumOfA(a);
}

long sumOfA(struct Llist *list)
{
    long total = 0;
    while(list->next)
    {    
        if(list->p)
        {
            total = total + list->p->n;
        }
        list = list->next; 
    }
    return total;
}

struct Llist *insert(struct A *p, struct Llist *list)
{
    struct Llist *q = malloc(sizeof(struct Llist));
    q -> next = list;
    q -> p = p;
    return q;
}

struct Llist *insertA(struct A *p, struct Llist *list)
{
    if(!p || isIn(p,list))
    {
        return list;
    } 
    else
    {
        list = insert(p,list);
        struct Llist *first = insertA(p->a,list);
        struct Llist *second = insertA(p->b,first);
        struct Llist *third = insertA(p->c,second);
        
        return third;
    }
}

int isIn(struct A *p, struct Llist *list)
{
    while(list->next)
    {
        if(p == list->p)
        {
            return 1;
        }
        else
        {
           return isIn(p,list->next);
        }
    }
    return 0;
}

