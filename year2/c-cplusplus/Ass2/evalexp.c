#include "evalexp.h"
#include <stdlib.h>
#include <string.h>

int evalexp(struct exp *e);
int evallist(enum op op, struct explist *list);
int val;
struct explist *elist;

int evalexp(struct exp *e)
{
    switch(e->tag)
    {
        case isconstant:
            return e->constant;
        case isopapp:
            return evallist(e->op, e->exps);
        case islet:
            return 0;
        case isvar:
            return 0;
    }
}

int evallist(enum op op, struct explist *list)
{
    if(list != NULL)
    {
        switch(op)
        {
            case isplus:
                return evalexp(list->head) + evallist(op, list->tail);
            case ismult:
                return evalexp(list->head) * evallist(op, list->tail);
        }
    }
    else
    {
        return 0;
    }
}

