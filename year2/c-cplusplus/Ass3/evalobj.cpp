#include <string>
#include <string.h>
using namespace std;
#include "evalobj.h"
#include <stdlib.h>

int evallist(enum op op, ExpList *elist, env *env);

int Constant::eval(env *env) {
    return n;
}

int Var::eval(env *env) {
    
    while(env != NULL)
    {    
        if(env->var == name)
        {
            return env->value;
        }
        env = env->next;
    }
    return 0;
}

int Let::eval(env *env) {
    // Create a new env pointer to point to new env
    struct env *newEnv;
    struct env env2;
    newEnv = &env2;
    
    // Add variable name into environment
    // Evaluate exp and add value into environment
    newEnv->var = bvar;
    newEnv->value = bexp->eval(env);
    newEnv->next = env;
    
    return body->eval(newEnv);
}

int OpApp::eval(env *env) {
    return evallist(op,args,env);
}

// Evalues expression lists
int evallist(enum op op, ExpList *elist, env *env)
{
    if(elist)
    {
        switch(op)
        {
            case plusop:
                return elist->head->eval(env) + evallist(op,elist->tail,env);
            case timesop:
                return elist->head->eval(env) * evallist(op,elist->tail,env);
        }
    }
    else 
    {
        if(op == plusop)
        {
            return 0;
        }
        else
        {
            return 1;
        }
    }
            
}