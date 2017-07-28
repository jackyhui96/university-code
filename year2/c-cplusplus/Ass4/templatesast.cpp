#include <string>
#include <string.h>
using namespace std;

#include <stdlib.h>

template<typename V>
V Constant<V>::eval(env<V> *env) {
    return c;
}

template<typename V>
V Var<V>::eval(env<V> *env) {
    
    while(env != NULL)
    {    
        if(env->var == name)
        {
            return env->value;
        }
        env = env->next;
    }
    return eval(env);
}

template<typename V>
V Let<V>::eval(env<V> *envA) {
    // Create a new env pointer
    env<V> *newEnv = new env<V>;
    
    // Add variable name into environment
    // Evaluate exp and add value into environment
    newEnv->var = bvar;
    newEnv->value = bexp->eval(envA);
    newEnv->next = envA;
    
    return body->eval(newEnv);
}

template<typename V>
V OpApp<V>::eval(env<V> *env) {
    return evallist(ops,args,env);
}

// Evaluates expression lists
template<typename V>
V evallist(operators<V> ops, ExpList<V> *elist, env<V> *env)
{
    if(elist)
    {
        
		return ops.binop(elist->head->eval(env), evallist(ops,elist->tail,env));
    }
    else 
    {
        return ops.unit;
    }     
}