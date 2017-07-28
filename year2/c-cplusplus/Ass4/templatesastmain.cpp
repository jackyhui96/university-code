#include <iostream>
#include <functional>

using namespace std;

#include "templatesast.h" 

#include "templatesast.cpp" // this is where the eval functions should be

// instantiate the AST template and try it

int add(int x, int y) { return x + y; }

float mult(float x, float y) { return x * y; }

int main(int argc, const char *argv[])
{
	/*
  operators<int> intops = { add, 0 };
    
  ExpList<int> *l = nullptr;
  l = new ExpList<int>(new Constant<int>(30), l);
  l = new ExpList<int>(new Constant<int>(12), l);
  Exp<int> *e = new OpApp<int>(intops, l);
  cout << e->eval(nullptr) << endl; // should print 0 + 12 + 30 = 42

  Exp<int> *e1 = new OpApp<int>(intops, nullptr);
  cout << e1->eval(nullptr) << endl; // should print 0
    
  operators<float> floatops = { mult, 1.0 };
    
  ExpList<float> *l2 = nullptr;
  l2 = new ExpList<float>(new Constant<float>(.222), l2);
  l2 = new ExpList<float>(new Constant<float>(3.0), l2);
  Exp<float> *e2 = new OpApp<float>(floatops, l2);
  cout << e2->eval(nullptr) << endl; // should print 0.666 = 1.0 * 3.0 * .222
  */
 
 operators<int> intops = { add, 0 };
 operators<int> intops2 = {mult, 1 };
 ExpList<int> *l = nullptr; 
 ExpList<int> *l2 = nullptr;
 
 l2 = new ExpList<int>(new Var<int>("x"), l2);
 l2 = new ExpList<int>(new Var<int>("y"), l2);
 l2 = new ExpList<int>(new Var<int>("z"), l2);
 Exp<int> *e5 = new OpApp<int>(intops2, l2);
 
 ExpList<int> *l3 = nullptr;
 l3 = new ExpList<int>(new Constant<int>(2), l3);
 l3 = new ExpList<int>(new Constant<int>(3), l3);
 l3 = new ExpList<int>(new Constant<int>(5), l3);
 Exp<int> *e4 = new OpApp<int>(intops, l3);
 Exp<int> *e3 = new Let<int>("x",e4, e5);
 Exp<int> *e2 = new Let<int>("y",new Constant<int>(5), e3);
 Exp<int> *e1 = new Let<int>("z",new Constant<int>(40), e2);
 
 cout << e1->eval(nullptr) << endl;//should print 2000
}
