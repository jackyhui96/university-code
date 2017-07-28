#include <string>
#include <iostream>

using namespace std;

#include "evalobj.h"

int main(int argc, const char *argv[])
{
  ExpList *l = nullptr;
  //l = new ExpList(new Constant(23), l);
  //l = new ExpList(new Constant(42), l);
  //l = new ExpList(new Constant(666), l);

  //l = new ExpList(new Constant(5), l);
  //l = new ExpList(new Constant(4), l);
  //l = new ExpList(new Constant(8), l);
  
 ExpList *l2 = nullptr;
 l2 = new ExpList(new Var("x"), l2);
 l2 = new ExpList(new Var("y"), l2);
 l2 = new ExpList(new Var("z"), l2);
 Exp *e5 = new OpApp(timesop, l2);
 
 ExpList *l3 = nullptr;
 l3 = new ExpList(new Constant(2), l3);
 l3 = new ExpList(new Constant(3), l3);
 l3 = new ExpList(new Constant(5), l3);
 Exp *e4 = new OpApp(plusop, l3);
 Exp *e3 = new Let("x",e4, e5);
 Exp *e2 = new Let("y",new Constant(5), e3);
 Exp *e1 = new Let("z",new Constant(40), e2);
 
 cout << e1->eval(nullptr) << endl;//should print 2000
  
  //Exp *e = new OpApp(plusop, l);
  //Exp *e = new OpApp(timesop, l);
  //cout << e->eval(nullptr) << endl;
}


