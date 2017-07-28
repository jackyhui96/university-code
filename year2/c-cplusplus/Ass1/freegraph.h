struct A {
    struct A *a, *b, *c;
    long n;
};

long sumA(struct A *ap);

void deallocateA(struct A *ap);

