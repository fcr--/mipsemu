void out(int data) {
    *(int*)0x10000000 = data;
}

int sum(int a, int b) {
    return a+b;
}

void __start(void) {
    out(sum(42, 10));  /*==0x34*/
    asm volatile(
        "li $4, 42;"     /*exit 42*/
        "li $2, 4246;"  /*__NR_exit*/
        "syscall;"
    );
}
