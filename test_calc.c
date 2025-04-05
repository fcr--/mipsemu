#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define MAX_STACK_SIZE 1024

void error(int status, const char *format, ...) {
    va_list ap;
    va_start(ap, format);
    fflush(stdout);
    vfprintf(stderr, format, ap);
    exit(status);
}

struct {
    int data[MAX_STACK_SIZE];
    int size;
} stack = {.size=0};

void push(int n) {
    stack.data[stack.size++] = n;
    if (stack.size > MAX_STACK_SIZE)
        error(1, "stack overflow\n");
}
int pop() {
    if (stack.size <= 0)
        error(1, "stack underflow\n");

    return stack.data[--stack.size];
}

int main(int argc, char * argv[]) {
    //printf("argc=%d\n", argc);
    if (argc < 2 || !strcmp(argv[1], "--help")) {
        error(
            1,
            "Usage %s <token> <token> <token> ...\n"
            "<token> can be an integer number or an operation, if it's an integer\n"
            "it's going to be pushed in the stack, otherwise operations are:\n"
            "\n"
            "int1 int2 <add> int:  adds the last two numbers\n"
            "int1 int2 <sub> int:  computes int1 - int2\n",
            *argv
        );
    }
    for (int i = 1; i < argc; i++) {
        char * endptr;
        char * arg = argv[i];
        int n = strtol(arg, &endptr, 0);
        if (*arg && !*endptr) {
            push(n);
        } else if (!strcmp(arg, "add")) {
            push(pop() + pop());
        } else if (!strcmp(arg, "dup")) {
            int x = pop();
            push(x);
            push(x);
        } else if (!strcmp(arg, "exch")) {
            int x = pop();
            int y = pop();
            push(x);
            push(y);
        } else if (!strcmp(arg, "idiv")) {
            int q = pop();
            push(pop() / q);
        } else if (!strcmp(arg, "index")) {
            int index = pop();
            if (index < 0) error(1, "negative index\n");
            if (index >= stack.size) error(1, "index underflow\n");
            push(stack.data[stack.size - index - 1]);
        } else if (!strcmp(arg, "mod")) {
            int q = pop();
            push(pop() % q);
        } else if (!strcmp(arg, "mul")) {
            push(pop() * pop());
        } else if (!strcmp(arg, "sub")) {
            int sub2 = pop();
            push(pop() - sub2);
        } else {
            fprintf(stderr, "unsupported command «%s»\n", argv[i]);
            return 1;
        }
    }
    printf("stack:");
    for (int i = 0; i < stack.size; i++)
        printf("%c%d", i?',':' ', stack.data[i]);
    puts("");
    return 0;
}
