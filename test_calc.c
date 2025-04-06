#include <stdarg.h>
#include <stdint.h>
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

int push(int n) {
    stack.data[stack.size++] = n;
    if (stack.size > MAX_STACK_SIZE)
        error(1, "stack overflow\n");
    return n;
}
int pop() {
    if (stack.size <= 0)
        error(1, "stack underflow\n");

    return stack.data[--stack.size];
}

uint32_t hash(const char *text) {
    if (!text) return 0;
    uint32_t res = 0x811c9dc5;
    while (*text) res = (res ^ *(text++)) * 0x01000193;
    return res;
}

typedef void(command_fn_t)();

#define COMMANDS_TABLE_SIZE 64
#define MAX_COMMAND_COLLISIONS 3
struct {
    const char *name;
    command_fn_t *fn;
} commands[COMMANDS_TABLE_SIZE] = {[0 ... (COMMANDS_TABLE_SIZE-1)] = 0};

void register_command(const char *name, command_fn_t *fn) {
    uint32_t h = hash(name);
    for (int c = 0; c < MAX_COMMAND_COLLISIONS; c++) {
        uint32_t bucket = (h+c) % COMMANDS_TABLE_SIZE;
        if (!commands[bucket].name) {
            commands[bucket].name = name;
            commands[bucket].fn = fn;
            return;
        }
    }
    error(1,
        "max collisions found for command «%s», "
        "increase COMMANDS_TABLE_SIZE (to next power of 2)\n"
        "or MAX_COMMAND_COLLISIONS (if usage < 50 percent)\n",
        name);
}

command_fn_t * find_command(const char *name) {
    uint32_t h = hash(name);
    for (int c = 0; c < MAX_COMMAND_COLLISIONS; c++) {
        uint32_t bucket = (h+c) % COMMANDS_TABLE_SIZE;
        if (!commands[bucket].name) return NULL;
        if (!strcmp(name, commands[bucket].name)) return commands[bucket].fn;
        bucket = (bucket + 1) % COMMANDS_TABLE_SIZE;
    }
    return NULL;
}

void command_add() { push(pop() + pop()); }
void command_and() { push(pop() & pop()); }
void command_bitshift() {
    int shift = pop();
    unsigned int num = pop();
    push(shift >= 0 ? (num<<shift) : (num>>-shift));
}
void command_copy() {
    int index = pop();
    if (index < 0) error(1, "negative index\n");
    if (index > stack.size) error(1, "index underflow\n");
    for (int i = 0; i < index; i++)
        push(stack.data[stack.size - index]);
}
void command_count() { push(stack.size); }
void command_dup() { push(push(pop())); }
void command_eq() { push(pop() == pop() ? -1 : 0); }
void command_exch() {
    int x = pop();
    int y = pop();
    push(x);
    push(y);
}
void command_idiv() {
    int q = pop();
    if (q == 0) error(1, "division by zero\n");
    push(pop() / q);
}
void command_index() {
    int index = pop();
    if (index < 0) error(1, "negative index\n");
    if (index >= stack.size) error(1, "index underflow\n");
    push(stack.data[stack.size - index - 1]);
}
void command_mod() { int q = pop(); push(pop() % q); }
void command_mul() { push(pop() * pop()); }
void command_neg() { push(-pop()); }
void command_not() { push(~pop()); }
void command_or() { push(pop() | pop()); }
void command_sub() { int x = pop(); push(pop() - x); }
void command_udiv() {
    unsigned int q = pop();
    if (q == 0) error(1, "division by zero\n");
    push((unsigned)pop() / q);
}
void command_weight() { push(__builtin_popcount(pop())); }
void command_xor() { push(pop() ^ pop()); }

void command_zzzext() {
    // Extracts some of the bits from the operand.
    // eg: 0x12345678 zzzext 0x34, or in decimal: 52
    push((pop()>>16) & 0xff);
}
void command_zzzins() {
    // This command was crafted to match one of the few cases where GCC
    // uses the INS MIPS32 instruction.
    // eg: 0x12345678 0x9abdef zzzins 0x12abdef8, or in decimal: 313253624
    unsigned int ins = pop();
    union {
        int num;
        struct {
            unsigned int a : 4;
            unsigned int b : 20;
            unsigned int c : 8;
        } bits;
    } rt = { .num = pop() };
    rt.bits.b = ins;
    push(rt.num);
}

int main(int argc, char * argv[]) {
    //printf("argc=%d\n", argc);
    if (argc < 2 || !strcmp(argv[1], "--help")) {
        error(1, "Usage %s <token> <token> <token> ...\n", *argv);
        /**
         * <token> can be an integer number or an operation, if it's an integer
         * it's going to be pushed in the stack, otherwise operations are:
         * 
         * int1 int2 <add>   int:       computes int1 + int2
         * int1 int2 <and>   int:       computes bitwise int1 AND int2
         * int1 int2 <bitshift> int:    logic shifts int1 by int2 bits (left pos, right neg)
         *   v1…vn n <copy> v1…vn v1…vn:  copies the top n values
         *   ⊢ v1…vn <count» v1…vn n:   counts the number of elements in the stack
         *         v <dup>   v:         duplicates top value
         *     v1 v2 <eq>    int:       pops 2 values, and pushes 1 if equal or 0 if not
         *     v1 v2 <exch>  v2 v1:     swaps top two values
         * int1 int2 <idiv>  int:       computes signed division int1 / int2
         *   vn…v0 n <index> vn…v0 vn:  pushes the n-th value
         * int1 int2 <mod>   int:       computes the remainder of dividing int1 by int2
         * int1 int2 <mul>   int:       computes int1 * int2
         *      int1 <neg>   int:       computes -int1
         *      int1 <not>   int:       computes ~int1 (bitwise not)
         * int1 int2 <or>    int:       computes bitwise int1 (inclusive) OR int2
         * int1 int2 <sub>   int:       computes int1 - int2
         * int1 int2 <udiv>  int:       computes unsigned division int1 / int2
         *      int1 <weight> int:      computes the number of 1 bits in int1
         * int1 int2 <xor>   int:       computes bitwise int1 XOR int2
         **/
    }
    register_command("add", command_add);
    register_command("and", command_and);
    register_command("bitshift", command_bitshift);
    register_command("copy", command_copy);
    register_command("count", command_count);
    register_command("dup", command_dup);
    register_command("eq", command_eq);
    register_command("exch", command_exch);
    register_command("idiv", command_idiv);
    register_command("index", command_index);
    register_command("mod", command_mod);
    register_command("mul", command_mul);
    register_command("neg", command_neg);
    register_command("not", command_not);
    register_command("or", command_or);
    register_command("udiv", command_udiv);
    register_command("sub", command_sub);
    register_command("weight", command_weight);
    register_command("xor", command_xor);
    // secret instructions, don't tell anyone:
    register_command("zzzext", command_zzzext);
    register_command("zzzins", command_zzzins);

    for (int i = 1; i < argc; i++) {
        char * endptr;
        char * arg = argv[i];
        int n = strtol(arg, &endptr, 0);
        command_fn_t * fn;
        if (*arg && !*endptr) {
            push(n);
        } else if ((fn = find_command(arg))) {
            fn();
        } else {
            fprintf(stderr, "unsupported command «%s» hash=%x, supported commands:\n", arg, hash(arg));
            int count = 0;
            for (int i = 0; i < COMMANDS_TABLE_SIZE; i++)
                if (commands[i].name) {
                    count++;
                    fprintf(stderr, "  (bucket=%03x, hash=%08x) «%s»\n", i, hash(commands[i].name), commands[i].name);
                }

            printf("command hash usage %.2f%%\n", 100.*count/COMMANDS_TABLE_SIZE);
            return 1;
        }
    }
    printf("stack:");
    for (int i = 0; i < stack.size; i++)
        printf("%c%d", i?',':' ', stack.data[i]);
    puts("");
    return 0;
}
