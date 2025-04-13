# external globals
#   debug
#   hex
#   args
BEGIN {
    start_time = 1744540199        # systime() is not in posix, so we start with a custom fake date
    time_t_size = 4                # defaults to 4 bytes (32 bits)
    simulated_frequency = 100000   # in Hertz
    INITIAL_SP =    h2d("300000")  # @ 3MiB (a stack of 3MiB should be enough for everyone)
    # usual .org:   h2d("400000")  # @ 4MiB
    VIDEO_RAM =   h2d("10000000")  # @ 256MiB (768B for palette + 64000B for frame-buffer)
    GENERAL_RAM = h2d("10010000")  # @ 256MiB + 64kiB (grows upwards)
    BRK_ADDR = h2d("400000")       # set to the largest align(vaddr+memsz, 0x10000)
    REG_AT = 1
    REG_V0 = 2
    REG_V1 = 3
    REG_A0 = 4
    REG_A1 = 5
    REG_A2 = 6
    REG_A3 = 7
    REG_T0 = 8
    REG_T1 = 9
    REG_T2 = 10
    REG_T3 = 11
    REG_T4 = 12
    REG_T5 = 13
    REG_T6 = 14
    REG_T7 = 15
    REG_S0 = 16
    REG_S1 = 17
    REG_S2 = 18
    REG_S3 = 19
    REG_S4 = 20
    REG_S5 = 21
    REG_S6 = 22
    REG_S7 = 23
    REG_T8 = 24
    REG_T9 = 25
    REG_K0 = 26
    REG_K1 = 27
    REG_GP = 28
    REG_SP = 29
    REG_FP = 30
    REG_RA = 31
}

# CPU:
#   0..31: (u32) General Purpose Registers, CPU[0]==0
#   "HI", "LO": (u32) registers used for the multiplication and division unit
#   "PC": (u32) instruction pointer
#   "NEXTPC": (u32|nil) value for the instruction pointer that should be used
#       after running next instruction
#   "TLS_TP": thread pointer, only one thread is supported.
#   "CYCLES": how many instructions were executed in total
#   "LLBIT":  Set to 1 by LL, set to 0 by SC.  SC only writes to memory if CPU["LLBIT"] is 1

# DEBUG: can be set by using -vdebug=[item1,item2,...]
#   "args": print each of the argv[0]..argv[argc-1] and their addresses
#   "instr": list each instruction executed
#   "nocheckregs": significant optimization that skips out of bound checks of registers
#   "phdrs": list program header entries
#   "syscall": list syscalls executed with params
#   "writev": show the base and len of each iov in the writev syscall
#   "write_u32": each time a word is written to memory at once

function error(text) {
    print "\033[41mERROR:\033[0m " text >"/dev/stderr"
    exit 1
}

function h2d(hex, dec,
    i, c) {
    hex = tolower(hex)
    dec = H2D[hex]
    if (dec != "") return dec
    for (i=1; i<=length(hex); i++) {
        c = index("0123456789abcdef", substr(hex, i, 1))
        if (!c) error("invalid hex number «" hex "»")
        dec = dec*16 + c-1
    }
    H2D[hex] = dec
    return dec
}

function band(x, y,
    res, i) {
    # bitwise add, always returns unsigned integer
    if (num<0) error("invalid negative value")
    res = 0
    for (i = 1; x && y; i *= 2) {
        if ((x % 2) && (y % 2)) res += i
        x = int(x/2)
        y = int(y/2)
    }
    return res
}

function bor(x, y) {
    # dumb bitwise or, always returns unsigned integer
    return x - band(x, y) + y
}

function bxor(x, y) {
    # clever bitwise or, always returns unsigned integer
    return x + y - 2*band(x, y)
}

function blshift(num, times) {
    if (num<0) error("invalid negative value")
    return to_u32(num * POW2[times])
}

function brshift(num, times) {
    # does not work properly for negative numbers
    if (num<0) error("invalid negative value")
    return int(num / POW2[times])
}

function barshift(num, times) {
    # does not work properly for negative numbers
    if (num<0) error("invalid negative value")
    if (!times) return num;
    if (num < 0x80000000) return int(num / POW2[times])
    return 0x100000000 - POW2[32-times] + int(num / POW2[times])
}

function align(addr, sz) {
    return int((addr + sz-1) / sz) * sz
}

function clz(num,
    i) {
    # Count leading zeros of num, if num==0 then returns 32
    if (num<0) error("invalid negative value")
    if (!num) return 32
    for (i = 0; num < 0x80000000; num*=2) i++;
    return i
}

function load_elf_file(filename, ELF,
    i, b, nline) {
    # reads the hex file at filename, loading the byte array at ELF, parsing
    # the header into the following ELF keys:
    # - entry: memory address where the program starts (.org)
    # - phoff: byte-offset into ELF for the program header start
    # - shoff: byte-offset into ELF for the section header start
    # - phnum: number of program header entries
    # - shnum: number of section header entries
    # - shstrndx: index of the section header entry containing section names

    # Load bytes:
    ELF["n"] = 0
    while ((getline <filename) > 0) {
        for (i = 1; i <= NF; i++) {
            b = h2d(tolower($i))
            if (b == "") error("invalid value «"tolower($i)"» line " $0 " at " $NR)
            ELF[ELF["n"]++] = b
        }
    }
    if (!ELF["n"]) error("make sure «"filename"» exists and is a valid ELF")

    # Parse the ELF header:
    # Assert known header "\177ELF" at 0:
    for (i = 0; i < 4; i++) {
        b = substr("\177ELF", i+1, 1)
        if (sprintf("%c", ELF[i]) != b) error("expected header "b" at "i": "ELF[i])
    }
    # Make sure everything else matches our expectations:
    if (ELF[4] != 1) error("expected 32-bits executable")
    if (ELF[5] != 1) error("expected little endian executable")
    if (ELF[6] != 1) error("expected version 1 ELF")
    if (ELF[7] != 0) error("expected NONE at OSABI, got: " ELF[7])
    if (read_u16(ELF, 16) != 2) error("expected executable at 0x10")
    if (read_u16(ELF, 18) != 8) error("mips?, got: "read_u16(ELF, 18))
    if (read_u16(ELF, 20) != 1) error("v=1?, got: "read_u16(ELF, 20))
    ELF["entry"] = read_u32(ELF, 24)
    # printf "entry: 0x%x\n", ELF["entry"]
    ELF["phoff"] = read_u32(ELF, 28)
    ELF["shoff"] = read_u32(ELF, 32)
    b = read_u32(ELF, 36)
    if (band(b, 1) != 1) error("expected .noreorder, flags="b)
    # having PIC or CPIC is also allowed (and expected)
    if (band(b, h2d("f000")) != h2d("1000"))
        error("expected o32 abi, got flags=0x" sprintf("%x", b))
    if (band(b, h2d("ff0000")))
        error("EF_MIPS_MACH 0 expected, flags=0x" sprintf("%x", b))
    if (read_u16(ELF, 40) != 52) error("unexpected ehsize")
    if (read_u16(ELF, 42) != 32) error("unexpected phentsize")
    ELF["phnum"] = read_u16(ELF, 44)
    if (ELF["shnum"] && read_u16(ELF, 46) != 40) error("weird shentsize")
    ELF["shnum"] = read_u16(ELF, 48)
    ELF["shstrndx"] = read_u16(ELF, 50)  # usually the last section (shnum-1)
}

function load_elf_program(ELF, MEM, CPU,
    phoff, i, type, offset, vaddr, filesz, memsz) {
    for (i = 0; i < 32; i++)
        CPU[i] = 0
    CPU["HI"] = CPU["LO"] = 0
    CPU["PC"] = ELF["entry"]
    for (phoff=ELF["phoff"]; phoff < ELF["phoff"]+ELF["phnum"]*32; phoff+=32) {
        type = read_u32(ELF, phoff)
        offset = read_u32(ELF, phoff + 4)
        vaddr = read_u32(ELF, phoff + 8)
        filesz = read_u32(ELF, phoff + 16)
        memsz = read_u32(ELF, phoff + 20)
        if (type == 0) {
            # PT_NULL:
        } else if (type == 1) {
            # PT_LOAD:
            for (i = 0; i < filesz; i++) MEM[vaddr + i] = ELF[offset + i]
            if (vaddr + memsz > BRK_ADDR) BRK_ADDR = align(vaddr + memsz, 0x1000)
            if (DEBUG["phdrs"])
                printf "loaded %d bytes at 0x%x to 0x%x (+0x%x)\n", filesz, offset, vaddr, memsz
        } else if (type == h2d("70000000")) {
            # PT_MIPS_REGINFO
            #CPU[REG_GP] = read_u32(ELF, offset + 0x4 * 5)
            #printf "$gp=0x%x\n", CPU[REG_GP]
        } else if (type == h2d("70000003")) {
            # PT_MIPS_ABIFLAGS (ignored)
        } else if (type == h2d("6474e551")) {
            # PT_GNU_STACK: for stack NX (ignored)
        } else {
            printf "unknown program header at phoff=0x%d: " \
                "type=0x%x, offset=0x%x, vaddr=0x%x, filesz=%d\n", \
                phoff, type, offset, vaddr, filesz
        }
    }
}

function load_stack(MEM, CPU, hex, args,
    ARGS, sp, OFFSETS, i, p, k, n) {
    sub(/\.hex$/, "", hex)
    argc = split(args, ARGS, /,/) + 1
    ARGS[0] = hex

    # let's place the actual strings in GENERAL_RAM and move it upwards
    sp = INITIAL_SP
    OFFSETS[n++] = argc
    for (i = 0; i < argc; i++) {
        if (DEBUG["args"]) printf "arg[i] «%s» at @0x%x\n", ARGS[i], GENERAL_RAM
        OFFSETS[n++] = GENERAL_RAM
        GENERAL_RAM = write_zstr(MEM, GENERAL_RAM, ARGS[i])
    }
    OFFSETS[n++] = 0  # a nice NULL pointer

    for (k in ENVIRON) {
        OFFSETS[n++] = GENERAL_RAM
        GENERAL_RAM = write_zstr(MEM, GENERAL_RAM, k "=" ENVIRON[k])
    }
    OFFSETS[n++] = 0

    OFFSETS[n++] = 6 # AT_PAGESZ
    OFFSETS[n++] = 4096
    # AT_HWCAP: 0 (no support for either R6 nor MSA)
    # AT_SYSINFO: TODO...

    OFFSETS[n++] = 0 # AT_NULL
    OFFSETS[n++] = 0

    # Now let's push the pointers to the stack:
    CPU[REG_SP] = INITIAL_SP - n*4
    for (i = 0; i < n; i++) {
        write_u32(MEM, CPU[REG_SP] + i*4, OFFSETS[i])
    }
}

function read_u32(BYTES, p,
    value) {
    # reads a 32bit unsigned little-endian int at position p in byte array BYTES
    value = BYTES[p] + 256*(BYTES[p+1] + 256*(BYTES[p+2] + 256*BYTES[p+3]))
    #printf "  reading from %x: %x\n", p, value
    return value
}

function write_u32(BYTES, p, value,
    i) {
    value = to_u32(value)
    if (DEBUG["write_u32"]) printf "  MEM[0x%08x] <- 0x%08x\n", p, value
    for (i = 0; i < 4; i++) {
        BYTES[p++] = value % 256
        value = int(value / 256)
    }
}

function write_zstr(BYTES, p, text,
    i, c) {
    for (i = 0; i < length(text); i++)
        BYTES[p++] = ORD[substr(text, i+1, 1)]
    BYTES[p++] = 0
    return p
}

function read_u16(BYTES, p) {
    # reads a 16bit unsigned little-endian int at position p in byte array BYTES
    return BYTES[p] + 256*BYTES[p+1]
}

function to_s8(value) {
    value %= 0x100  # range after this: [-(2^8-1), 2^8-1]
    if (value >= 0x80) value -= 0x100  # -> [-(2^8-1), 2^7-1]
    if (value < -0x80) value += 0x100  # -> [-(2^7), 2^7-1]
    return value
}

function to_s16(value) {
    value %= 0x10000  # range after this: [-(2^16-1), 2^16-1]
    if (value >= 0x8000) value -= 0x10000  # -> [-(2^16-1), 2^15-1]
    if (value < -0x8000) value += 0x10000  # -> [-(2^15), 2^15-1]
    return value
}

function to_u16(value) {
    value %= 0x10000  # range after this: [-(2^16-1), 2^16-1]
    return value < 0 ? value + 0x10000 : value  # -> [0, 2^16-1]
}

function to_s32(value) {
    value %= 0x100000000  # range after this: [-(2^32-1), 2^32-1]
    if (value >= 0x80000000) value -= 0x100000000  # -> [-(2^32-1), 2^31-1]
    if (value < -0x80000000) value += 0x100000000  # -> [-(2^31), 2^31-1]
    return value
}

function to_u32(value) {
    value %= 0x100000000  # range after this: [-(2^32-1), 2^32-1]
    return value < 0 ? value + 0x100000000 : value  # -> [0, 2^32-1]
}

function run_emulator_instruction(MEM, CPU,
    instr, pc, op, rs, rt, shmt, imm, tmp, lo1, lo2, hi1, hi2, p, q) {
    pc = CPU["PC"]
    # We use NEXTPC to simulate the branch-delay-slot
    if (CPU["NEXTPC"]) {
        CPU["PC"] = CPU["NEXTPC"]
        delete CPU["NEXTPC"]
    } else {
        CPU["PC"] = pc + 4
    }
    # Just in case section:
    CPU[0] = 0
    if (!DEBUG["nocheckregs"]) {
        for (rs = 1; rs < 32; rs++) {
            tmp = CPU[rs]
            if (tmp<0) error("negative register found, check previous instruction!")
            if (tmp>4294967295) error("reg > 2^32-1 found, check previous instruction!")
            if (tmp!=int(tmp)) error("register with float, check previous instruction!")
        }
    }
    instr = read_u32(MEM, pc)
    op = int(instr / 67108864)  # (instr >> 26)
    funct = instr % 64
    if (DEBUG["regs"]) {
        printf "              at=%08x v0=%08x v1=%08x a0=%08x a1=%08x a2=%08x a3=%08x\n", \
                            CPU[REG_AT], CPU[REG_V0], CPU[REG_V1], CPU[REG_A0], CPU[REG_A1], CPU[REG_A2], CPU[REG_A3]
        printf "  t0=%08x t1=%08x t2=%08x t3=%08x t4=%08x t5=%08x t6=%08x t7=%08x\n", \
               CPU[REG_T0], CPU[REG_T1], CPU[REG_T2], CPU[REG_T3], CPU[REG_T4], CPU[REG_T5], CPU[REG_T6], CPU[REG_T7]
        printf "  s0=%08x s1=%08x s2=%08x s3=%08x s4=%08x s5=%08x s6=%08x s7=%08x\n", \
               CPU[REG_S0], CPU[REG_S1], CPU[REG_S2], CPU[REG_S3], CPU[REG_S4], CPU[REG_S5], CPU[REG_S6], CPU[REG_S7]
        printf "  t8=%08x t9=%08x k0=%08x k1=%08x gp=%08x sp=%08x fp=%08x ra=%08x\n", \
               CPU[REG_T8], CPU[REG_T9], CPU[REG_K0], CPU[REG_K1], CPU[REG_GP], CPU[REG_SP], CPU[REG_FP], CPU[REG_RA]
        printf "  HI=%08x LO=%08x\n", \
               CPU["HI"], CPU["LO"]
    }
    #for (tmp=0x412000; tmp<0x412030; tmp+=4) printf "  %x: %08x\n", tmp, read_u32(MEM, tmp)
    if (DEBUG["instr"]) printf "PC=%06x (%d): instr=%08x, op=%d, funct=%d\n", pc, CPU["CYCLES"], instr, op, funct
    rs = int(instr / 2097152) % 32  # (instr>>21) & 0x1f
    rt = int(instr / 65536) % 32    # (instr>>16) & 0x1f
    rd = int(instr / 2048) % 32     # (instr>>11) & 0x1f
    shmt = int(instr / 64) % 32     # (instr>>6) & 0x1f
    imm = instr % 0x10000  # = rd || shmt || bits 5..0

    if (op==0) { # SPECIAL instructions: (op=0)
        if (funct==0) { # SLL rd, rt, sa
            CPU[rd] = blshift(CPU[rt], shmt)
        } else if (funct==2) { # SRL rd, rt, sa
            CPU[rd] = brshift(CPU[rt], shmt)
        } else if (funct==3) { # SRA rd, rt, sa
            CPU[rd] = barshift(CPU[rt], shmt)
        } else if (funct==4) { # SLLV rd, rt, rs
            CPU[rd] = blshift(CPU[rt], CPU[rs] % 32)
        } else if (funct==5) { # LSA rd, rs, rt, sa
            # GPR[rd] <- sign_extend.32( (GPR[rs] << (sa+1)) + GPR[rt] )
            tmp = brshift(instr, 6) % 0x4
            CPU[rd] = to_u32(blshift(CPU[rs], tmp+1) + CPU[rt])
        } else if (funct==6) { # SRLV rd, rt, rs
            CPU[rd] = brshift(CPU[rt], CPU[rs] % 32)
        } else if (funct==7) { # SRAV rd, rt, rs
            CPU[rd] = barshift(CPU[rt], CPU[rs] % 32)
        } else if (funct==8) { # JR rs
            CPU["NEXTPC"] = CPU[rs]
        } else if (funct==9) { # JALR [rd = 31 implied,] rs
            CPU[rd] = pc + 8
            CPU["NEXTPC"] = CPU[rs]
        } else if (funct==10) { # MOVZ rd, rs, rt
            if (!CPU[rt]) CPU[rd] = CPU[rs]
        } else if (funct==11) { # MOVN rd, rs, rt
            if (CPU[rt]) CPU[rd] = CPU[rs]
        } else if (funct==12) { # SYSCALL code
            tmp = syscall(MEM, CPU, CPU[REG_V0], CPU[4], CPU[5], CPU[6], CPU[7])
            if (tmp < 0) { # this is how we can signal an error:
                CPU[REG_V0] = -tmp
                CPU[REG_A3] = 1
            } else { # ok:
                CPU[REG_V0] = tmp
                CPU[REG_A3] = 0
            }
        } else if (funct==15) { # SYNC
            # this one is easy, nothing to do here...
        } else if (funct==16) { # MFHI/CLZ
            if (!shmt) CPU[rd] = CPU["HI"];
            else error("clz or whatever is at " shmt " is not implemented")
        } else if (funct==18) { # MFLO rd
            CPU[rd] = CPU["LO"]
        } else if (funct==25) { # (SOP31: MULU/MUHU) MULTU rs, rt
            lo1 = CPU[rs] % POW2[16]
            lo2 = CPU[rt] % POW2[16]
            hi1 = int(CPU[rs] / POW2[16])
            hi2 = int(CPU[rt] / POW2[16])
            tmp = lo1*hi2 + lo2*hi1
            p = lo1*lo2 + to_u16(tmp)*0x10000
            q = hi1*hi2 + brshift(tmp, 16) + int(p / POW2[32])
            if (shmt==0) { # MULTU
                CPU["LO"] = to_u32(p)
                CPU["HI"] = q
            } else if (shmt==2) # MULU
                CPU[rd] = to_u32(p)
            else if (shmt==3) # MUHU
                CPU[rd] = q
            else error("invalid SOP31 field " shmt)
        } else if (funct==26) { # DIV/MOD [rd,] rs, rt
            p = to_s32(CPU[rs])
            q = to_s32(CPU[rt])
            if (rd) { 
                if (shmt == 2) CPU[rd] = to_u32(int(q / p))
                else if (shmt == 3) CPU[rd] = to_u32(p % q)
                else error("invalid SOP32 field " shmt)
            } else {
                CPU["LO"] = to_u32(int(p / q))
                CPU["HI"] = to_u32(p % q)
            }
        } else if (funct==27) { # DIVU/MODU [rd,] rs, rt
            p = CPU[rs]
            q = CPU[rt]
            if (rd) {
                tmp = brshift(instr, 6) % 32
                if (tmp == 2) CPU[rd] = int(q / p)
                else if (tmp == 3) CPU[rd] = p % q
                else error("invalid SOP33 field " tmp)
            } else {
                CPU["LO"] = int(p / q)
                CPU["HI"] = p % q
            }
        } else if (funct==33) { # ADDU rd, rs, rt
            CPU[rd] = to_u32(CPU[rs] + CPU[rt])
        } else if (funct==35) { # SUBU rd, rs, rt
            CPU[rd] = to_u32(CPU[rs] - CPU[rt])
        } else if (funct==36) { # AND rd, rs, rt
            CPU[rd] = band(CPU[rs], CPU[rt])
        } else if (funct==37) { # OR rd, rs, rt
            CPU[rd] = bor(CPU[rs], CPU[rt])
        } else if (funct==38) { # XOR rd, rs, rt
            CPU[rd] = bxor(CPU[rs], CPU[rt])
        } else if (funct==39) { # NOR rd, rs, rt
            CPU[rd] = 0xffffffff - bor(CPU[rs], CPU[rt])
        } else if (funct==42) { # SLT rd, rs, rt
            CPU[rd] = (to_s32(CPU[rs]) < to_s32(CPU[rt])) ? 1 : 0
        } else if (funct==43) { # SLTU rd, rs, rt
            CPU[rd] = (CPU[rs] < CPU[rt]) ? 1 : 0
        } else if (funct==52) { # TEQ rs, rt, code
            if (CPU[rs] == CPU[rt])
                error(sprintf("trap with code %d at 0x%x", int(imm/0x40), pc))
        } else {
            error(sprintf("unknown special instruction, funct=%d", funct))
        }

    } else if (op==1) { # REGIMM instructions: (op=1)(rs)(rt)(immediate)
        if (rt==0) { # BLTZ rs, offset
            if (CPU[rs] >= 0x80000000)
                CPU["NEXTPC"] = pc + 4 + to_s16(imm) * 4
        } else if (rt==1) { # BGEZ rs, offset
            if (CPU[rs] < 0x80000000)
                CPU["NEXTPC"] = pc + 4 + to_s16(imm) * 4
        } else if (rt==2) { # BLTZL rs, offset
            if (CPU[rs] >= 0x80000000)
                CPU["NEXTPC"] = pc + 4 + to_s16(imm) * 4
            else # nullify delay slot:
                CPU["PC"] = pc + 8
        } else if (rt==3) { # BGEZL rs, offset
            if (CPU[rs] < 0x80000000)
                CPU["NEXTPC"] = pc + 4 + to_s16(imm) * 4
            else # nullify delay slot:
                CPU["PC"] = pc + 8
        } else if (rt==17) { # BGEZAL (BAL when rs=0, deprecated otherwise)
            if (CPU[rs] < 0x80000000) {
                CPU[REG_RA] = pc + 8
                CPU["NEXTPC"] = pc + 4 + to_s16(imm) * 4
            }
        else # nullify delay slot:
            CPU["PC"] = pc + 8
        } else {
            error(sprintf("unknown regimm instruction, rt=0x%x", rt))
        }

    # Regular Instructions:
    } else if (op==2) { # J instr_index
        CPU["NEXTPC"] = int((pc+4)/0x10000000)*0x10000000 + instr%0x4000000*4
    } else if (op==3) { # JAL instr_index
        CPU[REG_RA] = pc + 8
        CPU["NEXTPC"] = int((pc+4)/0x10000000)*0x10000000 + instr%0x4000000*4
    } else if (op==4) { # BEQ rs, rt, imm
        if (CPU[rs] == CPU[rt])
            CPU["NEXTPC"] = pc + 4 + to_s16(imm) * 4
    } else if (op==5) { # BNE rs, rt, imm
        if (CPU[rs] != CPU[rt])
            CPU["NEXTPC"] = pc + 4 + to_s16(imm) * 4
    } else if (op==6) { # BLEZ rs, imm
        if (rt) error("(POP06) BLEZALC, BGEZALC are not supported")
        tmp = CPU[rs]
        if (!tmp || tmp >= 0x80000000)
            CPU["NEXTPC"] = pc + 4 + to_s16(imm) * 4
    } else if (op==7) { # BGTZ rs, imm
        if (rt) error("POP07 is not supported")
        tmp = CPU[rs]
        if (tmp && tmp < 0x80000000)
            CPU["NEXTPC"] = pc + 4 + to_s16(imm) * 4
    } else if (op==9) { # ADDIU rt, rs, imm
        CPU[rt] = to_u32(CPU[rs] + to_s16(imm))
    } else if (op==10) { # SLTI rt, rs, imm
        CPU[rt] = (to_s32(CPU[rs]) < to_s16(imm)) ? 1 : 0
    } else if (op==11) { # SLTIU rt, rs, imm
        CPU[rt] = (CPU[rs] < to_u32(to_s16(imm))) ? 1 : 0
    } else if (op==12) { # ANDI rt, rs, imm
        CPU[rt] = band(CPU[rs], imm)
    } else if (op==13) { # ORI rt, rs, imm
        CPU[rt] = bor(CPU[rs], imm)
    } else if (op==14) { # XORI rt, rs, imm
        CPU[rt] = bxor(CPU[rs], imm)
    } else if (op==15) { # LUI rt, imm
        CPU[rt] = imm * 0x10000
    } else if (op==20) { # BEQL rs, rt, imm
        if (CPU[rs] == CPU[rt])
            CPU["NEXTPC"] = pc + 4 + to_s16(imm) * 4
        else # nullify delay slot:
            CPU["PC"] = pc + 8
    } else if (op==21) { # BNEL rs, rt, imm
        if (CPU[rs] != CPU[rt])
            CPU["NEXTPC"] = pc + 4 + to_s16(imm) * 4
        else # nullify delay slot:
            CPU["PC"] = pc + 8
    } else if (op==23) { # BGTZL rs, imm
        if (CPU[rt]) error("POP27 is not supported")
        tmp = CPU[rs]
        if (tmp && tmp < 0x80000000)
            CPU["NEXTPC"] = pc + 4 + to_s16(imm) * 4
        else # nullify delay slot:
            CPU["PC"] = pc + 8

    } else if (op==28) { # SPECIAL2 Instructions
        if (funct==0x2) { # MUL rd, rs, rt
            lo1 = CPU[rs] % 0x10000
            lo2 = CPU[rt] % 0x10000
            hi1 = int(CPU[rs] / 0x10000)
            hi2 = int(CPU[rt] / 0x10000)
            CPU[rd] = to_u32(lo1*lo2 + to_u16(lo1*hi2 + lo2*hi1)*0x10000)
        } else if (funct==0x20) { # CLZ rd, rs
            CPU[rd] = clz(CPU[rs])
        } else {
            error(sprintf("unknown SPECIAL2 instruction 0x%x", funct))
        }

    } else if (op==31) { # SPECIAL3 Instructions
        if (funct==0) { # EXT rt, rs, pos, size
            CPU[rt] = brshift(CPU[rs], shmt) % POW2[rd+1]
        } else if (funct==4) { # INS rt, rs, pos, size
            # shmt = pos
            # rd = pos+size-1
            # size = rd-pos+1 = rd-shmt+1
            # highest 32-(pos+size) bits from CPU[rt] shifted to (pos+size)
            tmp = CPU[rt]
            tmp -= tmp % POW2[rd+1]  # remove lowest rd+1 bits
            # restore lowest pos bits from CPU[rt]:
            tmp += CPU[rt] % POW2[shmt]
            # lowest size bits from CPU[rs] shifted to pos:
            CPU[rt] = tmp + (CPU[rs] % POW2[rd-shmt+1]) * POW2[shmt]

            # eg: pos=2, size=8  => rd=9
            #   tmp = CPU[rt] - CPU[rt]%POW2[10]
            #   tmp += CPU[rt] % POW2[2]
            #   CPU[rt] = tmp + (CPU[rs] % POW2[8]) * POW2[2]
        } else if (funct==32) { # BSHFL:
            if (shmt==0x10) { # SEB rd, rt
                CPU[rd] = to_s8(CPU[rt])
            } else {
                error(sprintf("unknown BSHFL instruction 0x%x", shmt))
            }
        } else if (funct==0x3b) { # RDHWR rt, rd[, sel]
            if (rd != 29)
                error("RDHWR only supports reading hw register $29")
            CPU[rt] = CPU["TLS_TP"]
        } else {
            error(sprintf("unknown SPECIAL3 instruction %d", funct))
        }
    } else if (op==32) { # LB rt, imm(rs)
        CPU[rt] = to_u32(to_s8(MEM[CPU[rs] + to_s16(imm)]))
    } else if (op==35) { # LW rt, imm(rs)
        CPU[rt] = read_u32(MEM, CPU[rs] + to_s16(imm))
    } else if (op==36) { # LBU rt, imm(rs)
        CPU[rt] = MEM[CPU[rs] + to_s16(imm)] + 0
    } else if (op==40) { # SB rt, imm(rs)
        MEM[CPU[rs] + to_s16(imm)] = CPU[rt] % 256
    } else if (op==43) { # SW rt, imm(rs)
        write_u32(MEM, CPU[rs] + to_s16(imm), CPU[rt])
    } else if (op==48) { # LL rt, imm(rs)
        CPU[rt] = read_u32(MEM, CPU[rs] + to_s16(imm))
        CPU["LLBIT"] = 1
    } else if (op==56) { # SC rt, imm(rs)
        if (CPU["LLBIT"]) {
            write_u32(MEM, CPU[rs] + to_s16(imm), CPU[rt])
            CPU[rt] = 1
            delete CPU["LLBIT"]
        } else CPU[rt] = 0
    } else {
        error("unknown regular instruction")
    }
    CPU["CYCLES"]++
}

function syscall(MEM, CPU, nr, a0, a1, a2, a3,
    lo, fd, s, i, base, len, j, tmp) {
    # Called when a SYSCALL instruction is issued.
    if (DEBUG["syscall"]) printf "syscall(%d, 0x%x, 0x%x, 0x%x, 0x%x)\n", nr, a0, a1, a2, a3
    nr -= 4000
    if (nr==45) { # int brk(void *addr);
        # a0==NULL is used to query the BRK_ADDR
        if (a0) BRK_ADDR = a0
        return BRK_ADDR
    } else if (nr==54) { # int ioctl(int fd, unsigned long request, ...);
        # uppermost 2 bits of a0 contain DIR (_IO=0, _IOW=1, _IOR=2, _IOWR=3)
        # then 14 bits for the size of the structure.
        # then 8 bits for the type
        # and finally the lower 8 bits for the number
        if (a0==0x40087468) { # TIOCGWINSZ=_IOR('t', 104, struct winsize): get window size
            # man ioctl_tty
            write_u32(MEM, a1, 80 * 0x10000 + 24)  # lie saying it's 80x24
            write_u32(MEM, a1+4, 0)  # we don't even know the size
        }
        return 0
    } else if (nr==146) { # ssize_t writev(int fd, const struct iovec *iov, int iovcnt);
        fd = a0==1?"/dev/stdout" : a0==2?"/dev/stderr" : ""
        if (!fd) return -9  #-EBADF
        for (i = 0; i < a2; i++) {
            # a1 is an array of struct iovec {void* iov_base; size_t iov_len;};
            base = read_u32(MEM, a1 + i*8)
            len = read_u32(MEM, a1 + i*8 + 4)
            for (j = 0; j < len; j++) s = s sprintf("%c", MEM[base + j])
            if (DEBUG["writev"]) printf "write base: 0x%x, len: %d\n", base, len
        }
        printf "%s", s >fd
        return length(s)
    } else if (nr==166) { # int nanosleep(const struct timespec *req, struct timespec *rem);
        # struct timespec { time_t tv_sec; long   tv_nsec; }
        # Since sleeping is super heavyweight here (we have to run a process), we rather
        # do nothing by default.
        tmp = read_u32(MEM, a0)
        if (time_t_size == 4) {
            tmp += read_u32(MEM, a0+4) / 1e9
        } else { # 8:
            tmp += read_u32(MEM, a0+4) * 1e9
            tmp += read_u32(MEM, a0+8) / 1e9
        }
        if (tmp < 0) return 0
        CPU["CYCLES"] += int(tmp * simulated_frequency)
        # print "sleeping for " tmp " seconds"
        if (DEBUG["sleep"]) system("sleep "tmp)
        return 0
    } else if (nr==263) { # int clock_gettime(clockid_t clockid, struct timespec *tp);
        # we are going to ignore clockid (a0)
        # struct timespec {time_t tv_sec; long tv_nsec;}
        tmp = start_time + int(CPU["CYCLES"] / simulated_frequency)
        write_u32(MEM, a1, to_u32(tmp))
        #print "tv_nsec:", CPU["CYCLES"], int(CPU["CYCLES"] / simulated_frequency % 1 * 1e9)
        if (time_t_size == 4) {
            write_u32(MEM, a1+4, int(CPU["CYCLES"] / simulated_frequency % 1 * 1e9))
        } else { # 8:
            write_u32(MEM, a1+4, int(tmp / POW2[32]))
            write_u32(MEM, a1+8, int(CPU["CYCLES"] / simulated_frequency % 1 * 1e9))
        }
        return 0
    } else if (nr==246) { # void exit(int status);
        exit a0
    } else if (nr==252) { # pid_t set_tid_address(int *tidptr);
        # store tidptr somewhere... I guess
        return 42  # the thread_id
    } else if (nr==283) { # int set_thread_area(unsigned long addr):
        CPU["TLS_TP"] = a0
        return 0
    } else {
        error("unknown syscall! use -vdebug=syscall")
    }
}

function run_emulator(MEM, CPU) {
    for (;;) run_emulator_instruction(MEM, CPU)
}

function main(hex, args,
    ELF, MEM, CPU, i, n, A) {
    if (!hex) error("hex is undefined, run emulator with -vhex=filename.hex")

    # Load ORD (array inverse to λc.sprintf("%c", c))
    for (i = 1; i < 256; i++) ORD[sprintf("%c", i)] = i

    # pre-calculate 2^i for i in [0..32] (busybox awk not always supports ^)
    POW2[0] = 1
    for (i = 1; i <= 32; i++) POW2[i] = 2 * POW2[i-1]

    # load DEBUG from debug
    n = split(debug, A, ",")
    for (i = 1; i <= n; i++) DEBUG[A[i]]++

    load_elf_file(hex, ELF)
    load_elf_program(ELF, MEM, CPU)
    load_stack(MEM, CPU, hex, args)
    run_emulator(MEM, CPU)
}
BEGIN {main(hex, args)}
