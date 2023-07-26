// c4.c - C in four functions

// char, int, and pointer types
// if, while, return, and expression statements
// just enough features to allow self-compilation and a bit more

// Written by Robert Swierczek

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <fcntl.h>
#define int long long

char *p, *lp, // current position in source code
     *data;   // data/bss pointer

int *e, *le,  // current position in emitted code
    *id,      // currently parsed identifier
    *sym,     // symbol table (simple list of identifiers)
    tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    loc,      // local variable offset
    line,     // current line number
    src,      // print source and assembly flag
    debug;    // print executed instructions

// tokens and classes (operators last and in increasing precedence order)
enum {
  Num = 128, Fun, Sys, Glo, Loc, Id,
  Char, Else, Enum, If, Int, Return, Sizeof, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

// opcodes
// opcode usage:
// LEA (Load Effective Address): get a address calculated by basic pointer + deferenced value pointed by program pointer. The calculated address will be stored in virtual register "a", you can regard it as "eax"(the virtual register is actually a int in c language). Its specification is a = (int)(bp + *pc++)
// IMM (Immediate Value): get a deferenced value pointed by program pointer and store it in virtual register "a". Its specification is a = *pc++
// JMP (Unconditional Jump): let the program pointer point to deferenced value (a new address of next instruction) pointed by current program pointer. Its specification is pc = (int *)*pc
// JSP (Jump to Subroutine):  1. record next instruction address of calling procedure in stack memory 2. let the program pointer point to deferenced value (a new address of subroutine instruction) pointed by current program pointer. Its specification is *--sp = (int)(pc + 1); pc = (int *)*pc
// BZ (Branch if Zero): let the program pointer point to defereced value ( a new address of instruction) pointed by current program pointer if data in register "a" is zero. Its specification is pc = a ? pc + 1 : (int *)*pc
// BNZ (Branch if Not Zero): let the program pointer point to defereced value ( a new address of instruction) pointed by current program pointer if data in register "a" is Not zero. Its specification is pc = a ? (int *)*pc: pc + 1
// ENT (Enter - Used to setup stack frame for a function): Setup a stack frame by 1. Store current base pointer of calling procedure in memory pointed by stack pointer to recover after this function finished. 2. Let new base pointer of a called frame be the current stack pointer. 3. adjust stack pointer to produce memory space to store local variable in later. Its specification is *--sp = (int)bp; bp = sp; sp = sp - *pc++
// ADJ (Adjust - Used to adjust the stack pointer quickly): backward stack pointer to deaccloate the memory of frame (often used when need local memory deallocation). Its specification is sp = sp + *pc++
// LEV (Leave - Used to clean up the stack frame and return from a function.): recover stack pointer, base pointer and program pointer. Its specification is sp = bp; bp = (int *)*sp++; pc = (int *)*sp++
// LI (load int): load int from memory whose address is stored in register "a" to register "a".  Its specification is a = *(int *)a
// LC (load char): load char from memory whose address is stored in register "a" to register "a".  Its specification is a = *(char *)a
// SI (store int): store int in register "a" to memory pointed by address pointed by stack point. Its specification is *(int *)*sp++ = a
// SC (store char): store char in register "a" to memory pointed by address pointed by stack point. Its specification is *(char *)*sp++ = a
// PSH (push): put data in register "a" to stack
// OR -> MOD: bitwise, logical and arithmetic operation between data in register "a" and element in stack top, then store result in register "a". Its specification is a = *sp++ [operation]  a 
// OPEN -> EXIT: functions that can be used by source code. The argument of functions is stored in stack, and the return value of functions is stored in register "a"
enum { LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
       OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT };

// types
enum { CHAR, INT, PTR };

// identifier offsets (since we can't create an ident struct)
// Every 9 ints describe an identifier
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };

// Read a token. After execution of the function, every varibale means:
// tk: the type of the token (Id, Num, etc)
// p: point to next unread char in source code
// ival: 1. the value of number or char; 2. the address of string
// id: point to current identifier in symbol tabale (every identifier is described by 9 ints in the table)
// sym: this pointer is not changed
void next()
{
  char *pp;
  // While not counter null character in source, then read character one by one until get a defined token
  // tk can be used as first character, hash of identifier or token type
  while (tk = *p) {
    ++p;
    // If src flag is in calling command, then print every line in source code 
    if (tk == '\n') {
      if (src) {
        // Print source code
        printf("%d: %.*s", line, p - lp, lp);
        lp = p;
        // Print assembly code. pointer le and e point to last printed instruction and currently newest instruction
        while (le < e) {
          printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                           "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                           "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[*++le * 5]);
          if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n");
        }
      }
      ++line;
    }
    // Encounter #xxx. Just passing this line by moving pointer and continue read token 
    else if (tk == '#') {
      while (*p != 0 && *p != '\n') ++p;
    }
    // Encounter [a-zA-Z_][a-zA-Z0-9_]* identifier or keyword
    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {
      pp = p - 1;
      while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
        tk = tk * 147 + *p++; // tk in this place is the hash code of identifier
      tk = (tk << 6) + (p - pp); // bit shifting is also for hashing 
      id = sym;
      // Search symbol table to see whether the identifier is already stored.
      // If yes, let identifier pointer id point to it
      // If not, add the identifier in the list and let identifier pointer id point to it
      while (id[Tk]) {
        if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { tk = id[Tk]; return; }
        id = id + Idsz;
      }
      // The hash of identifier is the value of previous tk which is calculated by a series of hashing operation.
      // The name of identifier is its first char's address. Since if we know the address, we can
      // compare it with other string to check whether they are the same identifier.
      // The type of token is identifier
      id[Name] = (int)pp;
      id[Hash] = tk;
      tk = id[Tk] = Id;
      return;
    }
    // Encounter [1-9][0-9]* | 0[xX][0-9a-fA-F]* | 0[0-7]*
    else if (tk >= '0' && tk <= '9') {
      if (ival = tk - '0') { while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0'; }
      else if (*p == 'x' || *p == 'X') {
        while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
          ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
      }
      else { while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0'; }
      tk = Num;
      return;
    }
    // Encounter // or /, the former is comment and need pass whole line, the later is division
    else if (tk == '/') {
      if (*p == '/') {
        ++p;
        while (*p != 0 && *p != '\n') ++p;
      }
      else {
        tk = Div;
        return;
      }
    }
    // Encounter character or string
    else if (tk == '\'' || tk == '"') {
      pp = data;
      // If the token is just a char, then store char number in ival
      // If the token is a string, then read char of string to memory pointed by data, and let ival store the address of string's first char 
      while (*p != 0 && *p != tk) {
        // Escape char '\n' should be treated specially since it means just a single number but occupy two word in source code 
        if ((ival = *p++) == '\\') {
          if ((ival = *p++) == 'n') ival = '\n';
        }
        if (tk == '"') *data++ = ival;
      } 
      ++p;
      if (tk == '"') ival = (int)pp; else tk = Num;
      return;
    }
    else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; } // = or ==
    else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return; } // + or ++
    else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; } // - or --
    else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; } // ! or !=
    else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return; } // <= or << or <
    else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return; } // >= or >> or >
    else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; } // || or |
    else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; } // && or &
    else if (tk == '^') { tk = Xor; return; }
    else if (tk == '%') { tk = Mod; return; }
    else if (tk == '*') { tk = Mul; return; }
    else if (tk == '[') { tk = Brak; return; }
    else if (tk == '?') { tk = Cond; return; }
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return; // left token type which is not defined by enum 
  }
}

// Generate instructions of expression
// Format of expression: (prefix unary)? expression (postfix unary)? (binary expression)*
// The irreducible expression: number literal, variable identifier or function identifier
void expr(int lev)
{
  int t, *d;
  // first generate instruction of unary operator
  if (!tk) { printf("%d: unexpected eof in expression\n", line); exit(-1); }
  // Encounter number token, which is char, enum or number literal, load number value to register
  else if (tk == Num) { *++e = IMM; *++e = ival; next(); ty = INT; }
  // Encounter string token, load string's first char's address to register
  else if (tk == '"') {
    *++e = IMM; *++e = ival; next();
    while (tk == '"') next();
    data = (char *)((int)data + sizeof(int) & -sizeof(int)); ty = PTR; // data in the right points to next place where char of a string literal can be stored. "sizeof(int) & -sizeof(int)" increase the data to multiple of sizeof(int) for null-terminate purpose
  }
  // Encounter sizeof token (sizeof(int/char/pointer)), load the value to register
  else if (tk == Sizeof) {
    next(); if (tk == '(') next(); else { printf("%d: open paren expected in sizeof\n", line); exit(-1); }
    ty = INT; if (tk == Int) next(); else if (tk == Char) { next(); ty = CHAR; }
    while (tk == Mul) { next(); ty = ty + PTR; }
    if (tk == ')') next(); else { printf("%d: close paren expected in sizeof\n", line); exit(-1); }
    *++e = IMM; *++e = (ty == CHAR) ? sizeof(char) : sizeof(int);
    ty = INT;
  }
  // Encounter identifier token: function
  else if (tk == Id) {
    d = id; next();
    // the identifier is function
    if (tk == '(') {
      next();
      t = 0;
      // push value of arugument of function to stack
      while (tk != ')') { expr(Assign); *++e = PSH; ++t; if (tk == ',') next(); }
      next();
      // if it is system function, add the function index (store in d[Val]) to emitted code, the function index will guide which system function be triggered. Else store the address of function to emitted code
      if (d[Class] == Sys) *++e = d[Val];
      else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; }
      else { printf("%d: bad function call\n", line); exit(-1); }
      if (t) { *++e = ADJ; *++e = t; } // after called function finished, clear stack occupied by argument of called function
      ty = d[Type];
    }
    // if the identifier is enum number, load the value to register
    else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; }
    else {
      // if the identifier is local variable, load its address to register by LEA instruction, which can be furthered visited by LI or LC instruction 
      if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; } // the address of local variable is bp+(loc-d[Val])=bp-ordinal number of this local vriable, since loc is (amount of arugment + 1 ) of the function current expression belongs to and d[Val] is (loc+which ordinal number of this local variable).
      // if the identifier is global variable, load its address to register by IMM instruction
      else if (d[Class] == Glo) { *++e = IMM; *++e = d[Val]; }
      else { printf("%d: undefined variable\n", line); exit(-1); }
      // load variable's value to stack using LI or LC instruction
      *++e = ((ty = d[Type]) == CHAR) ? LC : LI;
    }
  }
  // Encounter parenthesis which is used in 2 ways:
  // 1. data type cast 2. priority of expression calculation
  else if (tk == '(') {
    next();
    // data type cast
    if (tk == Int || tk == Char) {
      t = (tk == Int) ? INT : CHAR; next();
      while (tk == Mul) { next(); t = t + PTR; }
      if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }
      expr(Inc);
      ty = t;
    }
    // priority of expression
    else {
      expr(Assign);
      if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    }
  }
  // Encounter *. * can be used in multiplication or deference, but in expr() function, when the first token is *, it can only be deference purpose, since * for multiplication purpose won't appear as the first token in expr() function
  else if (tk == Mul) {
    next(); expr(Inc); // get the address for deference first, which is stored in register by convention
    // check whether deference type is pointer 
    if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); }
    *++e = (ty == CHAR) ? LC : LI;
  }
  // Encounter &, & can be used in addressing, OR or XOR operation, but in expr() function, when the first token is &, it can only be addressing purpose 
  else if (tk == And) {
    next(); expr(Inc); // Get the address by ignoring LC or LI instruction
    if (*e == LC || *e == LI) --e; else { printf("%d: bad address-of\n", line); exit(-1); }
    ty = ty + PTR;
  }
  // Encounter !, ! can be used in LOGICAL NOT or NOT EQUAL, but in expr() function, when the first token is !, it can only be LOGICAL NOT. 
  else if (tk == '!') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; }
  // Bitwise not.
  else if (tk == '~') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; }
  // Encounter add, regard "add something" just as evalute "something" 
  else if (tk == Add) { next(); expr(Inc); ty = INT; }
  // Encounter sub, regard "sub something" as something * -1
  else if (tk == Sub) {
    next(); *++e = IMM;
    if (tk == Num) { *++e = -ival; next(); } else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }
    ty = INT;
  }
  // Enounter ++ or --. ++ can be used in x++ or ++x, but when the first token in expr() function is ++, it must only be ++x.  Execution flow: get the address of x, which is achieved by ignoring LC instruction, then push the address of x into stack, then put the value of x to register, then push the incremented value to register, then add x value in stack and increment value to register, finally load the value in register to location specified by stack top  
  else if (tk == Inc || tk == Dec) {
    t = tk; next(); expr(Inc);
    if (*e == LC) { *e = PSH; *++e = LC; }
    else if (*e == LI) { *e = PSH; *++e = LI; }
    else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
    *++e = PSH;
    *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
    *++e = (t == Inc) ? ADD : SUB;
    *++e = (ty == CHAR) ? SC : SI;
  }
  // When the first token in expr() function is not in above options, it must be bad expression
  else { printf("%d: bad expression\n", line); exit(-1); }

  // second generate instruction of binary operator or postfix operator
  // when current token is not of operator, then evalution ends
  while (tk >= lev) { // "precedence climbing" or "Top Down Operator Precedence" method
  // Convention: in current position, the register contains left operand. expr() function will store the value of expression in register
    t = ty;
    if (tk == Assign) {
      next();
      // Don't load left operand to register. Only push its address to stack. After right operand is loaded to register, load the value to place pointed by top stack's address 
      if (*e == LC || *e == LI) *e = PSH; else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
      expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
    }
    else if (tk == Cond) {
      next();
      // Use the instruction template of branch segment
      *++e = BZ; d = ++e;
      expr(Assign);
      if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
      *d = (int)(e + 3); *++e = JMP; d = ++e;
      expr(Cond);
      *d = (int)(e + 1);
    }
    // short LOR operator. When current value in register is not zero, we can skip evaluted value of later expression. Instruction template:
    // BNZ
    // [address of place out of the branch, which is usually filled not now but in the later when the address is known]
    // [instruction for evaluating later expression]
    else if (tk == Lor) { next(); *++e = BNZ; d = ++e; expr(Lan); *d = (int)(e + 1); ty = INT; }
    // Short LAN operator. When current value in register is already zero, we can skip evaluted value of later expression. Instruction template:
    // BZ
    // [address of place out of the branch, which is usually filled not now but in the later when the address is known]
    // [instruction for evaluating later expression]
    else if (tk == Lan) { next(); *++e = BZ;  d = ++e; expr(Or);  *d = (int)(e + 1); ty = INT; }
    // From tk == Or to tk == Shr, from tk == Mul yo tk == Mod, the instruction template is the same:
    // PSH (put the value in register to stack, further calculation will happen in stack and result will be stored in register)
    // [instructions to evaluate later expression]
    // [Calculation instruction, including OR, XOR, etc]
    else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; }
    else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; }
    else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; }
    else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; }
    else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; }
    else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; }
    else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; }
    else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; }
    else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; }
    else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; }
    else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; }
    else if (tk == Add) {
      // addition for int pointer or nested pointer is slightly different from above operator. The value of later expression should be multiplied with sizeof(int) if current expression is int pointer or nested pointer
      next(); *++e = PSH; expr(Mul);
      if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
      *++e = ADD;
    }
    else if (tk == Sub) {
      // subtrction for int pointer or nested pointer is further silghtly different from addition. If two operand in subtraction is of the same type, then the result should be how many base units (not bytes) between them, else the result should be similar with that of addition
      next(); *++e = PSH; expr(Mul);
      if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = DIV; ty = INT; }
      else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; *++e = SUB; }
      else *++e = SUB;
    }
    else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; }
    else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; }
    else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; }
    // postfix ++ or --
    else if (tk == Inc || tk == Dec) {
      // execution flow: inc or dec the value and store it in right memory location, then dec or inc the value
      if (*e == LC) { *e = PSH; *++e = LC; }
      else if (*e == LI) { *e = PSH; *++e = LI; }
      else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? ADD : SUB;
      *++e = (ty == CHAR) ? SC : SI;
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? SUB : ADD;
      next();
    }
    // square bracket deferences pointer
    else if (tk == Brak) {
      // first calculate and store address to register 
      next(); *++e = PSH; expr(Assign);
      if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }
      if (t > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
      else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }
      *++e = ADD;
      // deference address
      *++e = ((ty = t - PTR) == CHAR) ? LC : LI;
    }
    else { printf("%d: compiler error tk=%d\n", line, tk); exit(-1); }
  }
}

// Generate instructions of statements
void stmt()
{
  int *a, *b;
  // Encounter if statement
  if (tk == If) {
    next();
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign); // generate instruction for if expression
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    // To write a branch segment, the instruction template is:
    // BZ
    // [address of negative branch, which is usually filled not now but in the later when the address is known]
    // [instruction of positive branch]
    // JMP
    // [address of place out of the branch, which is usually filled not now but in the later when the address is known]
    // [instruction of negative branch]
    *++e = BZ; b = ++e;
    stmt(); // generate instruction for if block's statement
    if (tk == Else) {
      // generate JMP instruction for previous if block
      // after BZ instruction, write beginning address (e+3) of else block's instructions
      // address (e+1) stores JMP instruction
      // address (e+2) stores jump-to-address of JMP
      // address (e+3) stores jump-to-address of BZ 
      *b = (int)(e + 3); *++e = JMP; b = ++e;
      next();
      stmt();
    }
    *b = (int)(e + 1);
  }
  // Encounter while statement
  else if (tk == While) {
    next();
    a = e + 1;
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e;
    stmt();
    *++e = JMP; *++e = (int)a; // generate jump-to-address of JMP instruction 
    *b = (int)(e + 1); // generate instruction address of BZ 
  }
  // Encounter return statement
  else if (tk == Return) {
    next();
    if (tk != ';') expr(Assign);
    *++e = LEV;
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
  // Encounter block statement
  else if (tk == '{') {
    next();
    while (tk != '}') stmt();
    next();
  }
  else if (tk == ';') {
    next();
  }
  // Encounter expression statement
  else {
    expr(Assign);
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
}

int main(int argc, char **argv)
{
  int fd, bt, ty, poolsz, *idmain;
  int *pc, *sp, *bp, a, cycle; // vm registers
  int i, *t; // temps

  // pick flag from usage "c4 [-s] [-d] file ..."; src flag means
  --argc; ++argv;
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; }
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; }
  if (argc < 1) { printf("usage: c4 [-s] [-d] file ...\n"); return -1; }

  if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }

  // acclocate memory to 4 area: identifier's symbol table, code text, global data and operation stack
  poolsz = 256*1024; // arbitrary size
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }
  if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }
  if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }
  if (!(sp = malloc(poolsz))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; }

  memset(sym,  0, poolsz);
  memset(e,    0, poolsz);
  memset(data, 0, poolsz);

  p = "char else enum if int return sizeof while "
      "open read close printf malloc free memset memcmp exit void main";
  i = Char; while (i <= While) { next(); id[Tk] = i++; } // add keywords to symbol table
  i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; } // add library to symbol table
  // Class attribute indicates whether the identifier means a number, library call, function, local variable and global variable
  // Type attribute indicates the specific type when the class of the identifier is known, including int, char or pointer
  // Val attribute indicates the special value when the type of the identifier is known, which is assigned literal value
  next(); id[Tk] = Char; // handle void type
  next(); idmain = id; // keep identifier track of main

  // Read text from file and write the content to where pointed by p
  if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }
  if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; }
  p[i] = 0; // Add null character 0 to form a valid c-string
  close(fd);

  // Parse declarations of all identifiers to fill symbol table 
  line = 1;
  next();
  while (tk) {
    bt = INT; // basetype
    if (tk == Int) next();
    else if (tk == Char) { next(); bt = CHAR; }
    // Parse identifiers defind in enum syntax
    else if (tk == Enum) {
      next();
      if (tk != '{') next();
      if (tk == '{') {
        next();
        i = 0;
        while (tk != '}') {
          if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; }
          next();
          if (tk == Assign) {
            next();
            if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; }
            i = ival;
            next();
          }
          id[Class] = Num; id[Type] = INT; id[Val] = i++; // class of identifier in enum is Num, type is INT, and the value is auto incrementing int or assinged value 
          if (tk == ',') next();
        }
        next();
      }
    }
    // Parse function, local variable and global variable declaration
    // Since in c standard, everything should be declared before using, so if we don't see an identifier token now, then there must be bad declaration
    while (tk != ';' && tk != '}') {
      ty = bt;
      while (tk == Mul) { next(); ty = ty + PTR; } // allowed type of data: char, int, nested char pointer, nested int pointer, which is represented by long long ty 
      if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; }
      if (id[Class]) { printf("%d: duplicate global definition\n", line); return -1; }
      next();
      id[Type] = ty;
      if (tk == '(') { // The identifier token is to represent function
        id[Class] = Fun;
        id[Val] = (int)(e + 1); // The instruction of function will be placed at memory pointed by (e+1)
        next(); i = 0;
        while (tk != ')') {
          // add arguments declaration in function to symbol table
          ty = INT;
          if (tk == Int) next();
          else if (tk == Char) { next(); ty = CHAR; }
          while (tk == Mul) { next(); ty = ty + PTR; }
          if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; }
          // If the same local identifier is in symbol table, then there is duplicate. But a same global identifier is allowed.
          if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; }
          // Transfer Class, Type, Val attribute of identifier in global context (if it is already in global context) to HClass, HType, HVal to recover Class, Type, Val when the function finishes
          id[HClass] = id[Class]; id[Class] = Loc;
          id[HType]  = id[Type];  id[Type] = ty;
          id[HVal]   = id[Val];   id[Val] = i++; // The Val attribute of argument identifier is its offset
          next();
          if (tk == ',') next();
        }
        next();
        if (tk != '{') { printf("%d: bad function definition\n", line); return -1; }
        loc = ++i; // loc is amount of arugument+1 
        next();
        // Encounter local variable declaration
        while (tk == Int || tk == Char) {
          bt = (tk == Int) ? INT : CHAR;
          next();
          while (tk != ';') {
            ty = bt;
            while (tk == Mul) { next(); ty = ty + PTR; }
            if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }
            if (id[Class] == Loc) { printf("%d: duplicate local definition\n", line); return -1; }
            id[HClass] = id[Class]; id[Class] = Loc;
            id[HType]  = id[Type];  id[Type] = ty;
            id[HVal]   = id[Val];   id[Val] = ++i;
            next();
            if (tk == ',') next();
          }
          next();
        }
        *++e = ENT; *++e = i - loc; // i-loc is the amount of local variable
        // Get the instruction of statements
        while (tk != '}') stmt();
        *++e = LEV;
        id = sym; // unwind symbol table locals
        // After the instruction of a function finished, recover covered variable.
        while (id[Tk]) {
          if (id[Class] == Loc) {
            id[Class] = id[HClass];
            id[Type] = id[HType];
            id[Val] = id[HVal];
          }
          id = id + Idsz;
        }
      }
      else { // Encounter global variable declaration
        id[Class] = Glo;
        id[Val] = (int)data; // Val attribtute of global variable is its address. The value of global variable occupies memory space like a int
        data = data + sizeof(int);
      }
      if (tk == ',') next();
    }
    next();
  }

  if (!(pc = (int *)idmain[Val])) { printf("main() not defined\n"); return -1; }
  if (src) return 0;

  // Setup stack from high address to low address 
  bp = sp = (int *)((int)sp + poolsz);
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; t = sp; // push return value of script's main function to stack, waits for EXIT to execute and then print the return value. The return address after script's main function is current sp, which contains instruction PUSH, after which is EXIT instruction
  *--sp = argc; // put arguments to stack
  *--sp = (int)argv;
  *--sp = (int)t;

  // run...
  cycle = 0;
  while (1) {
    i = *pc++; ++cycle;
    if (debug) {
      printf("%d> %.4s", cycle,
        &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
         "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[i * 5]);
      if (i <= ADJ) printf(" %d\n", *pc); else printf("\n");
    }
    if      (i == LEA) a = (int)(bp + *pc++);                             // load local address
    else if (i == IMM) a = *pc++;                                         // load global address or immediate
    else if (i == JMP) pc = (int *)*pc;                                   // jump
    else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; }        // jump to subroutine
    else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc;                      // branch if zero
    else if (i == BNZ) pc = a ? (int *)*pc : pc + 1;                      // branch if not zero
    else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; }     // enter subroutine
    else if (i == ADJ) sp = sp + *pc++;                                   // stack adjust
    else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // leave subroutine
    else if (i == LI)  a = *(int *)a;                                     // load int
    else if (i == LC)  a = *(char *)a;                                    // load char
    else if (i == SI)  *(int *)*sp++ = a;                                 // store int
    else if (i == SC)  a = *(char *)*sp++ = a;                            // store char
    else if (i == PSH) *--sp = a;                                         // push

    else if (i == OR)  a = *sp++ |  a;
    else if (i == XOR) a = *sp++ ^  a;
    else if (i == AND) a = *sp++ &  a;
    else if (i == EQ)  a = *sp++ == a;
    else if (i == NE)  a = *sp++ != a;
    else if (i == LT)  a = *sp++ <  a;
    else if (i == GT)  a = *sp++ >  a;
    else if (i == LE)  a = *sp++ <= a;
    else if (i == GE)  a = *sp++ >= a;
    else if (i == SHL) a = *sp++ << a;
    else if (i == SHR) a = *sp++ >> a;
    else if (i == ADD) a = *sp++ +  a;
    else if (i == SUB) a = *sp++ -  a;
    else if (i == MUL) a = *sp++ *  a;
    else if (i == DIV) a = *sp++ /  a;
    else if (i == MOD) a = *sp++ %  a;

    else if (i == OPEN) a = open((char *)sp[1], *sp);
    else if (i == READ) a = read(sp[2], (char *)sp[1], *sp);
    else if (i == CLOS) a = close(*sp);
    else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); }
    else if (i == MALC) a = (int)malloc(*sp);
    else if (i == FREE) free((void *)*sp);
    else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp);
    else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp);
    else if (i == EXIT) { printf("exit(%d) cycle = %d\n", *sp, cycle); return *sp; }
    else { printf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1; }
  }
}
