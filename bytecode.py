#### Bytecode Protocol                           ####
####                                             ####
#### Version 1.5                                 ####
#### By featheryleaf                             ####
#### 2013.11                                     ####

from collections import deque
print('import bytecode.')


### Class Defines ###

## Exception Class

class SchemeError(Exception)                     : pass
class SchemeSyntaxError(SyntaxError, SchemeError): pass
class SchemeTypeError(TypeError, SchemeError)    : pass
class SchemeValueError(ValueError, SchemeError)  : pass
class SchemeIOError(IOError, SchemeError)        : pass

## Variable Class

Void = type(None)
Bool = bool
Number = (int, float, complex)
Scalar = (Void, Bool, int, float, complex)
class Char(str)   : pass
class String(str) : pass
class Symbol(str) : pass
class Vector(list): pass
class Pair:
  PROPER_LIST   = 0
  IMPROPER_LIST = 1
  CIRCULAR_LIST = 2

  def __init__(self, a, d):
    self.data = [a, d]
  def car(self):
    return self.data[0]
  def cdr(self):
    return self.data[1]
  def set_car(self, value):
    self.data[0] = value
    return None
  def set_cdr(self, value):
    self.data[1] = value
    return None
    
  def is_list(self):
    a = b = self
    while(1):
      if isinstance(a, Pair):
        a = a.cdr()
        if isinstance(a, Pair):
          if a is b:
            return False
          a = a.cdr()
          b = b.cdr()
        else:
          return a is None
      else:
        return a is None 
  
  def is_circular(self):
    hash = {id(self):1}
    dfs = deque()
    dfs.append(self)
    while(len(dfs) > 0):
      cur = dfs.popleft()
      a = cur.car()
      b = cur.cdr()
      if (not isinstance(a, Scalar) and id(a) in hash) or (not isinstance(b, Scalar) and id(b) in hash):
        return True
      if a is not None:
        hash[id(a)] = 1
      if b is not None:
        hash[id(b)] = 1
      if isinstance(a, Pair):
        dfs.append(a)
      if isinstance(b, Pair):
        dfs.append(b)
    else:
      return False

  def __eq__(self, other):
    return isinstance(other, Pair) and self.data == other.data

  def __str__(self):
    string = '('
    if not self.is_circular():
      a = self
      while isinstance(a, Pair):
        if a is not self:
          string += ' '
        string += str(a.car())
        a = a.cdr()
      if a is not None:
        string += ' . ' + str(a)
      string += ')'
    else: # CIRCULAR_LIST
      string = '(...)'
    return string

def list2pair(li):
  if not li: # li = []
    return None
  elif not isinstance(li[0], list):
    return Pair(li[0], list2pair(li[1:]))
  else:
    return Pair(list2pair(li[0]), list2pair(li[1:]))

Atom = (Void, Bool, int, float, complex, Char, String, Symbol, Vector, Pair)

Tail = [None]


def Pair_Assertion():
  pair = list2pair([1, 2])
  assert pair == Pair(1, Pair(2, None))
  assert not pair.is_circular()
  assert pair.is_list()
  assert str(pair) == '(1 2)'
  
  pair = list2pair([1, 2, 3, 4])
  assert pair == Pair(1, Pair(2, Pair(3, Pair(4, None))))
  assert not pair.is_circular()
  assert pair.is_list()
  assert str(pair) == '(1 2 3 4)'
  
  pair = list2pair([[1, 2], 3, 4])
  assert pair == Pair(Pair(1, Pair(2, None)), Pair(3, Pair(4, None)))
  assert not pair.is_circular()
  assert pair.is_list()
  assert str(pair) == '((1 2) 3 4)'
  
  pair = Pair(1, 2)
  assert not pair.is_circular()
  assert not pair.is_list()
  assert str(pair) == '(1 . 2)'
  
  pair.set_cdr(pair)
  assert pair.is_circular()
  assert not pair.is_list()
  assert pair.car() == 1
  assert pair.cdr() == pair
  assert str(pair) == '(...)'
  
  pair = list2pair([1, 2])
  pair.set_car(pair)
  assert pair.is_circular()
  assert pair.is_list()
  assert pair.car() == pair
  assert pair.cdr() == Pair(2, None)
  assert str(pair) == '(...)'
  
  pair.set_cdr(pair)
  assert pair.is_circular()
  assert not pair.is_list()
  assert pair.car() == pair
  assert pair.cdr() == pair
  assert str(pair) == '(...)'
  
  pair1 = list2pair([1, 2])
  pair2 = list2pair([3, 4])
  pair1.set_car(pair2)
  pair2.set_cdr(pair1)
  assert pair1.is_circular()
  assert pair1.is_list()
  assert pair1.car() == pair2
  assert pair1.cdr() == Pair(2, None)
  assert str(pair1) == '(...)'
  assert pair2.is_circular()
  assert pair2.is_list()
  assert pair2.car() == 3
  assert pair2.cdr() == pair1
  assert str(pair2) == '(...)'


### Predefines of bytecode protocol ###

## Instruction Type Code
NOP     = 0    # NOP               : <no operation>
PUSH    = 1    # PUSH %var         : stack.append(%var)
POP     = 2    # POP  %var         : %var = stack.pop()
POPN    = 3    # POPN n            : cur-continuation[0:n] = stack[-n:].reverse(); del stack[-n:]
POPL    = 4    # POPL %var         : %var = Pair(stack[-?:].reverse()); del stack[-?:]
ALLOC   = 6    # ALLOC n           : cur-continuation.local-var alloc n elem
CALL    = 8    # CALL n            : call stack[-1] with args stack[-n-1:-1]; del stack[-n-1:]
NCAL    = 9    # NCAL %nproc n     : call native-proc %nproc with arg stack[-n:]; del stack[-n:]
TCAL    = 10   # TCAL n            : call without making new continuation
RET     = 12   # RET               : cur-continuation = cur-continuation.ret-continuation
JUMP    = 17   # JUMP COND offset  : if (cond(stack[-1])) PC += offset; stack.pop()

## Variable Storage Code
NULL    = 0
IMM     = 1
GLOBAL  = 2
LOCAL   = 3
# NATIVE  = 4  #not used
CC = 5

## Special Variable (for SPECIAL variable)
# PC = 0 # not used
# CC = 1

## Condition Code (for JUMP instruction)
ALWAYS = 0
TRUE   = 1
FALSE  = 2
NEVER  = 3

## Native Procedure (for NCAL instruction)
PASS       = 0

IS_EQ      = 4
IS_EQV     = 5
IS_EQUAL   = 6

NOT        = 8
IS_BOOL    = 24

ADD        = 51
SUB        = 52
MUL        = 53
DIV        = 54
ABS        = 55

MAX        = 57
MIN        = 58

EQ         = 60
LS         = 62
LE         = 63
BG         = 64
BE         = 65

IS_ZERO    = 66
IS_POS     = 67
IS_NEG     = 68
IS_ODD     = 69
IS_EVEN    = 70

IS_INT     = 120
IS_RAT     = 121
IS_REAL    = 122
IS_COMPLEX = 123
IS_NUM     = 128

IS_CHAR    = 240


MAKE_STR   = 310
STR        = 311
SUB_STR    = 312
STR_LEN    = 314
IS_STR     = 320

IS_VECTOR  = 420

IS_PAIR    = 480
IS_LIST    = 481
CONS       = 482
LIST       = 483
CAR        = 486
CDR        = 487
SET_CAR    = 488
SET_CDR    = 489
IS_NULL    = 500


IS_PORT    = 540


IS_SYMBOL  = 600

IS_PROC    = 810

OPEN_IFILE = 811
OPEN_OFILE = 812

READ       = 824
WRITE      = 828
DISPLAY    = 829


# Special Procedure (not for NCAL instruction)
SPECIAL_PROC_BOUND = 1000

SET     = 1001
BEGIN   = 1010
CALL_CC = 1020



### Global Variable & Procedure Define ###

## Procedure Class

class Procedure:
  proc_id = 0
  def push_env(self):
    self.bytecode_list        = []
    self.parent               = Global.cur_proc
    if self.parent:
      self.parent.bytecode_list = Global.bytecode_list
    self.id                   = Procedure.proc_id
    Procedure.proc_id        += 1

    Global.cur_proc           = self
    Global.bytecode_list      = self.bytecode_list
    Global.symbol_table.append([{}, 0])
    Global.add_const_table(self)

  def pop_env(self):
    Global.bytecode_list.insert(0, [ALLOC, Global.symbol_table[-1][1]]) # proc head
    Global.bytecode_list.append([RET])                                  # proc tail
    self.bytecode_list = Global.bytecode_list

    Global.cur_proc = self.parent
    if Global.cur_proc != None:
      Global.bytecode_list = Global.cur_proc.bytecode_list
    else:
      Global.bytecode_list = []
    del Global.symbol_table[-1]

  def __str__(self):
    string = '<' + str(self.id) + '>\n'
    for bytecode in self.bytecode_list:
      string += str(bytecode) + '\n'
    return string


class NativeProc:
  def __init__(self, symbol):
    if isinstance(symbol, Symbol):
      if not symbol in Global.native_proc:
        raise SchemeValueError('"%s" is not a native procedure!' % symbol)
      self.symbol        = symbol
      self.id            = Global.native_proc[symbol]
      self.bytecode_list = [[NCAL, self.id]]
      self.parent        = None
    elif isinstance(symbol, str):
      pass
      ####
  def __str__(self):
    return str(self.symbol)

## Global Variable Defines

class Global:
  global_proc      = None
  cur_proc         = None
  bytecode_list    = []
  const_table      = []
  const_hash_table = {}
  symbol_table     = []
  
  native_proc = {
    Symbol('+'):              ADD,
    Symbol('-'):              SUB,
    Symbol('*'):              MUL,
    Symbol('/'):              DIV,
    Symbol('abs'):            ABS,
    Symbol('max'):            MAX,
    Symbol('min'):            MIN,
    Symbol('='):              EQ,
    Symbol('<'):              LS,
    Symbol('<='):             LE,
    Symbol('>'):              BG,
    Symbol('>='):             BE,
    Symbol('zero?'):          IS_ZERO,
    Symbol('negative?'):      IS_NEG,
    Symbol('positive?'):      IS_POS,
    Symbol('odd?'):           IS_ODD,
    Symbol('even?'):          IS_EVEN,
    
    Symbol('integer?'):       IS_INT,
    Symbol('rational?'):      IS_RAT,
    Symbol('real?'):          IS_REAL,
    Symbol('complex?'):       IS_COMPLEX,
    Symbol('number?'):        IS_NUM,
  
    Symbol('boolean?'):       IS_BOOL,
    Symbol('not'):            NOT,

    Symbol('eq?'):            IS_EQ,
    Symbol('eqv?'):           IS_EQV,
    Symbol('equal?'):         IS_EQUAL,

    Symbol('char?'):          IS_CHAR,

    Symbol('string?'):        IS_STR,
    Symbol('make-string'):    MAKE_STR,
    Symbol('string'):         STR,
    Symbol('string-length'):  STR_LEN,
    Symbol('substring'):      SUB_STR,

    
    Symbol('vector?'):     IS_VECTOR,
    
    Symbol('pair?'):       IS_PAIR,
    Symbol('list?'):       IS_LIST,
    Symbol('list'):        LIST,
    Symbol('cons'):        CONS,
    Symbol('car'):         CAR,
    Symbol('cdr'):         CDR,
    Symbol('set-car!'):    SET_CAR,
    Symbol('set-cdr!'):    SET_CDR,
    Symbol('null?'):       IS_NULL,

    Symbol('port?'):       IS_PORT,
    Symbol('procedure?'):  IS_PROC,
    Symbol('symbol?'):     IS_SYMBOL,

    Symbol('read'):        READ,
    Symbol('write'):       WRITE,
    Symbol('display'):     DISPLAY,

    Symbol('open-input-file'): OPEN_IFILE,
    Symbol('open-output-file'): OPEN_OFILE,
    
    Symbol('set!'):        SET,
    Symbol('begin'):       BEGIN, 
    Symbol('call-with-current-continuation'): CALL_CC,    
  }

  def __init__():
    init()  
  
  def init():
    Global.cur_proc          = None
    Global.bytecode_list     = []
    Global.const_table       = []
    Global.const_hash_table  = {}
    Global.symbol_table      = []

    Procedure.proc_id = 0
    Global.global_proc       = Procedure()
    Global.global_proc.push_env()

  def end():
    Global.global_proc.pop_env()
    Global.bytecode_list = [[PUSH, GLOBAL, 0], [CALL, 0]]

  def print_global():
    print('global_proc:     ', Global.global_proc)
    print('cur_proc:        ', Global.cur_proc)
    print('bytecode_list:   ', Global.bytecode_list)
    print('const_table:     ', Global.const_table)
    print('const_hash_table:', Global.const_hash_table)
    print('symbol_table:    ', Global.symbol_table)

  def gen_bytecode(bytecode):
    if isinstance(bytecode, list):
      for code in bytecode:
        if not isinstance(code, int):
          break
      else:
        if (bytecode == [ALLOC, 0]) or bytecode == [POPN, 0]:
          return
        if  bytecode == [POP, NULL] and len(Global.bytecode_list) and Global.bytecode_list[-1] == [PUSH, NULL]:
          Global.bytecode_list.pop()
          return
        Global.bytecode_list.append(bytecode)
        return
    raise SchemeValueError('invalid bytecode format ( %s )!' % bytecode)

  def find_symbol(symbol):
    for rank in range(len(Global.symbol_table)):
      table, _ = Global.symbol_table[- 1 - rank]
      if symbol in table:
        return (rank, table[symbol])
    else: # symbol not in symbol_tables
      return (None, None)

  def add_symbol(symbol):
    Global.symbol_table[-1][0][symbol] = Global.symbol_table[-1][1]
    Global.symbol_table[-1][1] += 1
    return (0, Global.symbol_table[-1][1] - 1)
  
  def find_const_table(atom):
    try:
      if atom in Global.const_hash_table:
        return Global.const_hash_table[atom]
    except TypeError:
      if id(atom) in Global.const_hash_table:
        return Global.const_hash_table[id(atom)]
      return None

  def add_const_table(atom):
    try:
      if id(atom) in Global.const_hash_table:
        return
      elif atom in Global.const_hash_table:
        return
      else:
        Global.const_hash_table[atom] = len(Global.const_table)
        Global.const_table.append(atom)
        return
    except TypeError:
      Global.const_hash_table[id(atom)] = len(Global.const_table)
      Global.const_table.append(atom)

