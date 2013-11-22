#### Generator                                   ####
#### generate bytecode from scheme source code.  ####
####                                             ####
#### Version 1.5                                 ####
#### By featheryleaf                             ####
#### 2013.11                                     ####

import re
import sys
from bytecode import *
print('import generator.')


### Lexical Analysis ###

## Word Segmentation

_pattern = r'''
(?P<blank>      \s+)|                        # blank characters
(?P<comment>    ;.*)|                        # comment
(?P<separator>  [\(\)\[\]\{\}\|]|\#\()|      # separator  ( ) [ ] { } | #(
(?P<bool>       \#t|\#f)|                    # boolean
(?P<char>       \#\\(?:space|newline|\S))|   # char      #\a #\Z #\space #\newline
(?P<string>     "(?:\\\\|\\\"|[^"\\])*")|        # string
(?P<number>     [+-]?\d*\.?\d+)|             # number (without prefix, suffix, scientific and complex)
(?P<symbol>     [\w!$%&*+-./:<=>?@^_~]+)|    # symbol
(?:             .+)                          # invalid pattern
'''
pattern = re.compile(_pattern, re.VERBOSE)

def segment(string):
  'segment scheme source code into string fragment, return a list of string segment.'
  matches = pattern.finditer(string)
  return [match.group(match.lastgroup) for match in matches if match.lastgroup != None]

# Word Segmentation Assertion
assert segment(r'(1 2)')                  == ['(', '1', ' ', '2', ')']
assert segment(r'#\c #\space #\newline')  == ['#\\c', ' ', '#\\space', ' ', '#\\newline']
assert segment(r'#t #f')                  == ['#t', ' ', '#f']
#assert segment(r'"\"')                    == [r'"\"']
assert segment(r'"string"')               == ['"string"']
assert segment(r'("str1" "str2")')        == ['(', '"str1"', ' ', '"str2"', ')'], \
       "%s vs %s" % (segment(r'("str1" "str2")'), ['(', '"str1"', ' ', '"str2"', ')'])
assert segment(r'"\""')                   == [r'"\""'], "%s vs %s" % (segment(r'"\""'), [r'"\""'])
assert segment(r'#()')                    == ['#(', ')']
assert segment(r'symbol ...')             == ['symbol', ' ', '...']
assert segment(r'12 1.2 +12 -1.2 .1 -.1') == \
       ['12', ' ', '1.2', ' ', '+12', ' ', '-1.2', ' ', '.1', ' ', '-.1']
assert segment(r'(+ (sub 1 2) 3 "4")')    == \
       ['(', '+',' ', '(', 'sub', ' ', '1', ' ', '2', ')', ' ', '3', ' ', '"4"', ')']
assert segment(r';(+ (sub 1 2) 3 "4")')   == [';(+ (sub 1 2) 3 "4")']


## Segment To Token

def str2num(string):
  try: return int(string)
  except ValueError: pass
  try: return float(string)
  except ValueError: pass
  return complex(string.replace('i', 'j', 1))

def seg2token(seg):
  'transform a string segment into a token.'
  if seg[0] in '; \n\t\r\f\v':     # comment | blank
    return None
  elif seg[0] == '#':              # bool | char
    if seg == '#t':
      return True
    elif seg == '#f':
        return False
    elif seg == r'#\space':
        return Char(' ')
    elif seg == r'#\newline':
        return Char('\n')
    else:
        return Char(seg[2:])
  elif seg[0] == '"':              # string
    return String(seg[1:-1].replace('\\', ''))
  elif seg[0] in '0123456789' or (seg[0] in '+-.' and len(seg) > 1 and seg != '...'):  # number
    return str2num(seg)
  else:                            # symbol
    return Symbol(seg)

# Segment to Token Assertion
def Seg2token_Assertion():
  assert seg2token(' ') is None
  assert seg2token(';comment\n') is None
  assert seg2token('#f') is False
  assert seg2token('#\\space') == Char(' ')
  assert seg2token('#\\$') == Char('$')
  assert seg2token('"4"') == String('4')
  assert seg2token(r'"1\"2"') == String('1"2')
  assert seg2token('1') == 1
  assert seg2token('1.2') == 1.2
  assert seg2token('1+1.2i') == 1+1.2j
  assert seg2token('lambda') == Symbol('lambda')


## Segment List To Token Tree

def tokens(seglist):
  'transform fragment list into token tree.'
  tokens = [[Symbol('begin')]]
  for seg in seglist:
    if seg in '([{|':
      tokens.append([])
    elif seg in ')]}|':
      if len(tokens) <= 1:
        raise SchemeSyntaxError("bracket unmatch! (need ')')")
      tokens[-2].append(tokens[-1])
      del tokens[-1]
    else:
      token = seg2token(seg)
      if token is not None:
        tokens[-1].append(token)
    #print(tokens)
  if len(tokens) > 1:
    raise SchemeSyntaxError("bracket unmatch! (need '(')")
  return tokens[0]

# Segment List To Token Tree Assertion
def Tokens_Assertion():
  assert tokens(['(', '1', ' ', '2', ')']) == [Symbol('begin'), [1, 2]]
  assert tokens(['(', '1', '(', '2', '3', ')', '4', ')']) == [Symbol('begin'), [1, [2, 3], 4]]
  assert tokens(['1', '(', '2', '3', ')', '4']) == [Symbol('begin'), 1, [2, 3], 4]
  assert tokens(['(', 'define', 'x' ,'2', ')']) == [Symbol('begin'), [Symbol('define'), Symbol('x'), 2]]
  assert tokens(['(', '+', '1', '(', 'if', '#t', 'x', '2', ')', ')', '"string"', ';test']) == \
         [Symbol('begin'), [Symbol('+'), 1, [Symbol('if'), True, Symbol('x'), 2]], String('string')]


## Lexical Analysis Assertion
def Lexical_Assertion():
  assert tokens(segment(r'(1 2)')) == [Symbol('begin'), [1, 2]]
  assert tokens(segment(r'(1 (2 3) 4)  ')) == [Symbol('begin'), [1, [2, 3], 4]]
  assert tokens(segment(r'1 (2 3) 4 ;test3')) == [Symbol('begin'), 1, [2, 3], 4]
  assert tokens(segment(r'[define x 2]')) == [Symbol('begin'), [Symbol('define'), Symbol('x'), 2]]
  assert tokens(segment(r'(+ 1 (if #t x .2)) "string"')) == \
         [Symbol('begin'), [Symbol('+'), 1, [Symbol('if'), True, Symbol('x'), 0.2]], String('string')]



### Syntactic analysis ###

## Bytecode Generate

def generate(token, tail=False):
  if isinstance(token, Atom):
    gen_atom(token)
  elif len(token) == 0:
    raise SchemeSyntaxError('invalid syntax (empty bracket)!')
  elif token[0] == Symbol('define'):
    gen_define(token)
  elif token[0] == Symbol('lambda'):
    gen_lambda(token)
  elif token[0] == Symbol('if'):
    gen_if(token, tail)
  elif token[0] == Symbol('quote'):
    gen_quote(token)
  else:
    gen_proc(token, tail)

## Bytecode Generate - Atom

def gen_atom(token): # atom
  if isinstance(token, Void):
    bytecode = [PUSH, NULL] # nothing will happen
  elif isinstance(token, int):
    bytecode = [PUSH, IMM, token]
  elif isinstance(token, Symbol):
    if token in Global.native_proc:
      Global.add_const_table(NativeProc(token))
      bytecode = [PUSH, GLOBAL, len(Global.const_table)-1]
    else:
      rank, index = Global.find_symbol(token)
      if rank is None or index is None:
        raise SchemeSyntaxError('invalid syntax (%s not found)!' % token)
      bytecode = [PUSH, LOCAL, rank, index]
  else: # isinstance(token, (Bool, float, complex, Char, String))
    Global.add_const_table(token)
    bytecode = [PUSH, GLOBAL, Global.find_const_table(token)]
  Global.gen_bytecode(bytecode)

## Bytecode Generate - Define

def gen_define(token): # (define <symbol> <value>) or (define (<symbol> <param-list>) <body>)
  if len(token) < 3 or token[0] != Symbol('define'):
    raise SchemeSyntaxError('invalid syntax ' + str(token).translate(string.maketrans('[],', '() ')) + '!')
  if isinstance(token[1], Symbol): # (define <symbol> <value>)
    if len(token) != 3:
      raise SchemeSyntaxError('invalid syntax ' + str(token).translate(string.maketrans('[],', '() ')) + '!')
    _, symbol, value = token
    rank, index = Global.find_symbol(symbol)
    if rank is None or index is None:
      rank, index = Global.add_symbol(symbol)
    generate(value)
    Global.gen_bytecode([POP, LOCAL, rank, index])
    Global.gen_bytecode([PUSH, NULL])
    
  elif isinstance(token[1], list): # (define (<symbol> <param-list>) <body>)
    if len(token) < 3 or not isinstance(token[1], list) or len(token[1]) == 0:
      raise SchemeSyntaxError('invalid syntax ' + str(token).translate(string.maketrans('[],', '() ')) + '!')
    _, (symbol, *param_list), *exp_list, tail_exp = token
    rank, idx = Global.add_symbol(symbol)
    if not isinstance(symbol, Symbol):
      raise SchemeSyntaxError('invalid syntax ' + str(token).translate(string.maketrans('[],', '() ')) + '!')
    new_proc = Procedure()
    new_proc.push_env()
    index = len(Global.const_table) - 1
    
    #_, param_list, *exp_list, tail_exp = symbol
    gen_param_list(param_list)
    for exp in exp_list:
      generate(exp)
      Global.gen_bytecode([POP, NULL])
    generate(tail_exp, tail=True)
    
    new_proc.pop_env()
    Global.gen_bytecode([PUSH, GLOBAL, index])
    Global.gen_bytecode([POP, LOCAL, rank, idx])
    Global.gen_bytecode([PUSH, NULL])

  else:
    raise SchemeSyntaxError('invalid syntax (%s is not a symbol)!' % symbol)    


## Bytecode Generate - Lambda

def gen_lambda(token): # (lambda <param-list> <exp-list>)
  if len(token) < 3 or token[0] != Symbol('lambda'):
     raise SchemeSyntaxError('invalid syntax ' + str(token).translate(string.maketrans('[],', '() ')) + '!')

  new_proc = Procedure()
  new_proc.push_env()
  index = len(Global.const_table) - 1
  
  _, param_list, *exp_list, tail_exp = token
  gen_param_list(param_list)
  for exp in exp_list:
    generate(exp)
    Global.gen_bytecode([POP, NULL])
  generate(tail_exp, tail=True)
  
  new_proc.pop_env()
  Global.gen_bytecode([PUSH, GLOBAL, index])

def gen_param_list(param_list):
  if isinstance(param_list, Symbol):
    Global.add_symbol(param_list)
    Global.gen_bytecode([POPL, LOCAL, 0, 0])
    
  elif len(param_list) > 2 and param_list[-2] == Symbol('.'):
    # param_list = [param0, param1, ..., paramN , '.', paramT]
    *pre_param_list, _, tail_param = param_list
    
    for param in pre_param_list:
      rank, index = Global.add_symbol(param) 
    rank, index = Global.add_symbol(tail_param) 
    
    Global.gen_bytecode([POPN, len(pre_param_list)])
    Global.gen_bytecode([POPL, LOCAL, 0, index])
    
  else: # param_list = [param0, param1, ..., paramN]
    for param in param_list:
      rank, index = Global.add_symbol(param) 
    Global.gen_bytecode([POPN, len(param_list)])

    
## Bytecode Generate - Quote
    
def gen_quote(token):
  if len(token) != 2 or token[0] != Symbol('quote'):
     raise SchemeSyntaxError('invalid syntax ' + str(list2pair(token)) + '!')

  if not isinstance(token[1], list):
    gen_atom(token[1])
  else:
    pair = quote_list2pair(token[1])
    Global.add_const_table(pair)
    Global.gen_bytecode([PUSH, GLOBAL, Global.find_const_table(pair)])
  
def quote_list2pair(token):
  try:
    if not token:
      return None
    elif len(token) > 1 and token[1] == Symbol('.'):
      if len(token) != 3:
        raise SchemeSyntaxError('invalid syntax ' + str(list2pair(token)) + '!')
      elif not isinstance(token[2], list):
        return Pair(token[0], token[2])
      else:
        return Pair(token[0], quote_list2pair(token[2]))
    elif not isinstance(token[0], list):
      return Pair(token[0], quote_list2pair(token[1:]))
    else:
      return Pair(list2pair(token[0]), quote_list2pair(token[1:]))
    return
  except SchemeSyntaxError: pass
  raise SchemeSyntaxError('invalid syntax ' + str(list2pair(token)) + '!')


## Bytecode Generate - Procedure

def gen_proc(token, tail=False):
  length = len(token)
  if not isinstance(token, list) or length == 0:
    raise SchemeSyntaxError('invalid syntax ("%s")' % token)

  if isinstance(token[0], Symbol) and token[0] in Global.native_proc:
    gen_native_proc(token, tail)
    
  else:
    for i in range(length):
      generate(token[- 1 - i])

    if not tail:
      Global.gen_bytecode([CALL, length - 1])
    else:
      Global.gen_bytecode([TCAL, length - 1])


def gen_native_proc(token, tail=False):
  length = len(token)
  proc, *param_list = token
  index = Global.native_proc[proc]
  
  if index < SPECIAL_PROC_BOUND:        # (native-proc ...)
    for i in range(length - 1):
      generate(param_list[- 1 - i])
    Global.gen_bytecode([NCAL, index, length - 1])
  
  elif index == BEGIN:                  # (begin exp1 exp2 ... expN)
    for exp in param_list:
      generate(exp)
      Global.gen_bytecode([POP, NULL])
    del Global.bytecode_list[-1]
    
  elif index == SET:                    # (set! var value)
    if length != 3:
      raise SchemeSyntaxError('invalid syntax %s!' % list2pair(token))
    generate(token[2])
    rank, index = Global.find_symbol(token[1])
    if rank is None or index is None:
      raise SchemeSyntaxError('invalid syntax (undefind variable "%s")!' % token[1])
    Global.gen_bytecode([POP, LOCAL, rank, index])
    Global.gen_bytecode([PUSH, NULL])

  elif index == CALL_CC:                # (call/cc proc)
    if length != 2:
      raise SchemeSyntaxError('invalid syntax %s!' % list2pair(token))    
    Global.gen_bytecode([PUSH, SPECIAL, CC])
    generate(token[1])
    if not tail:
      Global.gen_bytecode([CALL, 1])
    else:
      Global.gen_bytecode([TCAL, 1])

  else:
    raise SchemeSyntaxError('invalid syntax (unsupported native call "%s")' % proc)
    pass # TO-DO


## Bytecode Generate - If
  
def gen_if(token, tail=False): # (if test exp) or (if test then-exp else-exp)
  length = len(token)
  if not isinstance(token, list) or length < 3 or length > 4 or token[0] != Symbol('if'):
    raise SchemeSyntaxError('invalid syntax %s!' % list2pair(token))

  if length == 3:
    _, test, then_exp = token
    else_exp = None
  else:
    _, test, then_exp, else_exp = token

  generate(test)
  Global.gen_bytecode([JUMP, FALSE, 0])                # need to write back
  write_back = Global.bytecode_list[-1]                # record to write back
  length = len(Global.bytecode_list)

  generate(then_exp, tail=True)
  if else_exp is not None:
    Global.gen_bytecode([JUMP, ALWAYS, 0])             # need to write back    
  write_back[2] = len(Global.bytecode_list) - length   # write back

  if else_exp is not None:
    write_back = Global.bytecode_list[-1]              # record to write back
    length = len(Global.bytecode_list)    
    generate(else_exp, tail=True)
    write_back[2] = len(Global.bytecode_list) - length # write back


## Bytecode Generate Assretion

# Generate Assertion - Atom
def gen_atom_assert(token, bytecode):
  length = len(Global.bytecode_list)
  gen_atom(token)
  assert len(Global.bytecode_list) == length + 1
  assert Global.bytecode_list[-1] == bytecode, '%s -> %s, not %s' % (token, Global.bytecode_list[-1], bytecode)

def Gen_Atom_Assertion():
  Global.init()
  _global_proc = Global.global_proc
  gen_atom_assert(None, [PUSH, NULL])
  gen_atom_assert(1, [PUSH, IMM, 1])
  gen_atom_assert(1.2, [PUSH, GLOBAL, 1])
  gen_atom_assert(1+1.2j, [PUSH, GLOBAL, 2])
  gen_atom_assert(String('string'), [PUSH, GLOBAL, 3])
  Global.symbol_table = [[{Symbol('x'):0, Symbol('y'):1}, 2], [{Symbol('x'):0}, 1]]
  gen_atom_assert(Symbol('x'), [PUSH, LOCAL, 0, 0])
  gen_atom_assert(Symbol('y'), [PUSH, LOCAL, 1, 1])
  
  assert Global.cur_proc         == Global.global_proc, print(Global.cur_proc, Global.global_proc)
  assert Global.const_table      == [Global.global_proc, 1.2, 1+1.2j, String('string')]
  assert Global.symbol_table     == [[{Symbol('x'):0, Symbol('y'):1}, 2], [{Symbol('x'):0}, 1]]
  Global.end()
  assert Global.cur_proc         is None
  assert Global.bytecode_list    == [[PUSH, GLOBAL, 0], [CALL, 0]]
  assert Global.const_table      == [Global.global_proc, 1.2, 1+1.2j, String('string')]
  assert Global.symbol_table     == [[{Symbol('x'):0, Symbol('y'):1}, 2]]
  Global.init()
  assert Global.cur_proc         == Global.global_proc
  assert Global.bytecode_list    == []
  assert Global.const_table      == [Global.global_proc]
  assert Global.symbol_table     == [[{}, 0]]

# Generate Assertion - Define
def gen_define_assert(token, push, pop):
  length = len(Global.bytecode_list)
  gen_define(token)
  assert len(Global.bytecode_list) == length + 3
  assert Global.bytecode_list[-3] == push, '%s -> %s, not %s' % (token[2], Global.bytecode_list[-2], push)
  assert Global.bytecode_list[-2] == pop,  '%s -> %s, not %s' % (token[1], Global.bytecode_list[-1], pop)
  assert Global.bytecode_list[-1] == [PUSH, NULL], '[-1] %s, not %s' % (Global.bytecode_list[-1], [PUSH, NULL])

def Gen_Define_Assertion():
  Global.init()
  _global_proc = Global.global_proc
  Global.symbol_table = [[{Symbol('y'):1}, 2], [{}, 0]]
  gen_define_assert([Symbol('define'), Symbol('x'), None], [PUSH, NULL],      [POP, LOCAL, 0, 0])
  gen_define_assert([Symbol('define'), Symbol('x'), 1],    [PUSH, IMM, 1],    [POP, LOCAL, 0, 0])
  Global.symbol_table = [[{Symbol('x'):0, Symbol('y'):1}, 2], [{Symbol('x'):0}, 1]]
  gen_define_assert([Symbol('define'), Symbol('y'), 1.2],  [PUSH, GLOBAL, 1], [POP, LOCAL, 1, 1])
  gen_define_assert([Symbol('define'), Symbol('y'), Symbol('x')],
                    [PUSH, LOCAL, 0, 0], [POP, LOCAL, 1, 1])
  gen_define_assert([Symbol('define'), Symbol('z'), String('string')],
                    [PUSH, GLOBAL, 2], [POP, LOCAL, 0, 1])
  assert Global.cur_proc         == _global_proc
  assert Global.const_table      == [Global.global_proc, 1.2, 'string']
  assert Global.symbol_table     == [[{Symbol('x'):0, Symbol('y'):1}, 2], [{Symbol('x'):0, Symbol('z'):1}, 2]]
  Global.end()
  assert Global.cur_proc         is None
  assert Global.bytecode_list    == [[PUSH, GLOBAL, 0], [CALL, 0]]
  assert Global.const_table      == [Global.global_proc, 1.2, 'string']
  assert Global.symbol_table     == [[{Symbol('x'):0, Symbol('y'):1}, 2]]
  Global.init()
  assert Global.cur_proc         == Global.global_proc
  assert Global.bytecode_list    == []
  assert Global.const_table      == [Global.global_proc]
  assert Global.symbol_table     == [[{}, 0]]

# Generate Assertion - Param List
def gen_param_list_assert(param_list, bytecode1, bytecode2):
  length = len(Global.bytecode_list)
  gen_param_list(param_list)
  if bytecode2 is None:
    assert len(Global.bytecode_list) == length + 1
    assert Global.bytecode_list[-1] == bytecode1, \
           '%s -> %s, not %s' % (param_list, Global.bytecode_list[-1], bytecode1)
  else:
    assert len(Global.bytecode_list) == length + 2
    assert Global.bytecode_list[-2] == bytecode1, \
           '%s -> %s, not %s' % (param_list, Global.bytecode_list[-2], bytecode1)
    assert Global.bytecode_list[-1] == bytecode2, \
           '%s -> %s, not %s' % (param_list, Global.bytecode_list[-1], bytecode2)
    

def Gen_Param_List_Assertion():
  Global.init()
  _global_proc = Global.global_proc
  gen_param_list_assert(Symbol('x'), [POPL, LOCAL, 0, 0], None)
  assert Global.symbol_table == [[{Symbol('x'):0}, 1]]
  Global.symbol_table = [[{}, 0]]
  gen_param_list_assert([Symbol('y'), Symbol('z')], [POPN, 2], None)
  assert Global.symbol_table == [[{Symbol('y'):0, Symbol('z'):1}, 2]]
  Global.symbol_table = [[{}, 0]]
  gen_param_list_assert([Symbol('u'), Symbol('.'), Symbol('v')], [POPN, 1], [POPL, LOCAL, 0, 1])
  assert Global.symbol_table == [[{Symbol('u'):0, Symbol('v'):1}, 2]]
  Global.symbol_table = [[{}, 0]]
  gen_param_list_assert([Symbol('a'), Symbol('b'), Symbol('c'), Symbol('.'), Symbol('d')],
                        [POPN, 3], [POPL, LOCAL, 0, 3])
  assert Global.cur_proc         == _global_proc
  assert Global.const_table      == [Global.global_proc]
  assert Global.symbol_table     == [[{Symbol('a'):0, Symbol('b'):1, Symbol('c'):2, Symbol('d'):3}, 4]]
  Global.end()
  assert Global.cur_proc         is None
  assert Global.bytecode_list    == [[PUSH, GLOBAL, 0], [CALL, 0]]
  assert Global.const_table      == [Global.global_proc]
  assert Global.symbol_table     == []
  Global.init()
  assert Global.cur_proc         == Global.global_proc
  assert Global.bytecode_list    == []
  assert Global.const_table      == [Global.global_proc]
  assert Global.symbol_table     == [[{}, 0]]

# Generate Assertion - Lambda
def gen_lambda_assert(token, bytecode_assert_list):
  length = len(Global.bytecode_list)
  gen_lambda(token)
  #for i in range(len(Global.const_table)):
  #  print('<' + str(i) + '>:', str(Global.const_table[i]))
  for i in range(len(Global.const_table)):
    if isinstance(Global.const_table[- 1 - i], Procedure):
      proc = Global.const_table[- 1 - i]
      break
  else:
    assert 0, 'Procedure not found'
  #print(proc.bytecode_list)
  assert proc.bytecode_list == bytecode_assert_list, \
         '%s -> %s, not %s' % (token, proc.bytecode_list, bytecode_assert_list)

def gen_lambda_assert2(token, outer_bytecode_list, inner_bytecode_list):
  length = len(Global.bytecode_list)
  gen_lambda(token)
  #for i in range(len(Global.const_table)):
  #  print('<' + str(i) + '>:', str(Global.const_table[i]))
  outer_proc , inner_proc = None, None
  for i in range(len(Global.const_table)):
    if isinstance(Global.const_table[- 1 - i], Procedure):
      if not inner_proc:
        inner_proc = Global.const_table[- 1 - i]
      else:
        outer_proc = Global.const_table[- 1 - i]
        break
  else:
    assert 0, 'Procedure not found'
  #print(proc.bytecode_list)
  assert inner_proc.bytecode_list == inner_bytecode_list, \
         '%s vs %s' % (inner_proc.bytecode_list, inner_bytecode_list)
  assert outer_proc.bytecode_list == outer_bytecode_list, \
         '%s vs %s' % (outer_proc.bytecode_list, outer_bytecode_list)


def Gen_Lambda_Assertion():
  Global.init()
  gen_lambda_assert([Symbol('lambda'), Symbol('x'), 1],
                    [[ALLOC, 1], [POPL, LOCAL, 0, 0], [PUSH, IMM, 1], [RET]])
  assert Global.bytecode_list == [[PUSH, GLOBAL, 1]]
  Global.bytecode_list    = []
  Global.const_table      = [Global.global_proc]
  Global.const_hash_table = {id(Global.global_proc):0}
  gen_lambda_assert([Symbol('lambda'), Symbol('x'), 1.2],
                    [[ALLOC, 1], [POPL, LOCAL, 0, 0], [PUSH, GLOBAL, 2], [RET]])
  assert Global.bytecode_list == [[PUSH, GLOBAL, 1]]
  Global.bytecode_list    = []
  Global.const_table      = [Global.global_proc]
  Global.const_hash_table = {id(Global.global_proc):0}
  gen_lambda_assert([Symbol('lambda'), [Symbol('x'), Symbol('y')], 1],
                    [[ALLOC, 2], [POPN, 2], [PUSH, IMM, 1], [RET]])
  assert Global.bytecode_list == [[PUSH, GLOBAL, 1]]
  Global.bytecode_list    = []
  temp = 1.2
  Global.const_table      = [Global.global_proc, temp]
  Global.const_hash_table = {id(Global.global_proc):0, id(temp):1}
  gen_lambda_assert([Symbol('lambda'), [Symbol('x'), Symbol('y'), Symbol('.'), Symbol('z')], 1],
                    [[ALLOC, 3], [POPN, 2], [POPL, LOCAL, 0, 2], [PUSH, IMM, 1], [RET]])
  assert Global.bytecode_list == [[PUSH, GLOBAL, 2]]
  Global.bytecode_list    = []
  Global.const_table      = [Global.global_proc]
  Global.const_hash_table = {id(Global.global_proc):0}
  gen_lambda_assert2([Symbol('lambda'), Symbol('x'),
                      [Symbol('lambda'), [Symbol('y'), Symbol('z')], 1]],
                     [[ALLOC, 1], [POPL, LOCAL, 0, 0], [PUSH, GLOBAL, 2], [RET]],
                     [[ALLOC, 2], [POPN, 2], [PUSH, IMM, 1], [RET]])
  assert Global.bytecode_list == [[PUSH, GLOBAL, 1]]
  assert Global.symbol_table  == [[{}, 0]]
  Global.init()  


# Generate Assertion - Quote
def gen_quote_list2pair_assert(token, pair, string, is_circular=False, is_list=True):
  assert quote_list2pair(token) == pair, '%s vs %s' % (quote_list2pair(token), pair)
  assert pair.is_list() == is_list, '%s: %s vs %s' % (pair, pair.is_list(), is_list)
  assert pair.is_circular() == is_circular, '%s vs %s' % (pair, is_circular)
  assert str(pair) == string, '%s vs %s' % (pair, string)

def gen_quote_assert(token, bytecode, const_table_elem=None):
  bytecode_length = len(Global.bytecode_list)
  gen_quote(token)
  assert bytecode_length + 1 == len(Global.bytecode_list)
  assert Global.bytecode_list[-1] == bytecode, \
         '%s -> %s, not %s' % (token, Global.bytecode_list[-1], bytecode)
  if const_table_elem is not None:
    assert Global.const_table[-1] == const_table_elem, \
           '%s -> %s, not %s' % (token, Global.const_table[-1], const_table_elem)

def Gen_Quote_Assertion():
  gen_quote_list2pair_assert([1], Pair(1, None), '(1)')
  gen_quote_list2pair_assert([1, 2], Pair(1, Pair(2, None)), '(1 2)')
  gen_quote_list2pair_assert([1, [2, 3]], Pair(1, Pair(Pair(2, Pair(3, None)), None)), '(1 (2 3))')
  gen_quote_list2pair_assert([[1, 2], 3], Pair(Pair(1, Pair(2, None)), Pair(3, None)), '((1 2) 3)')
  gen_quote_list2pair_assert([1, Symbol('.'), 2], Pair(1, 2), '(1 . 2)', is_list=False)
  gen_quote_list2pair_assert([1, Symbol('.'), [2, 3]], Pair(1, Pair(2, Pair(3, None))), '(1 2 3)')
  gen_quote_list2pair_assert([1, Symbol('.'), [2, Symbol('.'), 3]],
                             Pair(1, Pair(2, 3)), '(1 2 . 3)', is_list=False)
  
  Global.init()
  gen_quote_assert([Symbol('quote'), 1], [PUSH, IMM, 1])
  gen_quote_assert([Symbol('quote'), 1.2], [PUSH, GLOBAL, 1], 1.2)
  gen_quote_assert([Symbol('quote'), [1.2]], [PUSH, GLOBAL, 2], Pair(1.2, None))
  gen_quote_assert([Symbol('quote'), [1, 2]], [PUSH, GLOBAL, 3], Pair(1, Pair(2, None)))
  gen_quote_assert([Symbol('quote'), [1, 2, 3, 4]],
                   [PUSH, GLOBAL, 4], list2pair([1, 2, 3, 4]))
  gen_quote_assert([Symbol('quote'), [Symbol('quote'), Symbol('x')]],
                   [PUSH, GLOBAL, 5], Pair(Symbol('quote'), Pair(Symbol('x'), None)))
  gen_quote_assert([Symbol('quote'), [1, 2, Symbol('.'), 3]],
                   [PUSH, GLOBAL, 6], Pair(1, Pair(2, 3)))
  gen_quote_assert([Symbol('quote'), [1, 2, Symbol('.'), [3, Symbol('.'), 4]]],
                   [PUSH, GLOBAL, 7], Pair(1, Pair(2, Pair(3, 4))))

# Generate Assertion - Procedure
def gen_proc_assert(token, bytecode_assert_list):
  bytecode_length = len(Global.bytecode_list)
  assert_length = len(bytecode_assert_list)
  gen_proc(token)
  assert bytecode_length + assert_length == len(Global.bytecode_list)
  assert Global.bytecode_list[-assert_length:] == bytecode_assert_list, \
         '%s -> %s, not %s' % (token, Global.bytecode_list[-assert_length:], bytecode_assert_list)

def Gen_Proc_Assertion():
  Global.init()
  _global_proc = Global.global_proc
  gen_proc_assert([Symbol('+'), 1, 2], [[PUSH, IMM, 2], [PUSH, IMM, 1], [NCAL, ADD, 2]])
  gen_define([Symbol('define'), Symbol('x'), 1])
  gen_proc_assert([Symbol('x'), 2], [[PUSH, IMM, 2], [PUSH, LOCAL, 0, 0], [CALL, 1]])
  gen_proc_assert([Symbol('x')], [[PUSH, LOCAL, 0, 0], [CALL, 0]])
  gen_proc_assert([[Symbol('x')]], [[PUSH, LOCAL, 0, 0], [CALL, 0], [CALL, 0]])
  gen_proc_assert([Symbol('begin'), [Symbol('+'), 1, 2], 3, [Symbol('x')]],
                  [[PUSH, IMM, 2], [PUSH, IMM, 1], [NCAL, ADD, 2], [POP, NULL],
                   [PUSH, IMM, 3], [POP, NULL],
                   [PUSH, LOCAL, 0, 0], [CALL, 0]])
  assert Global.cur_proc         == _global_proc
  assert Global.const_table      == [Global.global_proc]
  assert Global.symbol_table     == [[{Symbol('x'):0}, 1]]
  Global.end()
  assert Global.cur_proc         is None
  assert Global.const_table      == [Global.global_proc]
  assert Global.symbol_table     == []
  Global.init()
  assert Global.cur_proc         == Global.global_proc
  assert Global.bytecode_list    == []
  assert Global.const_table      == [Global.global_proc]
  assert Global.symbol_table     == [[{}, 0]]

# Generate Assertion - Lambda & Procedure
def Gen_Lambda_Proc_Assertion():
  Global.init()
  _global_proc = Global.global_proc
  gen_lambda_assert([Symbol('lambda'), Symbol('x'), 1], # (lambda x 1)
                    [[ALLOC, 1], [POPL, LOCAL, 0, 0], [PUSH, IMM, 1], [RET]])
  gen_lambda_assert([Symbol('lambda'), [Symbol('x'), Symbol('y')],
                     [Symbol('+'), Symbol('x'), Symbol('y')]], # (lambda (x y) (+ x y))
                    [[ALLOC, 2], [POPN, 2],
                     [PUSH, LOCAL, 0, 1], [PUSH, LOCAL, 0, 0], [NCAL, ADD, 2],
                     [RET]])
  gen_lambda_assert([Symbol('lambda'), [Symbol('x'), Symbol('y')],
                     [Symbol('+'), Symbol('x'), Symbol('y')], Symbol('y')], # (lambda (x y) (+ x y) y)
                    [[ALLOC, 2], [POPN, 2],
                     [PUSH, LOCAL, 0, 1], [PUSH, LOCAL, 0, 0], [NCAL, ADD, 2],
                     [POP, NULL], [PUSH, LOCAL, 0, 1],
                     [RET]])
  Global.const_table = [Global.global_proc]
  Global.const_hast_table = [[{Global.global_proc:0}, 1]]
  gen_lambda_assert2([Symbol('lambda'), [Symbol('x'), Symbol('y')],
                      [[Symbol('lambda'), [Symbol('a'), Symbol('b')], Symbol('b')],
                       Symbol('x'), Symbol('y')]], # (lambda (x y) ((lambda (a b) b) x y))
                     [[ALLOC, 2], [POPN, 2],
                      [PUSH, LOCAL, 0, 1], [PUSH, LOCAL, 0, 0], [PUSH, GLOBAL, 2], [TCAL, 2],
                      [RET]],
                     [[ALLOC, 2], [POPN, 2], [PUSH, LOCAL, 0, 1], [RET]])
  Global.end()
  Global.init()

# Generate Assertion - If
def gen_if_assert(token, bytecode_assert_list):
  length = len(Global.bytecode_list)
  assert_length = len(bytecode_assert_list)
  gen_if(token)
  assert len(Global.bytecode_list) == length + assert_length
  assert Global.bytecode_list[-assert_length:] == bytecode_assert_list, \
         '%s -> %s, not %s' % (token, Global.bytecode_list[-assert_length:], bytecode_assert_list)

def Gen_If_Assertion():
  Global.init()
  _global_proc = Global.global_proc
  gen_if_assert([Symbol('if'), 1, 2, 3], \
                [[PUSH, IMM, 1], [JUMP, FALSE, 2],
                 [PUSH, IMM, 2], [JUMP, ALWAYS, 1],
                 [PUSH, IMM, 3]])
  gen_if_assert([Symbol('if'), [Symbol('+'), 1, 2], 3], \
                [[PUSH, IMM, 2], [PUSH, IMM, 1], [NCAL, ADD, 2], [JUMP, FALSE, 1],
                 [PUSH, IMM, 3]])
  Global.symbol_table = [[{Symbol('x'):0, Symbol('y'):1}, 2], [{Symbol('x'):0}, 1]]
  gen_if_assert([Symbol('if'), Symbol('x'), [Symbol('x'), 1]], \
                [[PUSH, LOCAL, 0, 0], [JUMP, FALSE, 3],
                 [PUSH, IMM, 1], [PUSH, LOCAL, 0, 0], [TCAL, 1]])
  gen_if_assert([Symbol('if'), 0, [Symbol('x'), 1], [Symbol('y'), 2]], \
                [[PUSH, IMM, 0], [JUMP, FALSE, 4],
                 [PUSH, IMM, 1], [PUSH, LOCAL, 0, 0], [TCAL, 1], [JUMP, ALWAYS, 3],
                 [PUSH, IMM, 2], [PUSH, LOCAL, 1, 1], [TCAL, 1]])


# Lexical & Generate Assertion
def lexical_generate_assert1(string, bytecode_list):
  Global.init()
  seg_list = segment(string)
  token_list = tokens(seg_list)
  generate(token_list)
  assert Global.bytecode_list == bytecode_list, \
         '\n>> %s\n>> %s\n>> %s\n>> %s\n~~ %s' % \
         (string, seg_list, token_list, Global.bytecode_list, bytecode_list)

def lexical_generate_assert2(string, global_bytecode, proc_bytecode):
  Global.init()
  seg_list = segment(string)
  token_list = tokens(seg_list)
  generate(token_list)
  
  for i in range(len(Global.const_table)):
    if isinstance(Global.const_table[- 1 - i], Procedure):
      proc = Global.const_table[- 1 - i]
      break
  else:
    assert 0, 'Procedure not found'
    
  assert proc.bytecode_list == proc_bytecode, \
         '\n>> %s\n>> %s\n>> %s\n>> %s\n~~ %s' % \
         (string, seg_list, token_list, proc.bytecode_list, proc_bytecode)
  assert Global.bytecode_list == global_bytecode, \
         '\n>> %s\n>> %s\n>> %s\n>> %s\n~~ %s' % \
         (string, seg_list, token_list, Global.bytecode_list, global_bytecode)

def lexical_generate_assert3(string, global_bytecode, outer_proc_bytecode, inner_proc_bytecode):
  Global.init()
  seg_list = segment(string)
  token_list = tokens(seg_list)
  generate(token_list)
  
  outer_proc , inner_proc = None, None
  for i in range(len(Global.const_table)):
    if isinstance(Global.const_table[- 1 - i], Procedure):
      if not inner_proc:
        inner_proc = Global.const_table[- 1 - i]
      else:
        outer_proc = Global.const_table[- 1 - i]
        break
  else:
    assert 0, 'Procedure not found'
     
  assert inner_proc.bytecode_list == inner_proc_bytecode, \
         '\n>> %s\n>> %s\n>> %s\n>> %s\n~~ %s' % \
         (string, seg_list, token_list, inner_proc.bytecode_list, inner_proc_bytecode)
  assert outer_proc.bytecode_list == outer_proc_bytecode, \
         '\n>> %s\n>> %s\n>> %s\n>> %s\n~~ %s' % \
         (string, seg_list, token_list, outer_proc.bytecode_list, outer_proc_bytecode)
  assert Global.bytecode_list == global_bytecode, \
         '\n>> %s\n>> %s\n>> %s\n>> %s\n~~ %s' % \
         (string, seg_list, token_list, Global.bytecode_list, global_bytecode)


def Lexical_Generate_Assertion():
  lexical_generate_assert1('1',
                           [[PUSH, IMM, 1]])
  lexical_generate_assert1('1 2',
                           [[PUSH, IMM, 1], [POP, NULL], [PUSH, IMM, 2]])
  lexical_generate_assert1('(1)',
                           [[PUSH, IMM, 1], [CALL, 0]])
  lexical_generate_assert1('(1 2)',
                           [[PUSH, IMM, 2], [PUSH, IMM, 1], [CALL, 1]])
  lexical_generate_assert1('(define x 1) (+ x x)',
                           [[PUSH, IMM, 1], [POP, LOCAL, 0, 0], [PUSH, NULL], [POP, NULL],
                            [PUSH, LOCAL, 0, 0], [PUSH, LOCAL, 0, 0], [NCAL, ADD, 2]])
  lexical_generate_assert1('(define x 0)',
                           [[PUSH, IMM, 0], [POP, LOCAL, 0, 0], [PUSH, NULL]])
  lexical_generate_assert1('(quote (1 2))',
                           [[PUSH, GLOBAL, 1]])

  lexical_generate_assert2('(define f (lambda (x) (* x x))) (f 2)',
                           [[PUSH, GLOBAL, 1], [POP, LOCAL, 0, 0], [PUSH, NULL], [POP, NULL],
                            [PUSH, IMM, 2], [PUSH, LOCAL, 0, 0], [CALL, 1]],
                           [[ALLOC, 1], [POPN, 1],
                            [PUSH, LOCAL, 0, 0], [PUSH, LOCAL, 0, 0], [NCAL, MUL, 2],
                            [RET]])
  lexical_generate_assert2('(define f (lambda (x) (f (- x 1)))) (f 4)',
                           [[PUSH, GLOBAL, 1], [POP, LOCAL, 0, 0], [PUSH, NULL], [POP, NULL],
                            [PUSH, IMM, 4], [PUSH, LOCAL, 0, 0], [CALL, 1]],
                           [[ALLOC, 1], [POPN, 1],
                            [PUSH, IMM, 1], [PUSH, LOCAL, 0, 0], [NCAL, SUB, 2],
                            [PUSH, LOCAL, 1, 0], [TCAL, 1],
                            [RET]])
  lexical_generate_assert2('(define f (lambda (x) (if x (* x (f (- x 1))) 1))) (f 4)',
                           [[PUSH, GLOBAL, 1], [POP, LOCAL, 0, 0], [PUSH, NULL], [POP, NULL],
                            [PUSH, IMM, 4], [PUSH, LOCAL, 0, 0], [CALL, 1]],
                           [[ALLOC, 1], [POPN, 1],
                            [PUSH, LOCAL, 0, 0], [JUMP, FALSE, 8],
                            [PUSH, IMM, 1], [PUSH, LOCAL, 0, 0], [NCAL, SUB, 2],
                            [PUSH, LOCAL, 1, 0], [CALL, 1],
                            [PUSH, LOCAL, 0, 0], [NCAL, MUL, 2], [JUMP, ALWAYS, 1],
                            [PUSH, IMM, 1], [RET]])


### Main Function ###

def Assertion_Main():
  Pair_Assertion()
  Seg2token_Assertion()
  Tokens_Assertion()
  Lexical_Assertion()
  Gen_Atom_Assertion()
  Gen_Define_Assertion()
  Gen_Param_List_Assertion()
  Gen_Lambda_Assertion()
  Gen_Quote_Assertion()
  Gen_Proc_Assertion()
  Gen_Lambda_Proc_Assertion()
  Gen_If_Assertion()
  Lexical_Generate_Assertion()
  print('All assertion succeed!!!')


def main():
  if len(sys.argv) > 1:
    scr_file = sys.argv[1]
  else:
    scr_file = input('Input scheme source file: ')
  if len(sys.argv) > 2:
    log_file = sys.argv[2]
  else:
    log_file = scr_file + '.byc'

  ifile = open(scr_file, 'r')
  string = ifile.read()

  Global.init()
  seg_list = segment(string)
  token_list = tokens(seg_list)
  generate(token_list)
  Global.end()

  ofile = open(log_file, 'w')
  ofile.write('<global>\n')
  ofile.write(str(Global.bytecode_list))
  ofile.write('\n\n')
  for i in range(len(Global.const_table)):
    ofile.write(str(Global.const_table[i]))
    ofile.write('\n')


if __name__ == '__main__':
  Assertion_Main()
  main()

