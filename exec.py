#!/usr/bin/env python3

from bytecode import *
from generator import *

class SchemeBytecodeError(SchemeError):pass
class SchemeInvalidOperationError(SchemeError):pass
class SchemeRuntimeError(SchemeError):pass
class Cont: #Activation Records
	__slots__ = ["procedure", "argc", "prev_cont", "prev_proc_cont", "local_vars", "cur_pc", "ret_pc", "stack"] 
	# init_cnt = 0
	'Continuation, execution envrionment of an procedure'
	def __init__(self, init_proc=None, argc=None):
		self.procedure = init_proc
		self.argc = argc
		self.prev_cont = None
		self.prev_proc_cont = None
		# self.local_vars = []
		self.local_vars = None
		self.cur_pc = None
		self.ret_pc = None
		# self.embryo = True
		# only caught continuations have stack and restore_pc
		self.stack = None
		# Cont.init_cnt = Cont.init_cnt + 1
	def copy(self):
		c = Cont(self.procedure, self.argc)
		c.local_vars, c.stack = self.local_vars, self.stack
		c.ret_pc, c.cur_pc = self.ret_pc, self.cur_pc
		c.prev_proc_cont, c.prev_cont = self.prev_proc_cont, self.prev_cont
		return c
	def __str__(self):
		return '<Cont, proc id:%d>' % self.procedure.id
class ExecEnv:
	'Entire execution envronment'
	def __init__(self):
		self.stack = []
		# self.sp = -1
		self.cc = None

def optimize_bycode(proc):
	byc = proc.bytecode_list[0]
	if byc[0] == ALLOC and byc[1] == 0: # remove ALLOC 0
		proc.bytecode_list.pop(0)
	byc = proc.bytecode_list[0]
	if byc[0] == POPN and byc[1] == 0: # remove POPN 0
		proc.bytecode_list.pop(0)
	i = 0
	while i + 1 < len(proc.bytecode_list):
		if proc.bytecode_list[i] == [PUSH, NULL] and proc.bytecode_list[i+1] == [POP, NULL]:
			proc.bytecode_list.pop(i)
			proc.bytecode_list.pop(i)
		else:
			i = i + 1

def trace_back(cc, trace_rank, item_idx, assign=False, new_val=None):
	# print(str(cc))
	while trace_rank:
		cc = cc.prev_proc_cont
		trace_rank = trace_rank - 1
	if assign: cc.local_vars[item_idx] = new_val
	else:
		return cc.local_vars[item_idx]

# def save_cc(cc):
	# cont = cc.copy()
	# cont.stack = env.stack.copy()
	# return cont # deep-copy continuation chain, duplicate stack

# def restore_cc(env, cont, arg):
# 	# del env.stack, env.cc
# 	env.stack = cont.stack.copy()
# 	env.stack.append(arg)
# 	env.cc = cont.copy()
# 	env.cc.stack = None
# 	# env.cc = cont

def exec_bytecode():
	bytecodes = Global.bytecode_list
	byc = bytecodes[0]
	byc_idx = 1
	# if env.cc: env.cc.cur_pc = byc_idx
	stack = []
	cc = None
	while True:
		if   byc[0] == NOP:
			print('encountered NOP')
		elif byc[0] == PUSH:
			item = None
			if   byc[1] == IMM: item = byc[2]
			elif byc[1] == GLOBAL:
				item = Global.const_table[byc[2]]
				if isinstance(item, Procedure):
					# optimize_bycode(item)
					c = Cont(item)
					prev = cc
					if prev:
						while not prev.procedure is item.parent: prev = prev.prev_proc_cont
					c.prev_proc_cont = prev
					item = c
			elif byc[1] == LOCAL:
				item = trace_back(cc, byc[2], byc[3])
			elif byc[1] == CC:
				item = cc
				item.stack = stack.copy()
			# print('pc:%d ' % byc_idx + str(item))
			stack.append(item)
		elif byc[0] == POP:
			item = stack.pop()
			if byc[1] == LOCAL: trace_back(cc, byc[2], byc[3], True, item)
			# else: raise SchemeInvalidOperationError('not allowed to modify native/special')
		elif byc[0] == POPN:
			n = byc[1]
			if n == 1 and isinstance(stack[-1], Cont) and stack[-1].stack != None:
				# print("call/cc")
				cont = stack[-1].copy()
				stack[-1].stack = None
				stack[-1] = cont
			elif n:
				cc.local_vars[:n] = list(reversed(stack[-n:]))
				del stack[-n:]
		elif byc[0] == POPL:
			n = cc.argc
			item = list2pair(list(reversed(stack[-n:])))
			del stack[-n:]
			if   byc[1] == NULL: pass
			elif byc[1] == LOCAL:   trace_back(cc, byc[2], byc[3], True, item)
			else: raise SchemeBytecodeError('invalid destination for POPL' % byc[1])
		elif byc[0] == ALLOC:
			# print("ALLOC %d for " % byc[1], str(cc))
			cc.local_vars = [None]*byc[1]
		elif byc[0] == CALL:
			item = stack.pop()
			# if isinstance(item, Procedure):
			# 	new_cont = Cont(item, byc[1])
			# 	if cc: 
			# 		new_cont.prev_cont = cc
			# 		c = cc
			# 		while not c.procedure is item.parent: c = c.prev_proc_cont
			# 		new_cont.prev_proc_cont = c
			# 	cc = new_cont
			# 	cc.ret_pc = byc_idx
			# 	cc.argc = byc[1]
			# 	bytecodes = cc.procedure.bytecode_list
			# 	byc_idx = 0
			if   isinstance(item, NativeProc): exec_native(stack, item.id, byc[1])
			elif isinstance(item, Cont):
				if not item.stack is None: # restore a cont.
					arg = stack.pop()
					stack = item.stack.copy()
					stack.append(arg)
					cc = item.copy()
					cc.stack = None
					bytecodes = cc.procedure.bytecode_list
					byc_idx = cc.cur_pc
				else:
					new_cont = item.copy()
					# new_cont.embryo = False
					if cc: new_cont.prev_cont = cc
					cc = new_cont
					cc.ret_pc = byc_idx
					cc.argc = byc[1]
					bytecodes = cc.procedure.bytecode_list
					byc_idx = 0
					cc.cur_pc = byc_idx 
			else: raise SchemeBytecodeError('invalid item to CALL:%s' % type(item))
		elif byc[0] == NCAL:
			exec_native(stack, byc[1], byc[2])
		elif byc[0] == TCAL:
			item = stack.pop()
			# if isinstance(item, Procedure):
			# 	cc.procedure = item
			# 	c = cc.prev_proc_cont
			# 	while not c.procedure is item.parent: c = c.prev_proc_cont
			# 	cc.prev_proc_cont = c
			# 	cc.argc = byc[1]
			# 	bytecodes = cc.procedure.bytecode_list
			# 	byc_idx = 0
			# 	cc.cur_pc = byc_idx
			if isinstance(item, NativeProc): exec_native(stack, item.id, byc[1])
			if not isinstance(item, Cont):
				print(stack)
				print(bytecodes)
				print(byc_idx)
				raise SchemeRuntimeError("want to call a %s?" % type(item))
			if not item.stack is None: # restore a cont.
				arg = stack.pop()
				stack = item.stack.copy()
				stack.append(arg)
				cc = item.copy()
				cc.stack = None
				bytecodes = cc.procedure.bytecode_list
				byc_idx = cc.cur_pc
			else:
				new_cont = item.copy()
				# new_cont.embryo = False
				new_cont.prev_cont = cc.prev_cont
				new_cont.ret_pc = cc.ret_pc
				cc = new_cont
				cc.argc = byc[1]
				bytecodes = cc.procedure.bytecode_list
				byc_idx = 0
				cc.cur_pc = byc_idx
		elif byc[0] == RET:
			if cc.prev_cont: bytecodes = cc.prev_cont.procedure.bytecode_list
			else: bytecodes = Global.bytecode_list
			byc_idx = cc.ret_pc
			cc = cc.prev_cont
		elif byc[0] == JUMP:
			taken = True
			if   byc[1] == TRUE:
				taken = stack.pop() and True or False
			elif byc[1] == FALSE:
				if stack.pop(): taken = False
				else: taken = True
			elif byc[1] == NEVER:
				taken = False
			if taken: byc_idx = byc_idx + byc[2]
		else: raise SchemeBytecodeError('invalid bytecode [%d]' % byc[0])
		if byc_idx == len(bytecodes):
			if len(stack) != 1:
				print('Stack not right!')
			print('Excution Complete. Bye Bye!')
			return
		byc = bytecodes[byc_idx]
		byc_idx = byc_idx + 1
		if cc: cc.cur_pc = byc_idx

def exec_file(fname):
	with open(fname, 'r') as f:
		src_code = f.read()
	Global.init()
	seg_list = segment(src_code)
	token_list = tokens(seg_list)
	generate(token_list)
	Global.end()
	for item in Global.const_table:
		if isinstance(item, Procedure):
			optimize_bycode(item)
	exec_bytecode()
	# print(Cont.init_cnt)

# def represent_bytecode(byc):


def main(args):
	fname = 'test.scm'
	if len(args):
		exec_file(args[0])
	else:
		print('Usage: exec.py srcfile')


def exec_native(stack, num, argc): 
	if num == PASS: pass
	elif num == IS_EQ: pass
	elif num == IS_EQV: pass
	elif num == IS_EQUAL: pass
	elif num == NOT: pass
	elif num == IS_BOOL: pass

	elif num == ADD:
		val = sum(stack[-argc:])
		del stack[-argc:]
		stack.append(val)
	elif num == SUB:
		val = stack.pop() - sum(stack[1-argc:])
		del stack[1-argc:]
		stack.append(val)
	elif num == MUL:
		val = stack.pop()
		argc = argc - 1
		while argc:
			val = val * stack.pop()
			argc = argc - 1
		stack.append(val)
	elif num == DIV:
		val = stack.pop()
		argc = argc - 1
		while argc:
			val = val / stack.pop()
			argc = argc - 1
		stack.append(val)
	elif num == ABS:
		if argc != 1: raise SchemeRuntimeError('abs works on one argument only')
		if isinstance(item, (int, float)):
			if stack[-1] < 0: stack[-1] = -stack[-1]
		else: raise SchemeRuntimeError('abs works on int or float only')
	elif num == MAX: pass
	elif num == MIN: pass

	elif num == EQ: pass
	elif num == LS: pass
	elif num == LE:
		if argc != 2:
			raise SchemeRuntimeError("<= takes two args")
		a = stack.pop()
		b = stack.pop()
		stack.append(a<=b) 
	elif num == BG:
		if argc != 2:
			raise SchemeRuntimeError("> takes two args")
		a = stack.pop()
		b = stack.pop()
		stack.append(a>b)
	elif num == BE: pass

	elif num == IS_ZERO:
		val = stack.pop()
		stack.append(val == 0 or val == 0.0)
	elif num == IS_POS: pass
	elif num == IS_NEG: pass
	elif num == IS_ODD: pass
	elif num == IS_EVEN: pass

	elif num == IS_INT: pass
	elif num == IS_RAT: pass
	elif num == IS_REAL: pass
	elif num == IS_COMPLEX: pass
	elif num == IS_NUM: pass

	elif num == IS_CHAR: pass


	elif num == MAKE_STR: pass
	elif num == STR: pass
	elif num == SUB_STR: pass
	elif num == STR_LEN: pass

	elif num == IS_VECTOR:  pass

	elif num == IS_PAIR: pass
	elif num == IS_LIST: pass
	elif num == CONS: pass
	elif num == LIST: pass
	elif num == CAR: pass
	elif num == CDR: pass
	elif num == SET_CAR: pass
	elif num == SET_CDR: pass
	elif num == IS_NULL: pass

	elif num == IS_PORT: pass
	elif num == IS_SYMBOL: pass
	elif num == IS_PROC: pass
	elif num == OPEN_IFILE: pass
	elif num == OPEN_OFILE: pass
	elif num == READ: pass
	elif num == WRITE: pass
	elif num == DISPLAY:
		# for i in range(argc):
		# 	print(stack[-i-1])
		while argc:
			print(stack.pop())
			argc = argc - 1
		stack.append(None)
	# elif num == CALL_CC:
	# 	proc = stack.pop()
	# 	item = env.cc
	# 	item.stack = stack.copy()
	# 	stack.append(item)
	# 	stack.append(proc)
	# 	call 1
			
	else: raise SchemeNativeError('native call [%d] does not exist' % num)


if __name__ == '__main__':
	import sys
	main(sys.argv[1:])