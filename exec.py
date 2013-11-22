#!/usr/bin/env python3

from bytecode import *
from generator import *

class SchemeBytecodeError(SchemeError):pass
class SchemeInvalidOperationError(SchemeError):pass
class SchemeRuntimeError(SchemeError):pass
class Cont: #Activation Records
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
		self.embryo = True
		# only caught continuations have stack and restore_pc
		self.stack = None
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

def restore_cc(env, cont, arg):
	# del env.stack, env.cc
	env.stack = cont.stack.copy()
	env.stack.append(arg)
	env.cc = cont.copy()
	env.cc.stack = None
	# env.cc = cont

def exec_bytecode(env):
	bytecodes = Global.bytecode_list
	byc = bytecodes[0]
	byc_idx = 1
	if env.cc: env.cc.cur_pc = byc_idx
	while True:
		# print(byc)
		if   byc[0] == NOP:
			print('encountered NOP')
		elif byc[0] == PUSH:
			item = None
			if   byc[1] == IMM: item = byc[2]
			elif byc[1] == GLOBAL:
				item = Global.const_table[byc[2]]
				if isinstance(item, Procedure):
					c = Cont(item)
					prev = env.cc
					if prev:
						while not prev.procedure is item.parent: prev = prev.prev_proc_cont
					c.prev_proc_cont = prev
					item = c
			elif byc[1] == LOCAL:
				item = trace_back(env.cc, byc[2], byc[3])
			elif byc[1] == SPECIAL and byc[2] == CC:
				item = env.cc
				item.stack = env.stack.copy()
			# print('pc:%d ' % byc_idx + str(item))
			env.stack.append(item)
		elif byc[0] == POP:
			item = env.stack.pop()
			if   byc[1] == NULL: pass
			elif byc[1] == LOCAL: trace_back(env.cc, byc[2], byc[3], True, item)
			else: raise SchemeInvalidOperationError('not allowed to modify native/special')
		elif byc[0] == POPN:
			n = byc[1]
			if n == 1 and isinstance(env.stack[-1], Cont) and env.stack[-1].stack != None:
				# print("call/cc")
				cont = env.stack[-1].copy()
				env.stack[-1].stack = None
				env.stack[-1] = cont
			if n:
				env.cc.local_vars[:n] = list(reversed(env.stack[-n:]))
				del env.stack[-n:]
		elif byc[0] == POPL:
			n = env.cc.argc
			item = list2pair(list(reversed(env.stack[-n:])))
			del env.stack[-n:]
			if   byc[1] == NULL: pass
			elif byc[1] == LOCAL:   trace_back(env.cc, byc[2], byc[3], True, item)
			else: raise SchemeBytecodeError('invalid destination for POPL' % byc[1])
		elif byc[0] == ALLOC:
			# print("ALLOC %d for " % byc[1], str(env.cc))
			env.cc.local_vars = [None]*byc[1]
		elif byc[0] == CALL:
			item = env.stack.pop()
			# if isinstance(item, Procedure):
			# 	new_cont = Cont(item, byc[1])
			# 	if env.cc: 
			# 		new_cont.prev_cont = env.cc
			# 		c = env.cc
			# 		while not c.procedure is item.parent: c = c.prev_proc_cont
			# 		new_cont.prev_proc_cont = c
			# 	env.cc = new_cont
			# 	env.cc.ret_pc = byc_idx
			# 	env.cc.argc = byc[1]
			# 	bytecodes = env.cc.procedure.bytecode_list
			# 	byc_idx = 0
			if   isinstance(item, NativeProc): exec_native(env, item.id, byc[1])
			elif isinstance(item, Cont):
				if not item.stack is None: # restore a cont.
					restore_cc(env, item, env.stack.pop())
					bytecodes = env.cc.procedure.bytecode_list
					byc_idx = env.cc.cur_pc
				else:
					new_cont = item.copy()
					new_cont.embryo = False
					if env.cc: new_cont.prev_cont = env.cc
					env.cc = new_cont
					env.cc.ret_pc = byc_idx
					env.cc.argc = byc[1]
					bytecodes = env.cc.procedure.bytecode_list
					byc_idx = 0
					env.cc.cur_pc = byc_idx 
			else: raise SchemeBytecodeError('invalid item to CALL:%s' % type(item))
		elif byc[0] == NCAL:
			exec_native(env, byc[1], byc[2])
		elif byc[0] == TCAL:
			item = env.stack.pop()
			# if isinstance(item, Procedure):
			# 	env.cc.procedure = item
			# 	c = env.cc.prev_proc_cont
			# 	while not c.procedure is item.parent: c = c.prev_proc_cont
			# 	env.cc.prev_proc_cont = c
			# 	env.cc.argc = byc[1]
			# 	bytecodes = env.cc.procedure.bytecode_list
			# 	byc_idx = 0
			# 	env.cc.cur_pc = byc_idx
			if isinstance(item, NativeProc): exec_native(env, item.id, byc[1])
			if not isinstance(item, Cont):
				print(env.stack)
				print(bytecodes)
				print(byc_idx)
				raise SchemeRuntimeError("want to call a %s?" % type(item))
			if not item.stack is None: # restore a cont.
				restore_cc(env, item, env.stack.pop())
				bytecodes = env.cc.procedure.bytecode_list
				byc_idx = env.cc.cur_pc
			else:
				new_cont = item.copy()
				new_cont.embryo = False
				new_cont.prev_cont = env.cc.prev_cont
				new_cont.ret_pc = env.cc.ret_pc
				env.cc = new_cont
				env.cc.argc = byc[1]
				bytecodes = env.cc.procedure.bytecode_list
				byc_idx = 0
				env.cc.cur_pc = byc_idx
		elif byc[0] == RET:
			if env.cc.prev_cont: bytecodes = env.cc.prev_cont.procedure.bytecode_list
			else: bytecodes = Global.bytecode_list
			byc_idx = env.cc.ret_pc
			env.cc = env.cc.prev_cont
		elif byc[0] == JUMP:
			taken = True
			if   byc[1] == TRUE:
				taken = env.stack.pop() and True or False
			elif byc[1] == FALSE:
				if env.stack.pop(): taken = False
				else: taken = True
			elif byc[1] == NEVER:
				taken = False
			if taken: byc_idx = byc_idx + byc[2]
		else: raise SchemeBytecodeError('invalid bytecode [%d]' % byc[0])
		if byc_idx == len(bytecodes):
			if len(env.stack) != 1:
				print('Stack not right!')
			print('Excution Complete. Bye Bye!')
			return
		byc = bytecodes[byc_idx]
		byc_idx = byc_idx + 1
		if env.cc: env.cc.cur_pc = byc_idx

def exec_file(fname):
	with open(fname, 'r') as f:
		src_code = f.read()
	Global.init()
	seg_list = segment(src_code)
	token_list = tokens(seg_list)
	generate(token_list)
	Global.end()
	exec_bytecode(ExecEnv())

def main(args):
	fname = 'test.scm'
	if len(args):
		exec_file(args[0])
	else:
		print('Usage: exec.py srcfile')


def exec_native(env, num, argc): 
	if num == PASS: pass
	elif num == IS_EQ: pass
	elif num == IS_EQV: pass
	elif num == IS_EQUAL: pass
	elif num == NOT: pass
	elif num == IS_BOOL: pass

	elif num == ADD:
		val = sum(env.stack[-argc:])
		del env.stack[-argc:]
		env.stack.append(val)
	elif num == SUB:
		val = env.stack.pop() - sum(env.stack[1-argc:])
		del env.stack[1-argc:]
		env.stack.append(val)
	elif num == MUL:
		val = env.stack.pop()
		argc = argc - 1
		while argc:
			val = val * env.stack.pop()
			argc = argc - 1
		env.stack.append(val)
	elif num == DIV:
		val = env.stack.pop()
		argc = argc - 1
		while argc:
			val = val / env.stack.pop()
			argc = argc - 1
		env.stack.append(val)
	elif num == ABS:
		if argc != 1: raise SchemeRuntimeError('abs works on one argument only')
		if isinstance(item, (int, float)):
			if env.stack[-1] < 0: env.stack[-1] = -env.stack[-1]
		else: raise SchemeRuntimeError('abs works on int or float only')
	elif num == MAX: pass
	elif num == MIN: pass

	elif num == EQ: pass
	elif num == LS: pass
	elif num == LE:
		if argc != 2:
			raise SchemeRuntimeError("<= takes two args")
		a = env.stack.pop()
		b = env.stack.pop()
		env.stack.append(a<=b) 
	elif num == BG:
		if argc != 2:
			raise SchemeRuntimeError("> takes two args")
		a = env.stack.pop()
		b = env.stack.pop()
		env.stack.append(a>b)
	elif num == BE: pass

	elif num == IS_ZERO:
		val = env.stack.pop()
		env.stack.append(val == 0 or val == 0.0)
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
		# 	print(env.stack[-i-1])
		while argc:
			print(env.stack.pop())
			argc = argc - 1
		env.stack.append(None)
	# elif num == CALL_CC:
	# 	proc = env.stack.pop()
	# 	item = env.cc
	# 	item.stack = env.stack.copy()
	# 	env.stack.append(item)
	# 	env.stack.append(proc)
	# 	call 1
			
	else: raise SchemeNativeError('native call [%d] does not exist' % num)


if __name__ == '__main__':
	import sys
	main(sys.argv[1:])