
#9月29日讨论总结-Scheme

###preprocessing阶段
	待确认

###bytecode generator 需要处理的语法结构
1. `lambda`, `quote`
2. `define`, `set!`
3. `if`
4. `()` 函数调用
5. 内置函数（例如：`+ - * / sin cos`）

###运行时的重要数据结构
1. `procedure` 即抽象过程，包含
    * 字节码序列
    * 局部变量个数
    * 标号
2. `continuation` 即过程被执行时的具体形式，包含
    * `procedure`原型
    * 局部变量
    * 上一层`continuation`
    	* 返回地址
    * `program counter` 目前执行的位置
3. `constant table` 常量表，包含
    * 字节码
    * 常量数据
4. `stack` 运行时栈
    * 用于传递参数、返回值，保存中间结果等
5. `continuation chain` 执行链，即调用链
	* `current continuation` 当前执行环境，即调用链的头部

###bytecode种类，括号中是参数，斜体是可选的
1. 栈操作
	* `push (something) to stacktop` 压栈，例如：   
   		* `push (immediate) to stacktop` 立即数
   		* `push (from const table) to stacktop` 取常量
   		* `push (from local var) to stacktop` 局部变量
   		* `push (current continuation) ...` push cc
    * `pop (number of objects)` 用于平衡栈，对于无用的返回值
    * `pop (number of objects) to local variables in current continuation` 用于参数传递，并弹栈（n个元素），也可用于加载变量
2. 过程调用
    * `call (procedure id)` 调用`procedure`，构造新的`continuation`
    	* **_`call (stacktop)` 调用位于栈顶的`procedure`_ 待讨论，是否动态生成`procedure`
    * `tail-call (procedure id)` 尾递归调用，复用`current continuation`
    * `native-call (native-procedure id) (number of arguments)` 调用内置过程
3. 返回
    * `return` 销毁`current continuation`并返回
    * `return (pop number of objects)` 返回并平衡栈，对于过程中的无用临时变量
4. 跳转
    * `jump (bytecode location)` 无条件跳转，目标必须在当前`procedure`内
    * `jump (cond) (bytecode location)` 条件跳转，根据`stack`顶上的元素与`cond`是否等价
5. 赋值
	* `set (from) (to)` 用于`define`和`set!` 


###未解决的问题
1. `call/cc`是否原封不动的保持状态？在捕获`current continuation`之后，如果局部变量被修改，再次跳回去时，该局部变量是什么状态？
    * 待wky在各个实现上测试
2. 内嵌`procedure`被捕获后，如果在外部被调用，对于内嵌`procedure`的父`procedure`中的局部变量引用，如何处理？
	* 待wky在各个实现上测试


###其他
####其他问题也请一并提出，可以在issue中提出