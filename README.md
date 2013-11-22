#Scheme 解释器报告
###代码生成
__经过预处理步骤，宏定义均被展开，复杂的语法结构被简单的替换，代码生成只需面对最基础的语言部分，包含有：__

1. 空格，作为分隔符
2. 括号，用于指示语义层级
3. 双引号，用于指示字符串常量
4. 单引号，作为`quote`的语法糖
5. 数值常量
6. 其他合法的标示符
    * 其中部分为语言保留字

    代码生成部分的输入是经简化的Scheme源代码，输出为字节码、常量表等。对标示符的引用经静态分析可以转换为数字标号。过程分两步：__分词和结构化__ 以及 __字节码生成__。

    #####分词和结构化
    以空格（和括号）为分隔符，处理数值、字符串常量（双引号包围的），去除注释（由分号起始至行尾），分辨标识符，将源码转换为token流。之后初始化符号表，常量表。对token流则根据括号的层级，使用递归下降的方法进行后续的分析。
    #####字节码生成
    在字节码生成的每一步中（这是一个递归的过程），
    对于出现的单引号：

    * 将后续内容认为是`list`常量
     
     在一对括号内部，首先分析__第一个__元素。该元素一定是标示符，否则报错：

     * 首先试图在内置标示符中寻找：
         * 如果其类型是函数（如数值运算，输入输出等），则依次（递归地）处理后续参数（从前往后），统计参数个数，输出`call`的字节码。
             * 如果是拥有特殊语义的标示符，如`if`和`define`等，则进入对应的处理函数。跳转地址如果无法确定的话，使用回填的策略（由于语言的特殊性，这里一般不用考虑逻辑短路的情况）。`define`会在当前__语义层级__的符号表中创建新的条目，并根据后续元素确定其类型。
             * 否则认为其是用户定义的标示符。从当前层级的符号表开始，依次向上寻找。随后分析后续的参数，生成`push`的字节码。

             对于后续的每一个元素，如果是`list`或另一级括号，回到上一步处理，否则根据类型：

             * 数值或字符串常量：在常量表中创建条目，并产生`push`的字节码。
             * 标示符：`push`该变量对应的标号（带有层级的信息）

             对于一门函数式编程语言，最重要的部分就是函数的定义和调用。Scheme中用`lambda`创建函数。对应到代码生成的流程是：

             1. 创建新的`Procedure`实例（其定义在上一次报告中有说明）。
             2. 根据`lambda`的内容，使用上一步生成的字节码填充该`Procedure`，合适地处理对变量的引用（在语义层级上）。

             ###运行时环境

             __新问题__：当`current-continuation`被捕获时，需要保存运行时栈的状态。（因为被捕获后，在再一次调用之前，栈已经被其他语句修改了）。


#10月10日讨论总结-预处理

###预处理用scheme语言实现, 用scheme.com的解释器  Petite Chez Scheme

###输出是scheme源代码

###1. 遇到内置函数/其他标示符-> 直接输出

###2. 遇到define-syntax

####记录name和keywords。对于一组pattern->output,
pattern: 都是递归的结构

1. list
2. improper list
3. list ... -> 记录前后确定的元素个数
4. improper list ... ->去掉最后一个cdr, 转化成3
5. 其他报错

###3. 遇到宏的名字-> 递归地以其出现的block粒度进行匹配展开，直到不能继续展开

1. 符号表中识别出宏
2. 尝试每一组pattern->output,
    1. 递归地往标号里填充内容，其他按字面匹配
    2. 对于output
        1. 非变量->直接输出
        2. 变量->查变量表，在递归的最里层输出值
        3. 如果出错，直接抛出错误      
3. 将错如写入另一文件


###（define-syntax name (keyword ...) ((_ . pattern) output)...）



#9月29日讨论总结-runtime

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


###已经解决的问题
1. `call/cc`是否原封不动的保持状态？在捕获`current continuation`之后，如果局部变量被修改，再次跳回去时，该局部变量是什么状态？
    * 该变量被修改。见large_test.scm中66-76行
2. 内嵌`procedure`被捕获后，如果在外部被调用，对于内嵌`procedure`的父`procedure`中的局部变量引用，如何处理？
	* 见man-or-boy.scm

