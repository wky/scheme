
#9月29日讨论总结-Scheme

###bytecode generator 需要处理的语法结构
1. `lambda`, `quote`
2. `define`, `set!`
3. `if`
4. `()` 函数调用
5. 内置函数

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
6. `current continuation` 当前执行环境

###bytecode种类，括号中是参数，斜体是可选的
1. 栈操作
    * `push (from const table) to stacktop` 取常量
    * `push (immediate) to stacktop` 存放立即数
    * `pop (number of objects) to local variables in current continuation` 参数传递
    * `pop (number of objects)` 用于平衡栈，对于某些不干净的函数
2. 过程调用
    * `call (procedure id)` 由`procedure`构造新的`continuation`
    * **_`call (stacktop)` 调用位于栈顶的`procedure`_ 待讨论，是否动态生成`procedure`
    * `tail-call (procedure id)` 重新初始化`current continuation`
        * 如果 `tail-call`中的`procedure` 与 `current continuation`相同即为尾递归
        * 不同则是新过程调用，但是不需要构造新的`continuation`
    * `native-call (native-procedure id) (arguments ...)` 调用内置过程
3. 跳转
    * `jump (bytecode location)` 无条件跳转，目标必须在当前`procedure`内
    * `jump (cond) (bytecode location)` 条件跳转，根据`stack`顶上的元素
4. 返回
    * `return` 销毁`current continuation`并返回
    * **_`return (number of objects)` 返回并平衡栈_

###两个未解决问题
1. `call/cc`是否原封不动的保持状态？在捕获`current continuation`之后，如果局部变量被修改，再次跳回去时，该局部变量是什么状态？
    * 待我在各个实现上测试
2. 另一个问题我忘了，请侯哥补充

###其他
####其他问题也请一并提出，可以在issue中提出