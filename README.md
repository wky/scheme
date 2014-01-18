# Scheme 解释器报告 #

**wky, featheryleaf, E478**

----------------

## 1. 选题 ##

使用 Scheme 和 Python 实现一个 Scheme 解释器。

### 1.1  Why Scheme? ###

1. 函数式语言，可以学习理解不同的编程模型；
2. 语法简单，代码规范不足 50 页；
3. 吴阳怿对Scheme语言比较熟悉。

### 1.2  Why Python? ###

1. 比 C 语言更灵活的语义特性和丰富的内置库；
2. 不需要自己管理垃圾回收；
3. wky对 Python 语言比较熟悉。  

(E478负责的预处理部分使用 Scheme 语言编写)

### 1.3  Why not compiler? ###

编译器将遇到实现上困难：

1. 依赖具体机器的编译器后端；
2. 垃圾回收模块的实现与测试；
3. 动态类型的编译支持。

这些困难的解决需要更多的时间和精力。

----------------

## 2. 分工 ##

- **Part A: 宏的预处理   (by E478)**；
- **Part B: 中间代码生成 (by featheryleaf)**；
- **Part C: 解释执行     (by wky)**。

### 2.1 Why Preprocess ###

Scheme支持复杂的宏定义，需要单独处理。

### 2.2 Why Bytecode ###

1. 一定程度上弥补了不能编译的缺憾；
2. 多次解释同一个程序时，只需执行 Part C；
3. 方便分工合作。


----------------

## 3. 宏的预处理 (by E478) ##

### 3.1  算法 ###

1. 首先使用 scheme 的 `read` 函数直接读入 S 表达式；
2. 由于 S 表达式本质上就是一颗树，故我们在树上做遍历，对于每个结点都递归处理；
3. 处理结点时，先判断是否为 `(define-syntax ...)` 的格式。
   - 若是，进行 **宏定义** 的处理；
   - 否则，进行 **匹配宏** 的处理。

### 3.2  宏定义的处理 ###

依次识别宏定义参数中的变量，将每个 `symbol` 转化为 `(symbol-type . symbol)`。
其中 `symbol-type` 的可能取值: `#t` 表示变量，`#f` 表示常量，`...` 表示是一个匹配表的变量。

>     (define-syntax when (key1 key2)  
>       [(_ x e ...) (if x ((lambda () e ...)))]  
>       [(_ key1) '(this is (when key1))]  
>       [(_ #f key2 1) (when key2)])

1. 首先 `(key1 key2)` 被识别为两个自定义关键字 `key1` 和 `key2`

2. 然后依次处理之后的三个 pattern

  - 1) 第一个 pattern 为 `(_ x e ...)`，
     
        `_` 是一个占位符，没有实际意义，忽略；  
        `x` 是一个变量，因而被转化为 `(#f . x)`；  
        `e ...` 是一个匹配表的变量，故转化为 `(... . e)`。
       
        最后生成的列表为 `((#f . x) (... . e))`

  - 2) 第二个 pattern 为 `(_ key1)`
        `key1` 是宏关键字，即常量，故直接生成 `((#t . key1))`。
     
  - 3) 最后一个 pattern 为 `(_ #f key2 1)`，
         `#f` 和 `1` 都是字面常量，而 `key2` 是宏关键字。
         我们生成的就是 `((#t . #f) (#t . key2) (#t . 1))`。
     
     如此处理了所有的 pattern 。

3. 之后我们来看对 output 的预处理


  - 1) 第一个 output 为 `(if x ((lambda () e ...)))`，
         如法炮制，生成 `((#f . if) (#t . x) (((#f . lambda) () (... . e))))`。
       
         注意到 `if` 和 `lambda` 是 scheme 默认支持的 procedure。
         是作为字面直接输出的，因而标记为 `#f` 。

  - 2) 类似的，第二个 output `'(this is (when key1))`，
         转化后为 `((#f . quote) ((#f . this) (#f . is) ((#f . when) (#f . key1))))`.
       
         其中 `'x` 在 scheme 中定义为 `(qoute x)` 的简写，这里做了还原展开。
       
       
  - 3) 最后一个 output  `(when key2)`，
       转化为 `((#f . when) (#f . key2))`。

### 3.3 匹配宏的处理 ###


对 S 表达式的每一层 `(symbol ...)`，看能否匹配某个已定义的宏调用的结构 `(macro ...)`。

>     (when (x 1)  
>       (display "hello, ")  
>       (display (when #f key2 1)))

1. 首先它匹配 `(when ...)` 宏。

2. 因此我们将 `when` 宏中各个 pattern 逐个匹配之后的语句
   - 如果匹配到某个 pattern ，就根据对应的 output 输出 (并不再继续匹配后面的 pattern )；
   - 如果没有匹配，则预处理器报错。

    显然 `((> x 1) (display "hello, ") (display (when #f key2 1)))`，能匹配第一个 pattern `(x e ...`) 。
     
    **以下是预处理器的具体过程。**

3. 试图将 `((#f . x) (... . e))` 与上述 S 表达式进行匹配。

   - 1) 首先 `(#f . x)` 匹配到 `(> x 1)`， `x` 赋值为 `'(> x 1)`；
   - 2) 然后 `(... . e)` 匹配到 `(display "hello, ") (display (when #f key2 1))`，
          `e ...` 赋值为一个列表 `[(display "hello, ") (display (when #f key2 1))]`。  

4. 然后根据 output 输出，即
   遍历 `((#f . if) (#t . x) (((#f . lambda) () (... . e))))`
   
   - 1) `(#f . if)`： `#f` 表示常量，输出字面量 `'if`
   - 2) `(#t . x)`： `#t` 表示变量，输出 `x` 的值 `'(> x 1)`
   - 3) `(#f . lambda)`： 常量，输出字面量 `'lambda`
   - 4) `(... . e)`： `...` 表示匹配表的变量，逐个输出 `e ...` 的项，即输出 
        `'(display "hello, ") '(display (when #f key2 1))`。

    我们得到输出：
    `(if (> x 1) ((lambda () (display "hello, ") (display (when #f key2 1))))`。
   
   
5. 但是还没完。`(when #f key2 1)` 又是一个宏调用，所以我们重复 1. ~ 4. 的过程。
   
    - 2nd pass: `(#f key2 1)` 匹配到 `(_ #f key2 1)`，根据 output 输出为 `(when key2)`；
    
    - 3rd pass: 这里依然有宏调用，我们进行第三次展开，得到 `(quote (this is (when key1)))`。
   
         其中 `(quote (this is (when key1)))` 的意义为 `(this is (when key1))` 的字面意义，
         因而不对其继续展开。
   
6. 终于，我们得到最终输出：

>     (if (> x 1)  
>       ((lambda ()
>         (display "hello, ")
>         (display (quote (this is (when key1)))))))


### 3.4 关键函数及其作用 ###


- `analyze` 函数：将宏定义中的 pattern 和 output 转化为 `(#t/#f/... . ???)` 形式。

- `process-pattern` 函数：用来预处理宏定义的主函数，主要就是调用了 `analyze` 做了两遍转换。

- `pattern-match` 函数：匹配宏调用主体和宏定义中 pattern 的函数，
                    首先将匹配的 pattern 中的变量做了赋值，
                    然后根据变量赋值表和 output 产生真正的输出。

- `preprocess` 函数：进行预处理的主函数，就是通过遍历的方式，根据不同情况分别调用
                 `process-pattern` 和 `pattern-match` 函数，以达到对宏定义和宏调用的预处理。


### 3.5  遇到的困难 ###


1. 一开始对于 `quote` 没处理好，没有直接将其做为 literal 输出，导致例子中的宏调用会死循环地无限展开下去。

2. 其次在 Scheme 中，可用的数据结构太少，基本上只能使用 list 。
   这样就无法轻易区分一个符号到底是常量，还是变量，还是匹配表的变量。
   此时我采用的方法就是 explicit typing，即每个符号之前都加上其类型来表示。
   具体的说就是将 `x` 变为 `(type-x . x)`，就如上所说的产生了若干 `(#t/#f/... . ???)` 形式的列表。

3. 最后是一个很微妙的细节：  
   由于 `(x ...)` 是能匹配空列表的。
   对于最初的代码，当我遇到空表的时候，我是没有对 `x ...` 做赋值的，这样就导致之后的输出出错。
   后来修正的时候，就是在遇到空表时，做特判将 `x ...` 赋一个空值。
   
这个小小的预处理器，表面看似简单，但实际真做起来的时候，会有无数的细节在等着你。由于时间和精力有限，我的预处理器只能支持常见 90% 的宏定义（当然包括常用的系统宏如 `let, begin, cond, case` 等）。而剩下的一些奇怪的语法，如 pattern 中的 improper list，预处理器是不能正常工作的。这些语法理论上是没有技术难度的，但如果要支持，则势必增加很多细节方面的工作。工作量其实并不小，故在此没有实现。


----------------

## 4. 中间代码生成 (by featheryleaf) ##


### 4.1 bytecode protocol ###


中间代码在设计时需要兼顾中间代码生成和解释执行两方面的需求。  
在介绍中间代码生成之前，先介绍中间代码的语义：

1.  `nop`： 无操作  
    为兼容性考虑加入的，在实际生成中没有用到。

2.  `push %var`： 将变量 `%var` (的指针)压栈  
    用于变量赋值和参数传递。  
    解释器采用栈式模型，在一个全局栈中进行参数和返回值的传递。
    `%var` 的具体形式见后。
   
3.  `pop  %var`： 将栈顶元素弹栈，并赋值给 `%var`  
    用于变量赋值和清理无用返回值。
   
    >     (define f 1)  
    >      
    >     push 1  
    >     pop  f  
   
4.  `call n`： 调用栈顶元素，n 个参数  
    这里栈顶必须是一个过程，否则执行期报错。
    将调用的过程也压栈，而非 `call %proc n` 同样考虑到其他情况，详见后。
   
    >     (f x)
    >      
    >     push x  
    >     push f  
    >     call 1  

5.  `ret`： 过程返回  
    每个过程都要保证在栈顶存留一个返回值，这一点由代码生成保证。
   
    >     (define f (lambda () 1))
    >      
    >     push 1  
    >     ret
   
6.  `jump %cond offset`： 条件跳转  
    - 若 `%cond` 为真，则 `next-pc = pc + 1 + offset`；  
    - 否则 `next-pc = pc + 1`。  
    
    其中 `%cond` 有 `always, true, false, never` 四种可能
    - 如果 `%cond == always`，无条件为真；  
    - 如果 `%cond == never`，相当于 `nop`；  
    - 如果 `%cond == true || %cond == false`，则需要测试栈顶元素，根据栈顶元素的 `bool` 值判断真假。  
    实际生成时只用到了 `false` 和 `always`。  


    >     (if a b c)  
    >               
    >     push a  
    >     jump false  2  
    >     push b  
    >     jump always 1  
    >     push c  

7.  `tcal`： n
    尾递归调用栈顶元素，不生长 continuation Chain ，其他同 call n，详见解释执行阶段。
   
    >     (lambda () (f) (g))  
    >     
    >     push f  
    >     call 0     // f 是正常调用  
    >     pop  null  // 将 f 过程返回值弹出  
    >     push g  
    >     tcal 1     // g 是尾调用，将复用匿名过程的 continuation  
    >     ret        // 由于 continuation 被复用，这条语句实际上不会被执行到  
   
8.  `ncal %native-proc n`： 调用内置过程， n 个参数  
    直接解释执行，不影响 continuation Chain 。
     
    >     (+ 1 2)  
    >      
    >     push 2  
    >     push 1  
    >     ncal + 2   // 将栈上 1 和 2 弹出，计算后压入返回值 3  
   
9.  `alloc n`： 声明该过程有 n 个局部变量  
    提示解释执行模块开辟适当内存。
    - 若不考虑优化，`alloc n`一定为每个过程的第一条语句；  
    - 但 `alloc 0` 可能被优化掉。  
  
  
10. `popn n`： 将栈顶 n 个元素弹出，依次赋值到前 n 个局部变量  
    用于参数传递。  
    “依次”的具体含义取决于参数传递的协议。当前的协议是从右向左压栈。

11. `popl %var`： 将剩余的参数作为一个表赋值到 `%var` 变量  
    用于参数传递。  
    剩余参数的个数，由该过程被调用时 `call n` 或 `tcal n` 以及其之前的 `popn m` 相减后得到。  
    每个过程一定以 `alloc n` 开始，之后为 1-2 条 `popn` 或/和 `popl` 指令。  
    
    >     (lambda (x y) (+ x y))
    >      
    >     alloc 2  
    >     popn  2    // 将 2 个参数赋值到 x, y  
    >     push  y  
    >     push  x  
    >     ncal  + 2  // + 的返回值直接作为返回值返回  
    >     ret 
    >      
    > -----------  
    >      
    >     (lambda (x . y) (define z (cons x y)))
    >      
    >     alloc 3    // 注意这里为 z 预留了位置  
    >     popn  1    // 将第一个参数赋值到 x  
    >     popl  y    // 将剩余的参数赋值到 y  
    >     push  y  
    >     push  x  
    >     ncal  cons 2  
    >     pop   z    // 将 cons 的返回值赋值到 z  
    >     push  z    // 压入返回值 (注意此时栈上是空的)  
    >     ret


### 4.2 变量类型 ###

0. `null`： 空类型  
   `push null` 意为压入空值；`pop null` 意为直接弹出栈顶，不予赋值。

    >     (define x 1)  
    >     (define y 2)  
    >     
    >     push imm   1  
    >     pop  local (0, 0)  
    >     push null         // 这里压入 (define x 1) 的返回值  
    >     pop  null         // 在 (define y 2) 开始前，要清掉栈上无用的返回值  
    >                       // 这时在不作优化时生成的代码，实际上以上两条语句可以被优化掉  
    >     push imm   2  
    >     pop  local (0, 1)  
    >     push null         // 同样的，压入 (define y 2) 的返回值  

1. `imm n`： 立即数 n  
   只使用立即数表示整数。

    >     (+ 1 2)  
    >      
    >     push imm 2  // 这是立即数 2 的真实表示  
    >     push imm 1  
    >     ncal +   2  

2. `global n`： 全局常量第 n 项  
   使用一个全局变量表，保存所有非整数常量(字符串，匿名过程等)，通过下标进行引用。  
   (整个 bytecode list 作为匿名过程保存在 `global[0]`)

    >     ((lambda () #t))  
    >     
    >     global-table = [global-lambda, unnamed-lambda, #t]  
    >     
    >     [global-lambda]  
    >     push global 1  // 压入 global[1] = unnamed-lambda  
    >     tcal 0         // 调用 unnamed-lambda  
    >     ret  
    >     
    >     [unnamed-lambda]  
    >     push global 2  // 压入 global[2] = `#t`  
    >     ret  

3. `local (n, m)`： n 阶父过程的局部变量第 m 项
   scheme 中过程允许嵌套定义，子过程可以访问父进程的局部变量。
   因此局部变量访问时，通过二维索引进行引用，第一维为嵌套的深度，第二维为在局部变量表的下标。
   在实现时，通过引用链查找 n 次父过程，然后查找该过程局部变量表的第 m 项。  
   
    >     (define y 2)  
    >     (define (f x) (+ x y))  
    >     (f 1)  
    >     
    >     global-table                = [global-lambda, lambda-f, #t]  
    >     local-table @ global-lambda = [y, f]  
    >     local-table @ lambda-f      = [x]  
    >     
    >     [global-lambda]  
    >     push imm 2  
    >     pop  local (0, 0)           // 弹出到 y  
    >     push global 1               // 压入 lambda-f  
    >     pop  local (0, 1)           // 弹出到 f
    >     push imm 1
    >     push local (0, 1)  
    >     tcal 0                      // 以参数 1 ，调用 f  
    >     ret  
    >     
    >     [lambda-f]  
    >     alloc 1  
    >     popn  1                     // 弹出到 y  
    >     push  local (1, 0)          // 压入 y (注意，这里 y 在 f 的父进程中，所以索引是 (1, ?) )  
    >     push  local (0, 0)          // 压入 x  
    >     ncal  +     2  
    >     ret  

4. `special cc`： 特殊变量 current-continuation  
   用于 `call/cc` 过程，详见 **5. 解释执行** 部分。


### 4.3 词法分析 ###

1. 分词

    Python 内置了对正则表达式的支持，这里使用正则表达式，将经过预处理的 Scheme 代码进行分词，转化为字符串流。
    
    以下为解释器使用的正则文法：

    >     (?P<blank>     \s+)|                        # blank characters
    >     (?P<comment>   ;.*)|                        # comment
    >     (?P<separator> [\(\)\[\]\{\}\|]|\#\()|      # separator  ( ) [ ] { } | #(
    >     (?P<bool>      \#t|\#f)|                    # boolean
    >     (?P<char>      \#\\(?:space|newline|\S))|   # char  #\a #\Z #\space #\newline
    >     (?P<string>"   (?:\\\\|\\\"|[^"\\])*")|     # string
    >     (?P<number>    [+-]?\d*\.?\d+)|             # number
    >     (?P<symbol>    [\w!$%&*+-./:<=>?@^_~]+)|    # symbol
    >     (?:            .+)                          # invalid pattern

2. 符号流

    分词的结果是一个字符串流，之后需要将每个字符串转化为一个对应类型的 Token。
    
    判断字符串类型的方法，通常是根据开头的一个或几个字符，这个逻辑可以根据上面的正则表达式方便地提取。

3. 语法树

    Scheme 使用一对括号对语句进行界定，因此可以在词法分析阶段，构造括号嵌套的树状结构，树的每个节点是一个符号或一棵子树。


词法分析过程示例：

>        (+ 1 (if #t x .2)) "string"  
>     
>     => ['(', ' ', '+', ' ', '1', '(', 'if', ' ', '#t', ' ', 'x', ' ', '.2', ')', ')', ' ', '"string"']  
>     
>     => ['(', Symbol('+'), 1, '(', Symbol('if'), True, Symbol('x'), 0.2, ')', ')', 'string'']  
>     
>     => [Symbol('begin'),   
>           [Symbol('+'), 1,   
>              [Symbol('if'), True, Symbol('x'), 1.2]],  
>           'string']


### 4.4 语法分析 ###

语法分析需要维护一个局部变量表栈(栈顶为在当前过程定义的局部变量)，一个中间代码表，一个全局常量表。其中中间代码表和全局常量表需要交给执行模块进行解释执行。

语法分析为递归地解析语法树的每个节点，当节点不是叶结点时，根据其第一个子节点的类型分别处理。

1. `generate` 函数  
   语法分析的入口，以及每次递归调用的入口函数。
   解析语法树的一个节点，根据节点的类型，分别调用以下函数中的一个。  

   - 当节点是一个元素时，调用 `gen_atom` 函数，生成形如 `push %var` 的中间代码；  
   - 当节点是一个表时，如果第一个元素是 `define`, `lambda`, `if`, `quote` 中的一个，则分别调用对应的函数；  
   - 当节点时一个表，且第一个元素不是以上 symbol 时，调用 `gen_proc`函数。

2. `(%proc %args ...)`  
    生成形如 `push %proc; push %arg-n; ...; push %arg-0; call/tcal n` 的中间代码，参数传递采用从后向前依次压栈。

    有些情况需要特殊处理：
    - `(begin %exp1 %exp2 ...)` => `push %exp1; pop null; push %exp2; ...`  
    即除了最后一个过程的过程的返回值需要显示弹出。

    - `(call/cc %proc)` => `push special cc; push %proc; call/tcal 1`  
    `call/cc` 过程意为以 `current-continuation` 为参数调用 `%proc` 过程，这里在转化为中间代码时还原了本意。`continuation` 和 `call/cc` 的含义见解释执行模块或 scheme 文档。

3. `(define %var %value)`  
   生成形如 `push %value; pop %var; push %null` 的中间代码，并将 `%var` 加入当前局部变量表栈顶。  
   其中 push 和 pop 联合使用完成赋值，最后的 `push null` 作为该过程的返回值

    >     (define f 1)  
    >      
    >     push imm   1  
    >     pop  local (0, 0)  
    >     push null  

4. `(lambda (%args ...) %procs ...)`  
   生成一个匿名过程，该过程被加入全局常量表中，该匿名过程的中间代码通过 `(begin %procs ...)` 生成。

    >     (lambda (x y) (+ x y))
    >      
    >     alloc 2  
    >     popn  2  
    >     push  local (0, 1)  // push y  
    >     push  local (0, 0)  // push x  
    >     ncal  +     2  
    >     ret  

5. `(if %cond %true-exps %false-exps)`  
   生成形如 `push %cond; jump false %off1; %true-exps; jump always %off2; %false-exps` 的中间代码，其中 `%off1` 和 `%off2` 在递归调用 `generate` 函数时由父函数完成填充。

    >     (if (+ 1 2) (+ 3 4) (+ 5 6))  
    >               
    >     push imm    2  
    >     push imm    1  
    >     ncal +      2  
    >     jump false  4  // goto false-block
    >     // true-block  
    >     push imm    4  
    >     push imm    3  
    >     ncal +      2  
    >     jump always 3  // goto end
    >     // false-block
    >     push imm    6  
    >     push imm    5  
    >     ncal +      2  
    >     // end         

6. `(quote %symbol)`  
   将 `%symbol` 视为一个常量，加入全局常量表中，生成形如 `push %symbol` 的中间代码。

    >     (quote (a b))  
    >  
    >     global-table = [global-lambda, '(a b)]
    >     
    >     push global 1  // push '(a b)  


----------------

## 5. 解释执行 (by wky) ##

### 5.1 数据结构 ###

1. 启动代码：Global.bytecode_list

2. 常量表：
   抽象过程 procedure：字节码序列，标号，父过程
   常量数据：字符串，Pair

3. 运行栈：传递参数和返回值

4. continuation:  

    有四种状态：  

    - 处于胚胎状态的（被调用时附加调用链，进入运行状态）
    - 处于运行状态的
    - 处于即将被捕获状态的（`push cc`得到）
    - 处于已经被捕获状态的（`call/cc`中函数的 `popn` 时刻，浅拷贝栈顶的 `continuation`）
     
    其中保存的数据：  

    - 对应的 procedure  
    - 当前 pc  
    - 返回地址 pc  
    - 局部变量  
    - 调用链  
    - 引用链  

### 5.2 基本的函数调用 ###

>     (define (f x) ...)
>     (f 2)
>      
>     push global f
>     pop  local  (0, 0)
>     push imm    2
>     push local  (0, 0)
>     call 1

1. `push global f`     ： 用 `f` 对应的 `Procedue` 填充一个新的胚胎状态 `continuation`  
2. `pop  local  (0, 0)`： 将栈顶的 `continuation` 绑定到局部变量 0 上  
3. `push imm    2`  
4. `push local  (0, 0)`： 在栈顶上创建一个对局部变量 0 的拷贝  
5. `call 1`            ： 函数调用，具体行为见下面描述  


### 5.3 高阶函数 ###

**5.3.1. 以函数为参数**

>     (define (f g x)
>       (+ (g x) (g x)))
>     (f (lambda (x) (* x x)) 10))
> 
>     [lambda global]
>     push global f
>     pop  local  (0, 0)
>     push imm    10
>     push global lambda0
>     push local  (0, 0)
> 
>     [lambda f]
>     alloc 2
>     popn 2
>     push local 0,0(x)
>     push local 0,1(g)
>     call 1
>     push local 0,0(x)
>     push local 0,1(g)
>     call 1
>     ncal +  2

可见， `f` 中会多次操作 `g` ，所以 `g` 在 `f` 中必须作为胚胎的 `continuation` 存在

**5.3.2. 以函数为返回值**

`(define (f x) ((define (g y) (+ x y)) g))`  
如何从 `f` 里面抛出 `g` ，并保存 `f` 的局部变量？
 `g` 在 `f` 的 `continuation` 中（`local_vars` 中），是一个胚胎的 `continuation` ，引用链指向 `f` （具体做法的 `push global *`）
抛出就是将 `g` 的胚胎 `continuation` 的引用写在栈顶，由于有引用链的存在， `f` 的 `continuation` 不会被 GC 。
`g` 中对 `x` 的引用是通过 `push local (1,0)` 做到的，即沿着引用链向前1次

为了实现第一等函数，我们使用了胚胎状态的 `continuation`

### 5.4 call/cc如何发生 ###

**5.4.1. 以 continuation 为参数**

>     (+ 1 (call/cc (lambda(cc) (...))))
>     
>     push 1
>     push cc
>     push global lambda0
>     call 1
>     ncal + 2

这里 `push cc` 的时候，栈上已经有元素了，所以 `cc.stack` 要做 `stack` 的浅拷贝
 `call` 进入 `lambda` 后，有 `popn 1` 的操作，这时的 `cc` 是 `lambda` 的 `continuation`，而栈顶的是 `cc.prev_cont`，
稍后还会返回到 `cc.prev_cont.cur_pc` (从 `lambda` 或者从后面的 `(cc 2)` )


**5.4.2. 调用一个被捕获的 continuation**

>     (cc 2)
>     
>     push 2
>     push local cc
>     call/tcal 1

当 `call` 一个被捕获的 `continuation` 时，直接抛弃当前的 `cc`，替换成栈顶的一个浅拷贝（拷贝是为了能够多次 `call` 同一个）
然后抛弃栈，替换为 `cc.stack`（这里的 `cc` 是已经替换过的），然后`cc.stack=None`，让当前 `cc` 进入运行状态
替换之后的 `cc` 里面存的 `cur_pc` 的位置是下面例子中第二个 `call 1` 的后面，恢复的栈的是`push cc` 之前的栈


**5.4.3. 另一个例子**

>     (call/cc (f x))
>     push cc <------这这时复制栈，是正确的
>     push x  <------ push cc的时候，cur_pc在这里
>     push F  <------这里Push到栈上的不应该被保存
>     call 1
>     call 1
>     ...     <------ 其实我们希望cc被恢复的时候，返回到这个地方（cur_pc）
>                     只有进入了(f x)返回的函数里面，才会走到这里，所以 popn 的时候才作浅拷贝


**5.4.4 具体做法**

1. `push global *(procedure)` 时：  
    需要将抽象过程转化为拥有引用链的具象过程（胚胎状态）。  
    从 `cc` 起，沿着引用链向上观察各 `continuation `，直到其过程与父过程对应，这时的 `continuation` 就是该胚胎 `continuation` 的引用链

    *为什么在这里把 `procedure` 换成了 `continuation` ?*  
    因为 `procedure` 不知道动态的语义范围（没有引用链）。
  

2. `push cc` 时：  
   对 `cc` 不做复制，栈顶元素直接指向 `cc` ，但是 `cc.stack` 赋值为 `stack` 的浅拷贝。

3. `call` 的时候：  

  - 如果栈顶是胚胎状态的 `continuation` ，即函数调用：  
    浅拷贝栈顶，接上调用链，成为新的 `cc` ，记录当前地址为返回地址，更新字节码序列，记录参数个数（类似于胚胎干细胞的分裂+分化）。  

        *这里为什么要浅拷贝（分裂）而不是直接用（分化）？*  
        胚胎干细胞以后还要用，所以先分裂（分化之后就不能分裂了）

  - 如果栈顶是已经被捕获状态的 `continuation` ：  
      - 取出栈顶的参数，将当前栈直接替换为被调用的 `continuation` 所保存的 `stack` 的浅拷贝，最后在新的栈上放回参数。  
      - 而 `cc` 直接替换为被调用的 `continuation` 的浅拷贝，并不再处于"已经被捕获状态"，而是运行状态。
      - 恢复字节码序列和 `pc` 。

        *为什么是被调用的 `continuation` 所保存的 `stack` 的浅拷贝？*  
        因为，这个 `continuation` 还有可能再一次被调用，为了保存栈，所以要浅拷贝。  

        *为什么不是深拷贝？*  
        深拷贝可能会递归的拷贝，一般而言我们的运行时数据结构有很多环。
        另外，具体的数据还是要变的：  

         >   (define x 0)  
         >   (define c 0)  
         >   (display (call/cc (lambda (cc) (set! c cc))))  
         >   (set! x (+ x 1))  
         >   (c x)  
       
        如果深拷贝，那每次打出来的 `x` 都是 `0` 。
        实际情况是打印 `0，1，2，3，4，5` 。

4. `tcal` 的时候：
    - 如果栈顶是胚胎状态的 `continuation` ，即函数调用：  
      胚胎干细胞先分裂，然后分化的时候，调用链接的是 `cc` 的上一个，返回地址写成 `cc` 的返回地址。
      其他工作于 `call` 相同
    - 如果栈顶是已经被捕获状态的 `continuation` ：  
      与 `call` 完全相同。  

        *为什么 `tcal` 没有不同？*  
        因为调用一个捕获了的 `continuation` 就是抛弃当前的状态

5. `popn 1` 而且栈顶是捕获的 `continuation` 时：  
  这时就是 `call/cc` 中的函数被调用的时刻, 栈顶是 `cc->prev_cont` （即调用链的上一个）。  
  为了保存状态，需要用栈顶的浅拷贝替换栈顶（随后被 `pop` 进入局部变量，成为已经被捕获状态）。  
  代码中还有 `stack[-1].stack = None` 是标记栈顶 `continuation` 不再处于"即将被捕获状态"，恢复为运行状态。  


***附：浅拷贝是什么？***

设有 `A` 类的实例 `a {x, y, z}` ， `x, y, z` 分别指向其他对象。  
做 `a` 的浅拷贝 `b` ，则 `b{x, y, z}` 中的 `x, y, z` 也指向 `a{x, y, z}` 指向的对象。  
但是当修改 `b.x` 为另一对象时，`a.x` 仍指向原来的对象
