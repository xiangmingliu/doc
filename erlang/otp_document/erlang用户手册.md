# 1. 介绍

## 1.1 目的

Erlang用户手册的焦点是语言本身, 而不是介绍语言的实现.  我们会有文本和例子来介绍语言的构造而
不是形式化的描述它. 这样可读性更好. 不能把本手册做为教程来使用.

有关Erlang的实现可以在下列文档中找到:
  
  * 系统原则
  　　开始和停止, 启动脚本, 代码加载, 错误日志, 创建目标系统
  * 效率指南
    内存消耗, 系统限制
  * ERTS用户手册
    Crashdump, 驱动

## 1.2 前提条件

阅读本文档需要有一些编程背景, 并对变成语言语法(说话表达的方法)有一些了解.

## 1.3 书写惯例

## 1.4 全部BIF

全部的bif以及它的参数和返回值的解释可以在man手册中查找.

## 1.5 保留字

以下这些是Erlang中的保留字:

    after and andalso band begin bnot bor bsl bsr bxor case catch cond div end fun if let not of or orelse receive rem try when xor


# 2. 字符集和源文件编码

# 3. 数据类型

## 3.1 Term

不管是什么类型的数据，　都可以称为term.

## 3.2 数字

有两种类型的数字字面量, 整型和浮点型. 除了正常的表示法外, 还有两种特殊的表示方法:

* $char - ASCII或者unicode的码点
* base#value - base进程的整数值, base在2..36之间.

<b>例子:</b>

    1> 42.
    42
    2> $A.
    65
    3> $\n.
    10
    4> 2#101.
    5
    5> 16#1f.
    31
    6> 2.3.
    2.3
    7> 2.3e3.
    2.3e3
    8> 2.3e-3.
    0.0023

## 3.3 原子

原子是一种字面量常量, 如果原子不以小写字母开头或包含特殊字符, 则必须用单引号括起来

<b>例子:</b>

    hello
    phone_number
    'Monday'
    'phone number'

## 3.4 位串和二进制

位串的处理对象是一块无类型的内存.

位串使用bit语法来表达

如果位串中的位数能被８整除, 我们就称之为binaries

<b>例子:</b>

    1> <<10,20>>.
    <<10,20>>
    2> <<"ABC">>.
    <<"ABC">>
    1> <<1:1,0:1>>.
    <<2:2>>

## 3.5 引用

引用由make_ref/0创建，　在整个虚拟机运行时系统中保持唯一.

## 3.6 Fun

fun 是一个函数对象. 使用函数对象我们可以创建匿名函数, 同时还能将函数本身传递给其它函数，
而不是仅仅传递函数名.

<b>例子:</b>

    1> Fun1 = fun (X) -> X+1 end.
    #Fun<erl_eval.6.39074546>
    2> Fun1(2).
    3


## 3.7 端口标志符

端口标志符有来标志一个Erlang端口.

用来创建端口的open_port/2的返回值就是这种类型

## 3.8 Pid

进程标志符, 标志一个进程.

下面这些有来创建一个进程的内置函数的返回值就是这种类型:

  * spawn/1,2,3,4
  * spawn_link/1,2,3,4
  * spawn_opt/4

<b>例子:</b>

    1> spawn(m, f, []).
    <0.51.0>

在下面的例子中, self()返回调用进程的pid:

    -module(m).
    -export([loop/0]).

    loop() ->
        receive
            who_are_you ->
                io:format("I am ~p~n", [self()]),
                loop()
        end.

    1> P = spawn(m, loop, []).
    <0.58.0>
    2> P ! who_are_you.
    I am <0.58.0>
    who_are_you

## 3.9 元组

元组是元素个数固定的复合数据类型, 有很多内置函数可以操作原组:

    1> P = {adam,24,{july,29}}.
    {adam,24,{july,29}}
    2> element(1,P).
    adam
    3> element(3,P).
    {july,29}
    4> P2 = setelement(2,P,25).
    {adam,25,{july,29}}
    5> tuple_size(P).
    3
    6> tuple_size({}).
    0

## 3.10　映射

映射是一种元素个数不确定的复合数据类型, 其中的每个元素都是键值对, 有很多操作map的内置函数:

<b>例子:</b>

    1> M1 = #{name=>adam,age=>24,date=>{july,29}}.
    #{age => 24,date => {july,29},name => adam}
    2> maps:get(name,M1).
    adam
    3> maps:get(date,M1).
    {july,29}
    4> M2 = maps:update(age,25,M1).
    #{age => 25,date => {july,29},name => adam}
    5> map_size(M).
    3
    6> map_size(#{}).
    0

## 3.11 列表

## 3.12 字符串

## 3.13 记录

## 3.14 布尔变量

## 3.15 转义序列

## 3.16 类型转换

# ４. 模式匹配