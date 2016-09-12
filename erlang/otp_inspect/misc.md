Erlang　虚拟机探索
====================

1. erlc最终会调用erl(这个得做个实验, 看看它以什么参数传递进去的，　以及在erl中哪部分代码对这些参数做了处理)
2. erl_init.c中的have_break_handle用来指示是否需要安装处理SIGINT的信号处理器，　默认是安装的，　实验方法，
　　在信号处理函数break_requested中有语句fprintf(stderr, "break\n") 需要打开调试开关, 方法，　在运行./configure
    后在生成的Makefile中在CC变量后添加-DDEBUG(不其作用呀)

3. 在linux系统下，　Erlang虚拟机的入口地址为 erlexec.c 中的main, 因为erl命令是个脚本文件，　最终会执行erlexec中
　　的main, 可以在该方法中加入printf语句进行测试. 该函数最终会执行beam.smp, 其对应入口在erl_main.c中的main, 这个函数
　　就更简单了，　只是简单地调用erl_init中的erl_start


4. erl_start的任务是执行初始化动作, 加载预定义模块, 创建第一个进程(模仿linux?), 最后开启调度器

5. 能否从已经开始工作的erlang虚拟机为切入点对虚拟机进行探索．
  * erl最后将向用户输入提示符, 这个提示符号在哪里呢？     
        使用grep "abort with ^G" 可以找到唯一的erlang代码文件(stdlib/src/shell.erl) ... 这是erlang代码了呀．．．，　不太好玩

---------------------------吐曹-----------------------------
本来以为在iex下能看到erlang的文档，　好吧，　是我太天真了．．．能不能扩展一下h函数，　让它去读取man手册，　或者erlang源码
目前还没这个能力，　搁置．．．．
代码导航．．．．，　怎么导航呢？　怎么去找定义呢？
