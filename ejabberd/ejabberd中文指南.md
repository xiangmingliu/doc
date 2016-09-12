# 环境安装

本章讲述从源码开始安装配置ejabberd开发环境，主要需要安装erlang环境,　安装数据库postgres
以及ejabberd集群. 为了测试集群效果，　使用软负载均衡haproxy. 

使用ubuntu以及在虚拟机中的两台centos

## erlang环境安装

* 从github上下载erlang源码 git clone https://github.com/erlang/otp.git
* 切换到想要安装的分支, 这里我们选择18.2 :  git checkout OTP-18.2
* 执行./otp_build setup 进行源码的初始配置(主要是生成configure脚本)
* 执行./configure --prefix=/opt/erlang 并根提示安装缺少的依赖
* 执行make 
* sudo make install 将erlang安装到目标地址中.
* 在终端运行erl命令, 成功则表明安装成功

## 安装postgres数据库　(到时候配置ejabberd使用它, 默认使用mnesia不太方便观察)

  * 下载postgres源码, 这里使用的版本是9.5.4
  * 进入源码目录执行 ./configure --prefix=/opt/postgres
  * 执行命令 make && sudo make install将postgres安装到目标位置
  * 创建postgres用户 adduser postgres
  * 创建data目录:   mkdir -p /opt/postgres/data
  * 修改data目录的属主: chown -R postgres /opt/postgres/data
  * 切换到postgres用户 su - postgres
  * /opt/postgres/bin/initdb -D /opt/postgres/data
  * 开启postgres允许远程登录:
      (以下两步需要管理员权限的用户来执行)    
    * /opt/postgresql/data/pg_hba.conf中增加一行: host all all 0.0.0.0/0 password
    * 修改/opt/postgresql/data/postgres.conf使 
          listen= '*' port=5432 (也可以指定其它端口)
  * 启动: /opt/postgresql/bin/postgres -D /usr/local/pgsql/data >logfile 2>&1 &
  *　登录: /opt/postgresql/bin/psql 
  *　创建数据库和用户:
        CREATE USER ejabberd WITH PASSWORD 'ejabberd'; 
        CERATE DATABASE ejabberd OWNER 'ejabberd';
  * 执行ejabberd源码包中的pg.sql创建相应的表 \i pg.sql

至此, Erlang环境和数据库环境已经搭建完毕.


