# postgres设置远程登录
  
  1. 修改pg_hba.conf　新增一行　host all all 0.0.0.0/0 password
  　　　
	  　　參數1:host表遠端存取，local表本機端存取
	    參數2:設定可存取的Database
	    參數3:設定可存取的使用者
	    參數4:設定可存取之網域，此設定全部網域皆可存取
	    參數5:trust表不需認證，password表示需要密碼
　　2. 修改postgres.conf
　　       listen_addresses ='*'
          port=5432
  3. 重启: 
    /opt/postgresql/9.5.4/bin/pg_ctl restart -D /opt/postgresql/9.5.4/data
　　4. psql -h localhost 登录后　\password postgres 设置密码
　　5. 在其他机器上psql -h IP -U postgres 进行登录

# 创建新用户并为其创建配套数据库

　　1. CREATE USER dbuser WITH PASSWORD 'password';
　　2. CREATE DATABASE exampledb OWNER dbuser;
　　3. GRANT ALL PRIVILEGES ON DATABASE exampledb to dbuser;

# 常用命令
  \l 列出可用数据库
  \du 列出用户
  \dt 列出当前数据库中的表

#　删除schema下的所有表:

  1. drop schema public cascade;
  2. create schema public;
  3. grant all on schema public to ejabber;


