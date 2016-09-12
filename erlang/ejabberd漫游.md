# 　缘起
     
     初识Erlang，就是因为ejabberd服务器. 当时刚毕业让那家公司要做视频会议系统, 项目经理让
     我们研究当时流行的xmpp服务器, 后来主要选择不ejabberd和openfire. 因为众所周知的原因，
     最终选择了openfire.　但它却让我第一次认识了Erlang这种语言.

     关于这门语言，毁誉参半，　却从来没有流行过。　大家对它的神秘充满了好奇，却又恐惧于它的怪异
     少有人尝试。
　
#  第一阶段　把环境跑起来

## 安装ejabberd

    ./configure --prefix=/opt/ejabberd && make && sudo make install

  安装好后接下来要干什么呢，我们的目标是把服务器跑起来，　添加两个用户a和b, 并让它们
能够登陆服务器互相聊天.

# 杂记

## ejabberdctl 由ejabberdctl.template生成

## ejabberd.yml　由ejabberd.yml.example生成

## 配置ejabberd使用postgres

   默认是明文存储密码的呀, 这个怎么改啊
## 如果是数据库的话, 离线消息默认存储在spool表中.
  



  


#  第二阶段　xmpp热身
#  第三阶段　ejabberd系统架构
#  第四阶段 Erlang大闯关
#  第五阶段　源码导读
#  第六阶段　系统扩展
#  第七阶段　系统评估