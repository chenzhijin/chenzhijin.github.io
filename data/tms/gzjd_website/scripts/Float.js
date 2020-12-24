/*Float漂浮效果
接受参数:无
返回类型:无

应用技术：
javascript

制作人：
黄若儒 Roy.Huang

注意:
1、全部采用静态/prototype写法，方便继承重载
2、请注意漂浮效果用得太多对性能的影响
3、通过IE\FireFox\Opera浏览器测试
4、现支持碰撞特效，但必须先导入FloatCollision.js和GetPosition.js
*/
var Float=function(){};

try{//设置命名空间
	Class.setNameSpace("com.gzgi.ad.Float",Float);
}
catch(e){}

Float.prototype.target=null;//飘动对象
Float.prototype.speed=1;//漂浮速度
Float.prototype.frame=1;//漂浮贞度
Float.prototype.type=0;//漂浮类型:0为全页,1为全屏
Float.prototype.interval=null;//定时执行方法
Float.prototype.UD="DOWN";//漂浮方向
Float.prototype.LR="RIGHT";//漂浮方向
Float.prototype.randomInit=true;//随机初始化

Float.prototype.init=function(){//程序实例化
	Float.setDefault(this);
	if(this.randomInit)
		Float.randomSet(this);
};

Float.prototype.start=function(){//开始漂浮
	var base=this;
	base.stop();
	base.interval=setInterval(function(){
		Float.doFloat(base);
		try{
			Float.doCollision(base);
		}
		catch(e){}
	},base.speed);
};

Float.prototype.stop=function(){//停止漂浮
	clearInterval(this.interval);
};

Float.prototype.open=function(){//打开漂浮对象
	Float.$(this.target).style.display="block";
};

Float.prototype.close=function(){//关闭漂浮对象
	Float.$(this.target).style.display="none";
};

Float.setDefault=function(float){//设置目标属性
	var t=this.$(float.target);
	if(!t)
		return this.onError(0);
	t.style.position="absolute";
	if(t.style.zIndex==0)
		t.style.zIndex=1;
	if(!t.style.top || t.style.top=="")
		t.style.top=0;
	if(!t.style.left || t.style.left=="")
		t.style.left=0;
	return true;
};

Float.randomSet=function(float){//随机设置元素
	var LR=["LEFT","RIGHT"];
	var UD=["UP","DOWN"];
	float.LR=LR[parseInt(Math.random()*2)];
	float.UD=UD[parseInt(Math.random()*2)];
	var t=this.$(float.target);
	if(document.compatMode && document.compatMode!="BackCompat"){
		t.style.top=parseInt(Math.random()*(document.documentElement.clientHeight-t.offsetHeight))+1+"px";
		t.style.left=parseInt(Math.random()*(document.documentElement.clientWidth-t.offsetWidth))+1+"px";
	}
	else{
		t.style.top=parseInt(Math.random()*(document.body.clientHeight-t.offsetHeight))+1+"px";
		t.style.left=parseInt(Math.random()*(document.body.clientWidth-t.offsetWidth))+1+"px";
	}
};

Float.doFloat=function(float){//执行漂浮操作
	var t=this.$(float.target);
	var top=parseInt(t.style.top);
	var left=parseInt(t.style.left);
	var tW=t.offsetWidth;
	var tH=t.offsetHeight;
	var cW,cH,sW,sH;
	if(float.type==0){
			sW=0;
			sH=0;
			cW=document.body.scrollWidth;
			cH=document.body.scrollHeight;
	}
	else{
		if(document.compatMode && document.compatMode!="BackCompat"){
			sW=document.documentElement.scrollLeft;
			sH=document.documentElement.scrollTop;
			cW=document.documentElement.clientWidth+sW;
			cH=document.documentElement.clientHeight+sH;
		}
		else{
			sW=document.body.scrollLeft;
			sH=document.body.scrollTop;
			cW=document.body.clientWidth+sW;
			cH=document.body.clientHeight+sH;
		}
	}
	if(float.UD.toUpperCase()=="DOWN"){
		if(top+t.offsetHeight+float.frame<cH)
			top+=float.frame;
		else{
			top=cH-t.offsetHeight;
			float.UD="UP";
		}
	}
	else{
		if(top-float.frame>sH)
			top-=float.frame;
		else{
			top=sH;
			float.UD="DOWN";
		}
	}
	if(float.LR.toUpperCase()=="RIGHT"){
		if(left+t.offsetWidth+float.frame<cW)
			left+=float.frame;
		else{
			left=cW-t.offsetWidth;
			float.LR="LEFT";
		}
	}
	else{
		if(left-float.frame>sW)
			left-=float.frame;
		else{
			left=sW;
			float.LR="RIGHT";
		}
	}
	t.style.top=top+"px";
	t.style.left=left+"px";
};

Float.addCollision=function(i,f){//添加碰撞队列
	try{
		this.floats.push({group:i,float:f});
		f.group=i;
	}
	catch(e){
		return this.onError(1);
	}
};

Float.$=function(t){//获取指定对象
	if(typeof(t)!="object")
		t=document.getElementById(t);
	return t;
};

Float.onError=function(e){//错误处理
	switch(e){
		case 0:alert("target属性指向对象不存在!");break;
		case 1:alert("装载碰撞效果驱动失败，请按Float.js-GetPosition.js-FloatCollision.js的顺序加载包");break;
		default:alert(e);
	}
	return false;
}