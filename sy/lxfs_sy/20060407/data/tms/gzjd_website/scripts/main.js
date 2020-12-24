// JavaScript Document

$(document).ready(function(e) {	
	// 根据窗口宽度判断是否引入针对1003px宽度的样式文件
	var winWidth = $(window).width();
	var targetLink = $("#theme_for1003px");
	if(winWidth < 1280 && targetLink.attr("href") == "") {
		targetLink.attr({"href":"/data/tms/gzjd_website/styles/theme_1003.css", "url":"/data/tms/gzjd_website/styles/theme_1003.css"});
	} else if(winWidth >= 1280 && targetLink.attr("href") != "") {
		targetLink.attr("href", "");
	}
	
	$(window).resize(function(e) {
		winWidth = $(window).width();
		if(winWidth < 1280 && targetLink.attr("href") == "")
			targetLink.attr({"href":"/data/tms/gzjd_website/styles/theme_1003.css", "url":"/data/tms/gzjd_website/styles/theme_1003.css"});
		else if(winWidth >= 1280 && targetLink.attr("href") != "")
			targetLink.attr("href", "");
		if(winWidth < 1003){
			$("header").width("1003px");
			$("footer").width("1003px");
			$(".top_tools").width("1003px");
		}else{
			$("header").width("100%");
			$("footer").width("100%");
			$(".top_tools").width("100%");
		}
		
		// 判断友情链接显示4列还是5列
		var yqljWidth = parseInt($(".index_bottom_container .main_part .right_part").width());
		if(yqljWidth < 965) {
			$(".index_bottom_container .main_part .right_part_main table td").width("23%");
		} else {
			$(".index_bottom_container .main_part .right_part_main table td").width("17%");
		}
	});	
	
	// 导航菜单的一级菜单选择
	var onIndex = $(".first_menu .on").index();
	$(".first_menu a").mouseover(function(){
	//	$(".first_menu a").removeClass("on");
	//	$(this).addClass("on");
	}).mouseleave(function(){
	//	$(".first_menu a").removeClass("on");
	//	$(".first_menu a").eq(onIndex).addClass("on");
	});
	
	// 导航菜单的二级子菜单的显隐
	$(".mainNav").mouseenter(function(e) {
		$(this).stop().animate({height: '178px'}, "fast");
	//	$(this).children(".second_menu").stop(true, true).slideDown();
	}).mouseleave(function(e){
		$(this).stop().animate({height: '47px'}, "fast");
	//	$(this).children(".second_menu").stop(true, true).slideUp();
	});
	
	// 首页底部"友情链接"TAG切换
	$(".index_bottom_container .main_part .right_part ol li").mouseover(function(){
		var bottom_menu_index = 0;
		bottom_menu_index = $(this).index();
		$(".index_bottom_container .main_part .right_part ol li").removeClass("on").eq(bottom_menu_index).addClass("on");
		$(".index_bottom_container .main_part .right_part table").hide().eq(bottom_menu_index).css("display","table");
	});
	
	// 顶部"无障碍阅读"工具栏的显隐
	$(".assist_tools_btn").click(function(){
		$(".top_tools").slideToggle(500);	
	});
	$(".top_tools_hide").click(function(){
		$(".top_tools").slideToggle(500);	
	});
	
	
});



//网页简繁体转换
//本js用于客户在网站页面选择繁体中文或简体中文显示，默认是正常显示，即简繁体同时显示
//在用户第一次访问网页时,会自动检测客户端语言进行操作并提示.此功能可关闭
//本程序只在UTF8编码下测试过，不保证其他编码有效

//-------------- 以下参数大部分可以更改 --------------------
//s = simplified 简体中文 t = traditional 繁体中文 n = normal 正常显示
var zh_default = 'n'; //默认语言，请不要改变
var zh_choose = 'n'; //当前选择
var zh_expires = 7; //cookie过期天数
var zh_class = 'zh_click'; //链接的class名，id为class + s/t/n 之一
var zh_style_active = 'font-weight:bold; color:green;'; //当前选择的链接式样
var zh_style_inactive = 'color:blue;'; //非当前选择的链接式样
var zh_browserLang = ''; //浏览器语言
var zh_autoLang_t = false; //浏览器语言为繁体时自动进行操作
var zh_autoLang_s = false; //浏览器语言为简体时自动进行操作
var zh_autoLang_alert = false; //自动操作后是否显示提示消息
//自动操作后的提示消息
var zh_autoLang_msg = '歡迎來到本站,本站爲方便台灣香港的用戶\n1.采用UTF-8國際編碼,用任何語言發帖都不用轉碼.\n2.自動判斷繁體用戶,顯示繁體網頁\n3.在網頁最上方有語言選擇,如果浏覽有問題時可以切換\n4.本消息在cookie有效期內只顯示一次';
var zh_autoLang_checked = 0; //次检测浏览器次数,第一次写cookie为1,提示后为2,今后将不再提示


//判断浏览器语言的正则,ie为小写,ff为大写
var zh_langReg_t = /^zh-tw|zh-hk$/i;
var zh_langReg_s = /^zh-cn$/i;

//简体繁体对照字表,可以自行替换
var zh_s = '皑蔼碍爱翱袄奥坝罢摆败颁办绊帮绑镑谤剥饱宝报鲍辈贝钡狈备惫绷笔毕毙闭边编贬变辩辫鳖瘪濒滨宾摈饼拨钵铂驳卜补参蚕残惭惨灿苍舱仓沧厕侧册测层诧搀掺蝉馋谗缠铲产阐颤场尝长偿肠厂畅钞车彻尘陈衬撑称惩诚骋痴迟驰耻齿炽冲虫宠畴踌筹绸丑橱厨锄雏础储触处传疮闯创锤纯绰辞词赐聪葱囱从丛凑窜错达带贷担单郸掸胆惮诞弹当挡党荡档捣岛祷导盗灯邓敌涤递缔点垫电淀钓调迭谍叠钉顶锭订东动栋冻斗犊独读赌镀锻断缎兑队对吨顿钝夺鹅额讹恶饿儿尔饵贰发罚阀珐矾钒烦范贩饭访纺飞废费纷坟奋愤粪丰枫锋风疯冯缝讽凤肤辐抚辅赋复负讣妇缚该钙盖干赶秆赣冈刚钢纲岗皋镐搁鸽阁铬个给龚宫巩贡钩沟构购够蛊顾剐关观馆惯贯广规硅归龟闺轨诡柜贵刽辊滚锅国过骇韩汉阂鹤贺横轰鸿红后壶护沪户哗华画划话怀坏欢环还缓换唤痪焕涣黄谎挥辉毁贿秽会烩汇讳诲绘荤浑伙获货祸击机积饥讥鸡绩缉极辑级挤几蓟剂济计记际继纪夹荚颊贾钾价驾歼监坚笺间艰缄茧检碱硷拣捡简俭减荐槛鉴践贱见键舰剑饯渐溅涧浆蒋桨奖讲酱胶浇骄娇搅铰矫侥脚饺缴绞轿较秸阶节茎惊经颈静镜径痉竞净纠厩旧驹举据锯惧剧鹃绢杰洁结诫届紧锦仅谨进晋烬尽劲荆觉决诀绝钧军骏开凯颗壳课垦恳抠库裤夸块侩宽矿旷况亏岿窥馈溃扩阔蜡腊莱来赖蓝栏拦篮阑兰澜谰揽览懒缆烂滥捞劳涝乐镭垒类泪篱离里鲤礼丽厉励砾历沥隶俩联莲连镰怜涟帘敛脸链恋炼练粮凉两辆谅疗辽镣猎临邻鳞凛赁龄铃凌灵岭领馏刘龙聋咙笼垄拢陇楼娄搂篓芦卢颅庐炉掳卤虏鲁赂禄录陆驴吕铝侣屡缕虑滤绿峦挛孪滦乱抡轮伦仑沦纶论萝罗逻锣箩骡骆络妈玛码蚂马骂吗买麦卖迈脉瞒馒蛮满谩猫锚铆贸么霉没镁门闷们锰梦谜弥觅绵缅庙灭悯闽鸣铭谬谋亩钠纳难挠脑恼闹馁腻撵捻酿鸟聂啮镊镍柠狞宁拧泞钮纽脓浓农疟诺欧鸥殴呕沤盘庞国爱赔喷鹏骗飘频贫苹凭评泼颇扑铺朴谱脐齐骑岂启气弃讫牵扦钎铅迁签谦钱钳潜浅谴堑枪呛墙蔷强抢锹桥乔侨翘窍窃钦亲轻氢倾顷请庆琼穷趋区躯驱龋颧权劝却鹊让饶扰绕热韧认纫荣绒软锐闰润洒萨鳃赛伞丧骚扫涩杀纱筛晒闪陕赡缮伤赏烧绍赊摄慑设绅审婶肾渗声绳胜圣师狮湿诗尸时蚀实识驶势释饰视试寿兽枢输书赎属术树竖数帅双谁税顺说硕烁丝饲耸怂颂讼诵擞苏诉肃虽绥岁孙损笋缩琐锁獭挞抬摊贪瘫滩坛谭谈叹汤烫涛绦腾誊锑题体屉条贴铁厅听烃铜统头图涂团颓蜕脱鸵驮驼椭洼袜弯湾顽万网韦违围为潍维苇伟伪纬谓卫温闻纹稳问瓮挝蜗涡窝呜钨乌诬无芜吴坞雾务误锡牺袭习铣戏细虾辖峡侠狭厦锨鲜纤咸贤衔闲显险现献县馅羡宪线厢镶乡详响项萧销晓啸蝎协挟携胁谐写泻谢锌衅兴汹锈绣虚嘘须许绪续轩悬选癣绚学勋询寻驯训讯逊压鸦鸭哑亚讶阉烟盐严颜阎艳厌砚彦谚验鸯杨扬疡阳痒养样瑶摇尧遥窑谣药爷页业叶医铱颐遗仪彝蚁艺亿忆义诣议谊译异绎荫阴银饮樱婴鹰应缨莹萤营荧蝇颖哟拥佣痈踊咏涌优忧邮铀犹游诱舆鱼渔娱与屿语吁御狱誉预驭鸳渊辕园员圆缘远愿约跃钥岳粤悦阅云郧匀陨运蕴酝晕韵杂灾载攒暂赞赃脏凿枣灶责择则泽贼赠扎札轧铡闸诈斋债毡盏斩辗崭栈战绽张涨帐账胀赵蛰辙锗这贞针侦诊镇阵挣睁狰帧郑证织职执纸挚掷帜质钟终种肿众诌轴皱昼骤猪诸诛烛瞩嘱贮铸筑驻专砖转赚桩庄装妆壮状锥赘坠缀谆浊兹资渍踪综总纵邹诅组钻致钟么为只凶准启板里雳余链泄';
var zh_t = '皚藹礙愛翺襖奧壩罷擺敗頒辦絆幫綁鎊謗剝飽寶報鮑輩貝鋇狽備憊繃筆畢斃閉邊編貶變辯辮鼈癟瀕濱賓擯餅撥缽鉑駁蔔補參蠶殘慚慘燦蒼艙倉滄廁側冊測層詫攙摻蟬饞讒纏鏟産闡顫場嘗長償腸廠暢鈔車徹塵陳襯撐稱懲誠騁癡遲馳恥齒熾沖蟲寵疇躊籌綢醜櫥廚鋤雛礎儲觸處傳瘡闖創錘純綽辭詞賜聰蔥囪從叢湊竄錯達帶貸擔單鄲撣膽憚誕彈當擋黨蕩檔搗島禱導盜燈鄧敵滌遞締點墊電澱釣調叠諜疊釘頂錠訂東動棟凍鬥犢獨讀賭鍍鍛斷緞兌隊對噸頓鈍奪鵝額訛惡餓兒爾餌貳發罰閥琺礬釩煩範販飯訪紡飛廢費紛墳奮憤糞豐楓鋒風瘋馮縫諷鳳膚輻撫輔賦複負訃婦縛該鈣蓋幹趕稈贛岡剛鋼綱崗臯鎬擱鴿閣鉻個給龔宮鞏貢鈎溝構購夠蠱顧剮關觀館慣貫廣規矽歸龜閨軌詭櫃貴劊輥滾鍋國過駭韓漢閡鶴賀橫轟鴻紅後壺護滬戶嘩華畫劃話懷壞歡環還緩換喚瘓煥渙黃謊揮輝毀賄穢會燴彙諱誨繪葷渾夥獲貨禍擊機積饑譏雞績緝極輯級擠幾薊劑濟計記際繼紀夾莢頰賈鉀價駕殲監堅箋間艱緘繭檢堿鹼揀撿簡儉減薦檻鑒踐賤見鍵艦劍餞漸濺澗漿蔣槳獎講醬膠澆驕嬌攪鉸矯僥腳餃繳絞轎較稭階節莖驚經頸靜鏡徑痙競淨糾廄舊駒舉據鋸懼劇鵑絹傑潔結誡屆緊錦僅謹進晉燼盡勁荊覺決訣絕鈞軍駿開凱顆殼課墾懇摳庫褲誇塊儈寬礦曠況虧巋窺饋潰擴闊蠟臘萊來賴藍欄攔籃闌蘭瀾讕攬覽懶纜爛濫撈勞澇樂鐳壘類淚籬離裏鯉禮麗厲勵礫曆瀝隸倆聯蓮連鐮憐漣簾斂臉鏈戀煉練糧涼兩輛諒療遼鐐獵臨鄰鱗凜賃齡鈴淩靈嶺領餾劉龍聾嚨籠壟攏隴樓婁摟簍蘆盧顱廬爐擄鹵虜魯賂祿錄陸驢呂鋁侶屢縷慮濾綠巒攣孿灤亂掄輪倫侖淪綸論蘿羅邏鑼籮騾駱絡媽瑪碼螞馬罵嗎買麥賣邁脈瞞饅蠻滿謾貓錨鉚貿麽黴沒鎂門悶們錳夢謎彌覓綿緬廟滅憫閩鳴銘謬謀畝鈉納難撓腦惱鬧餒膩攆撚釀鳥聶齧鑷鎳檸獰甯擰濘鈕紐膿濃農瘧諾歐鷗毆嘔漚盤龐國愛賠噴鵬騙飄頻貧蘋憑評潑頗撲鋪樸譜臍齊騎豈啓氣棄訖牽扡釺鉛遷簽謙錢鉗潛淺譴塹槍嗆牆薔強搶鍬橋喬僑翹竅竊欽親輕氫傾頃請慶瓊窮趨區軀驅齲顴權勸卻鵲讓饒擾繞熱韌認紉榮絨軟銳閏潤灑薩鰓賽傘喪騷掃澀殺紗篩曬閃陝贍繕傷賞燒紹賒攝懾設紳審嬸腎滲聲繩勝聖師獅濕詩屍時蝕實識駛勢釋飾視試壽獸樞輸書贖屬術樹豎數帥雙誰稅順說碩爍絲飼聳慫頌訟誦擻蘇訴肅雖綏歲孫損筍縮瑣鎖獺撻擡攤貪癱灘壇譚談歎湯燙濤縧騰謄銻題體屜條貼鐵廳聽烴銅統頭圖塗團頹蛻脫鴕馱駝橢窪襪彎灣頑萬網韋違圍爲濰維葦偉僞緯謂衛溫聞紋穩問甕撾蝸渦窩嗚鎢烏誣無蕪吳塢霧務誤錫犧襲習銑戲細蝦轄峽俠狹廈鍁鮮纖鹹賢銜閑顯險現獻縣餡羨憲線廂鑲鄉詳響項蕭銷曉嘯蠍協挾攜脅諧寫瀉謝鋅釁興洶鏽繡虛噓須許緒續軒懸選癬絢學勳詢尋馴訓訊遜壓鴉鴨啞亞訝閹煙鹽嚴顔閻豔厭硯彥諺驗鴦楊揚瘍陽癢養樣瑤搖堯遙窯謠藥爺頁業葉醫銥頤遺儀彜蟻藝億憶義詣議誼譯異繹蔭陰銀飲櫻嬰鷹應纓瑩螢營熒蠅穎喲擁傭癰踴詠湧優憂郵鈾猶遊誘輿魚漁娛與嶼語籲禦獄譽預馭鴛淵轅園員圓緣遠願約躍鑰嶽粵悅閱雲鄖勻隕運蘊醞暈韻雜災載攢暫贊贓髒鑿棗竈責擇則澤賊贈紮劄軋鍘閘詐齋債氈盞斬輾嶄棧戰綻張漲帳賬脹趙蟄轍鍺這貞針偵診鎮陣掙睜猙幀鄭證織職執紙摯擲幟質鍾終種腫衆謅軸皺晝驟豬諸誅燭矚囑貯鑄築駐專磚轉賺樁莊裝妝壯狀錐贅墜綴諄濁茲資漬蹤綜總縱鄒詛組鑽緻鐘麼為隻兇準啟闆裡靂餘鍊洩';
String.prototype.tran = function() {
	var s1,s2;
	if (zh_choose == 't') {
		s1 = zh_s;
		s2 = zh_t;
	}else if(zh_choose == 's') {
		s1 = zh_t;
		s2 = zh_s;
	}else {
		return this;
	}
	var a = '';
	var l = this.length;
	for(var i=0;i<this.length;i++){
	     var c = this.charAt(i);
	     var p = s1.indexOf(c)
	     a += p < 0 ? c : s2.charAt(p);
	 }
	return a;
}
function setCookie(name, value) {
	var argv = setCookie.arguments;
	var argc = setCookie.arguments.length;
	var expires = (argc > 2) ? argv[2] : null;
	if (expires != null) {
		var LargeExpDate = new Date ();
		LargeExpDate.setTime(LargeExpDate.getTime() + (expires*1000*3600*24));
	}
	document.cookie = name + "=" + escape (value)+((expires == null) ? "" : ("; expires=" +LargeExpDate.toGMTString()));
}
function getCookie(Name) {
	var search = Name + "="
	if (document.cookie.length > 0) {
		offset = document.cookie.indexOf(search);
		if(offset != -1) {
			 offset += search.length;
			 end = document.cookie.indexOf(";", offset);
			 if(end == -1) end = document.cookie.length;
			 	return unescape(document.cookie.substring(offset, end));
		}else {
			return '';
		}
	}
}


function zh_tranBody(obj) { 
	var o = (typeof(obj) == "object") ? obj.childNodes : document.body.childNodes;
	for (var i = 0; i < o.length; i++) {
		var c = o.item(i);
		if ('||BR|HR|TEXTAREA|SCRIPT|OBJECT|'.indexOf("|"+c.tagName+"|") > 0) continue;
			if (c.className == zh_class) {
				 if (c.id == zh_class + '_' + zh_choose) {
					 c.setAttribute('style', zh_style_active);
					 c.style.cssText = zh_style_active;
				 }else {
					 c.setAttribute('style', zh_style_inactive);
					 c.style.cssText = zh_style_inactive;
				 }
				 continue;   
			}
			if (c.title != '' && c.title != null) c.title = c.title.tran();
			if (c.alt != '' && c.alt != null) c.alt = c.alt.tran();
			if (c.tagName == "INPUT" && c.value != '' && c.type != 'text' && c.type != 'hidden' && c.type != 'password') c.value = c.value.tran();
			if (c.nodeType == 3) {
				c.data = c.data.tran();  
		}else{
			zh_tranBody(c);
		}
	}
}

function zh_tran(go) {
	if (go) zh_choose = go;
	setCookie('zh_choose', zh_choose, zh_expires);
	if (go == 'n') {
		window.location.reload();
	}else {
		if(go=='s')
			$('.subNav .subNav_btn02').attr('href', 'javascript:zh_tran("t")').html('繁体版');
		if(go=='t')
			$('.subNav .subNav_btn02').attr('href', 'javascript:zh_tran("s")').html('简体版');
		zh_tranBody();
	}
}

function zh_getLang() {
	if (getCookie('zh_choose')) {
		zh_choose = getCookie('zh_choose');
		return true;
	} 
	if (!zh_autoLang_t && !zh_autoLang_s) return false;
	if (getCookie('zh_autoLang_checked')) return false;
	if (navigator.language) {
		zh_browserLang = navigator.language;
	}else if (navigator.browserLanguage) {
		zh_browserLang = navigator.browserLanguage;
	}
	if (zh_autoLang_t && zh_langReg_t.test(zh_browserLang)) {
		zh_choose = 't';
	}else if (zh_autoLang_s && zh_langReg_s.test(zh_browserLang)) {
		zh_choose = 's';
	}
	zh_autoLang_checked = 1;
	setCookie('zh_choose', zh_choose, zh_expires);
	if (zh_choose == zh_default) return false;
		return true;
}

function zh_init() {
	zh_getLang(); 
	c = document.getElementById(zh_class + '_' + zh_choose);
	if (zh_choose != zh_default) {
		if (window.onload) {
			window.onload_before_zh_init = window.onload;
			window.onload = function() {
				zh_tran(zh_choose);
				if (getCookie('zh_autoLang_check')) {alert(zh_autoLang_msg);};
				window.onload_before_zh_init();
			};
		}else {
			window.onload = function() {
				zh_tran(zh_choose);
				if (getCookie('zh_autoLang_check')) {alert(zh_autoLang_msg);};
			};
		}
	}
}

zh_init();
//网页简繁体转换 结束


// 设为首页
function SetHome(obj,vrl){
	try{
		obj.style.behavior='url(#default#homepage)';
		obj.setHomePage(vrl);
	}catch(e){
		if(window.netscape) {
			try {
				netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect");
			}catch (e) {
				alert("此操作被浏览器拒绝！\n请在浏览器地址栏输入“about:config”并回车\n然后将 [signed.applets.codebase_principal_support]的值设置为'true',双击即可。");
			}
			var prefs = Components.classes['@mozilla.org/preferences-service;1'].getService(Components.interfaces.nsIPrefBranch);
			prefs.setCharPref('browser.startup.homepage',vrl);
		}
	}
}

// 加入收藏
function AddFavorite(sURL, sTitle)
{
    try{
        window.external.addFavorite(sURL, sTitle);
    }catch (e){
        try{
            window.sidebar.addPanel(sTitle, sURL, "");
        }catch (e){
            alert("加入收藏失败，请使用Ctrl+D进行添加");
        }
    }
}





// 无障碍阅读功能
$(function(){
	// 1、纯文本功能
	$(".top_tools_txt").click(function(){
		if(!$(this).hasClass("on")){
			$("link[id != 'base_style']").each(function(index, element) {
				var e =  $(element);
				e.attr("url",e.attr("href"));
			});
			$(".main_container").css("text-align","center");
			$("link[id != 'base_style']").attr("href","");
			$(".main_container img").hide();
			$(".section_index_pics").hide();
			$(".section_index_swiper").hide();
			$(".section_news_swiper").hide();
			$(this).addClass("on");
		}else{
			$("link[id != 'base_style']").each(function(index, element) {
				var e =  $(element);
				e.attr("href",e.attr("url"));
			});
			$(".main_container").css("text-align","left");
			$(".main_container img").show();
			$(".section_index_pics").show();
			$(".section_index_swiper").show();
			$(".section_news_swiper").show();
			$(this).removeClass("on");	
		}
	});
	
	// 2、页面放大缩小
	var zoon = 1;
	$(".top_tools_pageB").click(function(){zoonfun(0);});
	$(".top_tools_pageS").click(function(){zoonfun(1);});
	var zoonfun = function(bool){
		if(bool==0){
			zoon+=.1;
			if(zoon > 2){
				zoon=2;
			}
		}else if(bool==1){
			if(zoon < .5){
				zoon=.5;
			}else if(zoon>1){
				zoon-=.1;
			}
		}
		if(bool == -1){
			zoon = 1;
		}
		var bodym = $(".main_container");
		bodym.css({'transform-origin':'top center'});
		bodym.css({'transform':'scale('+zoon+','+zoon+')'});
		if(navigator.appName == "Microsoft Internet Explorer" && navigator.appVersion .split(";")[1].replace(/[ ]/g,"")=="MSIE8.0"){ 
			bodym.css("zoom",zoon);
		}
	};
	
	// 3、配色
	var element_styles = new Array(); // 修改的对象样式集合(记录已修改的)
	var element_doms = new Array(); // 修改的对象集合(记录已修改的)
	var changeColorFlag = false;
	$(".top_tools_color").click(function(){
		var index = $(".top_tools_color").index(this);
		if(index !=0){
			change_color($(this).css("background-color"),$(this).css("color"));
			changeColorFlag = true;
			return;
		}else{
			if(changeColorFlag){
				for(var i=0; i<element_doms.length; i++){
					$(element_doms[i]).attr("style",element_styles[i]);
				}
				$('body').css("background","#f2f2f4");
				changeColorFlag = false;
			}
		}
	});
	
	var change_color = function(bgcolor,txtcolor){
		var e = $($(".main_container").find("div,dl,dt,dd,ul,ol,li,h1,h2,h3,h4,h5,h6,pre,form,fieldset,input,textarea,p,blockquote,th,td,ul, ol input,select,textare ,a ,span, header"));
		if(!changeColorFlag){
			for(var i =0; i<e.length; i++){
				element_doms[i] = $(e[i]);
				element_styles[i] = $(e[i]).attr('style')?$(e[i]).attr('style'):'';
			}
		}
		e.css("background",bgcolor);
		e.css("color",txtcolor);
		$('body').css("background",bgcolor);
	};
	
	
	// 4、辅助线
	$(".top_tools_line").toggle(
		function(){
			$(this).html("开").css("background-position","left bottom");
			assistant_line();
		}, function(){
			$(this).html("关").css("background-position","left top");
			assistant_line();
		});
	//$(".top_tools_line").click(function(){assistant_line();});
	var assistant_line = function(){
		if($(".page_x").size()>0){
			$(".page_x").remove();	
			$(".page_y").remove();	
		}else{
			$("body").append('<div class="page_x"></div>');
			$("body").append('<div class="page_y"></div>');
			$(document).mousemove(function (o){
				var m = $(".page_y").outerWidth(true);
				var n = $(".page_x").outerHeight(true);
				var l = o.clientX - m;
				var k = o.clientY - n;
				$(".page_x").css({left : 0, top : k});
				$(".page_y").css({left : l, top : 0});
			});
		}
	};
	
	// 5、放大器
	var txt = '<table class="accessibility_txt_content">'
			   + '<tr>'
			   + '<td id="accessibility_txt_content_td"></td>'
		       + '</tr>'	
		       + '</table>';
	$("body").append(txt); //插入放大框
	$(".top_tools_enlarger").click(function(){assistant_txt();});
	var assistant_txt = function(){
		$(".accessibility_txt_content").fadeToggle();
		$(".main_container").mousemove(function (e){	
			var tar = e.target;
			var txt = $(tar).text();
			if(txt.length>20){
				$("#accessibility_txt_content_td").css("font-size","40px");
			}
			if($(tar).is("a")||$(tar).is("span")||$(tar).is("p")||$(tar).is("td")||$(tar).is("dd")){
				$("#accessibility_txt_content_td").html(txt);
			}
		});
	};
	
	// 无障碍阅读重置
	$(".top_tools_reset").click(function(){window.location.reload();});
	
});

	




