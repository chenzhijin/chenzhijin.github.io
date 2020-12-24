$(document).ready(function() {
	$(".news_content_txt2 a").each(function(){
		var ym0 = $(this).attr('href').replace(/^\s+|\s+$/g, "");
		var ym0_1 = ym0.substring(0, 1);
		var ym0_5 = ym0.substring(0, 5);
		var ym0_10 = ym0.substring(0, 10);
		var ym0_18 = ym0.substring(0, 18);
		var ym0_22 = ym0.substring(0, 22);
		
		if(ym0_1 == "/" || ym0_1 == "." || ym0_1 == "#" || ym0_5 == "index" || ym0_10 == "javascript" || ym0_18 == 'javascript:void(0)' || ym0_22 == 'http://www.gzjd.gov.cn') {
			return;
		} else {
			var aa = $(this).html();
			$(this).prop('outerHTML', '<span>' + aa + '</span>');
		}
	});
	
	$(".info_content_txt2 a").each(function(){
		var ym0 = $(this).attr('href').replace(/^\s+|\s+$/g, "");
		var ym0_1 = ym0.substring(0, 1);
		var ym0_5 = ym0.substring(0, 5);
		var ym0_10 = ym0.substring(0, 10);
		var ym0_18 = ym0.substring(0, 18);
		var ym0_22 = ym0.substring(0, 22);
		
		if(ym0_1 == "/" || ym0_1 == "." || ym0_1 == "#" || ym0_5 == "index" || ym0_10 == "javascript" || ym0_18 == 'javascript:void(0)' || ym0_22 == 'http://www.gzjd.gov.cn') {
			return;
		} else {
			var aa = $(this).html();
			$(this).prop('outerHTML', '<span>' + aa + '</span>');
		}
	});
	
	$("a").click(function() {
		var ym0 = $(this).attr('href').replace(/^\s+|\s+$/g, "");
		var ym0_1 = ym0.substring(0, 1);
		var ym0_5 = ym0.substring(0, 5);
		var ym0_10 = ym0.substring(0, 10);
		var ym0_18 = ym0.substring(0, 18);
		var ym0_22 = ym0.substring(0, 22);
		
		if(ym0_1 == "/" || ym0_1 == "." || ym0_1 == "#" || ym0_5 == "index" || ym0_10 == "javascript" || ym0_18 == 'javascript:void(0)' || ym0_22 == 'http://www.gzjd.gov.cn') {
			return;
		} else {
			return confirm('鎮ㄨ闂殑閾炬帴鍗冲皢绂诲紑鈥滃箍宸炲競鍏畨灞€鈥濋棬鎴风綉绔欙紝鏄惁缁х画锛�');
		}
	});
});