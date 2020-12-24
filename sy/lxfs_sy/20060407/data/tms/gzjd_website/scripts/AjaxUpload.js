/**
 * ajax上传类
 * 
 * @author roy huang
 * @version 20120118
 */
(function() {

	var AjaxUpload = function(form) {
		this._init(form);
	};

	// 返回数据格式(json/xml/text)
	AjaxUpload.prototype.resultType = "json";
	AjaxUpload.prototype._form;
	AjaxUpload.prototype._iFrame = null;// 存放iFrame对象
	AjaxUpload.prototype._loader;
	AjaxUpload.prototype.useLoader=false;

	/**
	 * 构造函数
	 * 
	 * @param target
	 */
	AjaxUpload.prototype._init = function(form) {
		this._form = $(form);
		try {
			this._loader = new org.developerworld.Loader();
		} catch (e) {
		}
	};
	/**
	 * 完成回调方法（一般不需要重写，除非想自己处理返回结果）
	 */
	AjaxUpload.prototype._onLoad = function() {
		if (this.useLoader && this._loader)
			this._loader.unlockComplate(this._form);
		var data = null;
		var html = null;
		var text = null;
		var dom = null;
		try {
			var iFrame = this._iFrame[0];
			if (iFrame.contentWindow) {
				html = iFrame.contentWindow.document.body ? iFrame.contentWindow.document.body.innerHTML
						: null;
				text = iFrame.contentWindow.document.body ? $(
						iFrame.contentWindow.document.body).text() : null;
				dom = iFrame.contentWindow.document.XMLDocument ? iFrame.contentWindow.document.XMLDocument
						: iFrame.contentWindow.document;

			} else if (iFrame.contentDocument) {
				html = iFrame.contentDocument.document.body ? iFrame.contentDocument.document.body.innerHTML
						: null;
				text = iFrame.contentDocument.document.body ? $(
						iFrame.contentDocument.document.body).text() : null;
				dom = iFrame.contentDocument.document.XMLDocument ? iFrame.contentDocument.document.XMLDocument
						: iFrame.contentDocument.document;
			}
			if (this.resultType == "json")
				eval("data=" + text);
			else if (this.resultType == "xml")
				data = dom;
			else
				data = html;
			this.onSuccess(data);
		} catch (e) {
			this.onError(e, html);
		}
	};
	/**
	 * 上传成功执行函数(自行填写)
	 */
	AjaxUpload.prototype.onSuccess = function(data) {
	};
	/**
	 * 上传失败执行函数(自行填写)
	 */
	AjaxUpload.prototype.onError = function(e, html) {
		alert("org.developerworld.AjaxUpload Error!");
		alert(e.message);
		alert(html);
	};
	/**
	 * 执行上传
	 */
	AjaxUpload.prototype.upload = function() {
		if (this.useLoader && this._loader)
			this._loader.lockLoading(this._form);
		var time = new Date().getTime();
		this._iFrame = $(
				"<iframe name='org_developerworld_ajaxupload_" + time
						+ "' style='display:none'/>").appendTo(document.body);
		this._form.attr("target", this._iFrame.attr("name"));
		var base = this;
		this._iFrame.load(function() {
			base._onLoad();
		});
		this._form[0].submit();
	};

	// 写入命名空间
	if (!window.org)
		window.org = {};
	if (!window.org.developerworld)
		window.org.developerworld = {};
	window.org.developerworld.AjaxUpload = AjaxUpload;
	if (!window.AjaxUpload)
		window.AjaxUpload = AjaxUpload;
})();