(function() {
    var s_id="DL_1146465_2_1154160",w=window,d=document;
    
    function z() {
        var s, i=(w==top),u;
        if (!d.body) {
            setTimeout(z, 100);
            return;
        }

        if (!i) {
            try {
                s=top.document.body;
                i=true;
            } catch(x) {}
			
			try {
				if (window['$WLXRmAd'] || (window.parent && window.parent['$WLXRmAd'])) {
					i=true;
				}
			} catch(x) {}
        }

        s=d.getElementById(s_id);
        if (!s) {
            s=d.createElement('script');
            u="//amch.questionmarket.com/adsc/d1146465/2/1154160/decide.php?1";
            if (i) u += "&noiframe=1";
            s.src=u;
            s.id=s_id;
            d.body.insertBefore(s, d.body.firstChild);
        }
    }

    function t() {
    		setTimeout(z, 2000);
    }
 
    if (false || d.readyState=="complete" || w.inDapIF) {
        t();
    } else if (w.addEventListener) { // DOM
        w.addEventListener("load", t, false);
    } else if (w.attachEvent) { // IE 5+
        w.attachEvent("onload", t);
    }
})();

