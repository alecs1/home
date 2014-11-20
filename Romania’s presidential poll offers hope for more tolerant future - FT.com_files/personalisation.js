var P13n=P13n||{};
P13n.cookie=(function(){var a=/\+/g,e={};
function c(f){return f
}function b(f){return decodeURIComponent(f.replace(a," "))
}return function d(k,j,o){if(arguments.length>1&&(!/Object/.test(Object.prototype.toString.call(j))||j==null)){o=$.extend({},e,o);
if(j==null){o.expires=-1
}if(typeof o.expires==="number"){var l=o.expires,n=o.expires=new Date();
n.setDate(n.getDate()+l)
}j=String(j);
return(document.cookie=[encodeURIComponent(k),"=",o.raw?j:encodeURIComponent(j),o.expires?"; expires="+o.expires.toUTCString():"",o.path?"; path="+o.path:"",o.domain?"; domain="+o.domain:"",o.secure?"; secure":""].join(""))
}o=j||e||{};
var f=o.raw?c:b;
var m=document.cookie.split("; ");
for(var h=0,g;
(g=m[h]&&m[h].split("="));
h++){if(f(g.shift())===k){return f(g.join("="))
}}return null
}
}());
var P13n=P13n||{};
P13n.features=(function(){var g={},f=100;
var d={getFlags:function e(){var i="",k,l;
for(var j in g){if(g.hasOwnProperty(j)){k=g[j];
l=d[k];
if(typeof l==="boolean"&&l===true){i+=j
}}}return i
},setFlags:function b(n,k){if(!n){return
}k=typeof k==="undefined"?true:k;
var j,l;
for(var m=0;
m<n.length;
m+=1){j=n.charAt(m);
l=g[j];
if(typeof d[l]==="boolean"){d[l]=k
}}},loadOverrides:function a(i){var k=window.location.hash.substr(1),o=k.split("="),n="",m="";
if(o.length){if(o[0]==="p13nOn"){n=o.length===2?o[1]:"";
P13n.cookie("p13nOn",n,{path:"/",expires:f})
}else{if(o[0]==="p13nOff"){m=o.length===2?o[1]:"";
P13n.cookie("p13nOff",m,{path:"/",expires:f})
}}}d.setFlags(i,true);
var l=P13n.cookie("p13nOn"),j=P13n.cookie("p13nOff");
d.setFlags(l,true);
d.setFlags(j,false)
},init:function h(k){var l,j,n;
for(var m=0;
m<k.length;
m+=1){l=k[m][0];
j=k[m][1]||"";
n=k[m][2];
if(typeof d[l]==="undefined"){d[l]=n;
if(j.length>=1&&typeof g[j]==="undefined"){g[j]=l
}}}},destroy:function c(){g={};
for(var i in d){if(d.hasOwnProperty(i)&&typeof d[i]==="boolean"){delete d[i]
}}}};
return d
}());
var P13n=P13n||{};
P13n.logging=(function(){var b="http://track.ft.com/log";
function c(e,f){if(a[f]&&a[f]===true){$.ajax({cache:false,data:e,dataType:"script",global:false,type:"GET",url:b})
}}function d(e){if(typeof e!=="undefined"){e=e.split(",");
for(var f=0;
f<e.length;
f=f+1){a[e[f]]=true
}}}var a={annotations:false,notWhitelisted:false,recommends:false};
return{logMap:a,logEvent:c,init:d}
}());
var P13n=P13n||{};
P13n.recommends=(function(){var z={elementId:"recommended-reads-container",insertAfter:"#ft-story-tools-bottom",serviceBaseUrl:"",override:false,maxItems:10,timeout:10000,whitelist:null},p=z,b=".p13n.rr",l="#recommended",t=$(window),A={NOT_IN_WHITELIST:"NOT_IN_WHITELIST",EMPTY_LIST:"EMPTY_LIST"},u={READ:"read",POSITIVE:"pos",NEGATIVE:"neg"},c={NONE:{abbr:"x",title:" ",tooltip:" ",compView:"noreason-rr-li"},RELATED:{abbr:"r",title:"Related articles",tooltip:"Related articles bring you news and stories similar to the one you're currently reading to help you discover content on FT.com.",compView:"leiki-related-rr-li"},POPULAR:{abbr:"p",title:"Popular articles",tooltip:"These recommendations are based other popular articles.",compView:"leiki-popular-rr-li"},HISTORY:{abbr:"h",title:"Based on your browsing history",tooltip:'These recommendations are based on your browsing history and bring you news and relevant stories to help you discover more content on FT.com.<br/>FT.com takes your privacy seriously. For more information, please view our <a href="http://www.ft.com/servicestools/help/privacy">privacy policy</a>.',compView:"recommended-reads"},ANNOTATIONS:{abbr:"a",title:"Articles annotated by your professors",tooltip:"The articles displayed have been annotated by your school.",compView:"newslines-links-li"}},s={getsmartcontent:c.HISTORY,getrelated:c.RELATED,getpopularcontent:c.POPULAR};
var a={feedback:u,excuses:A,reasons:c,request:{articleRecommendations:function G(N,K){var O=!!K;
if(O&&!a.isValidErightsId(K)){return n("Not a valid user")
}if(!a.isValidUUID(N)){return n("Not a valid UUID")
}var L=p.serviceBaseUrl+"recommendations/"+(O?K:""),M={articleUUID:N,max:(p.maxItems||1)+2,engine:"ontotext"};
return d(L,M,"rr_li")
},articleRead:function e(K,O,N){var P=!K;
if(P||!a.isValidErightsId(K)){return n("Cannot register feedback without a valid ErightsID")
}if(!a.isValidUUID(O)){return n("Not a valid UUID")
}var L=p.serviceBaseUrl+"recommendations/update/"+K,M={articleUUID:O,feedbackType:N||u.READ};
return d(L,M,"rr_up")
},newslinesAnnotations:function i(K,N){if(!a.isValidErightsId(K)){return n("Cannot get Annotated Articles without a valid ErightsID")
}var M=f(N);
if(!M){return n(N+" is not a valid Newslines school")
}var L="http://newslines.ft.com/feed/organisation/"+encodeURI(M);
return d(L,{format:"json"},"aaf_li",true)
}},isValidErightsId:function q(K){return typeof K==="string"&&/^\d+$/.test(K)
},isValidUUID:function w(K){return typeof K==="string"&&/^[0-9a-f\-]{36}/i.test(K)
},hasDoNotTrackFlag:function(){return(/_52Y_/).test((P13n.cookie("AYSC")||""))
},hasMBANewslinesAnnotations:function F(){var K=(P13n.cookie("AYSC")||"").match(/_50([^_]*)_/);
return !!K&&K.length>1&&parseInt(K[1],10)>0
},getOrgName:function o(){var K=(P13n.cookie("AYSC")||"").match(/_59([^_]*)_/);
return !!K&&K.length>1?K[1]:null
},isCurrentSectionWhiteListed:function H(K){var M=K||p.whitelist;
if(!M){return true
}if(M&&M instanceof Array&&!M.length){return false
}var N="";
try{N=FT.env.dfp_site.split(".");
N=N[N.length-1].toLowerCase();
return new RegExp("^("+M.join("|")+")$","i").test(N)
}catch(L){return false
}},getTemplate:function J(N){var M="";
M+=('<div class="recommends clearfix" data-comp-name="p13nRecommendedReads" data-comp-view="'+N.reason.compView+'" data-comp-index="0">');
M+="<style>";
M+=".recommends{ clear:left ; border-top:8px solid #e9decf ; padding:6px 0 13px 0 ; width:600px ; }";
M+=".recommends h3{ margin:0 0 15px 0 ; }";
M+=".recommends h3 a{ color:#000 ; }";
M+=".recommends h3 a:hover{ color:#2e6e9e ; }";
M+=".recommends-row{ float:left ; clear:left ; margin:0 0 11px 0 ; width:600px ; }";
M+=".recommends-box{ float:left ; padding:0 15px 0 0 ; }";
M+=".tripleColumn .recommends-box { width:185px ; }";
M+=".doubleColumn .recommends-box { width:285px ; }";
M+=".recommends-box h4{ margin:0 0 5px 0 ; font-size:14px ; }";
M+=".recommends-box h4 a{ color:#000 ; }";
M+=".recommends-box h4 a:hover{ color:#2e6e9e ; }";
M+=".recommends-box li{ margin:0 0 5px 0 ; font-size:13px ; line-height:16px ; }";
M+=".msie .recommends { padding-bottom:9px ; }";
M+=".msie6 .recommends-row { display:inline ; }";
M+="#recommended-reads-container{font-family:Arial, Helvetica, sans-serif;clear:both;margin-top:20px;}";
M+='#recommended-reads-container .ui-dialog-titlebar-info {background: url("'+p.serviceBaseUrl+'assets/img/question-mark-icon.png") no-repeat scroll center center transparent;';
M+="cursor: pointer;float: right;height: 16px;margin: 5px;width: 16px;}";
M+="#recommended-reads-container .ui-dialog-titlebar-info span {left: -10000px;position: absolute;}";
M+="#recommendedReadsToolTipState {position: relative;} .roundedCorners { -moz-border-radius: 5px;border-radius: 5px 5px 5px 5px;}";
M+="#recommendedReadsToolTipContent {background-color: #FFFFFF;padding: 0 5px 5px 10px;position: relative;width: 345px;}";
M+="#recommendedReadsToolTipContent br{line-height:35px;}";
M+=".recommendedReadsToolTipOpen #recommendedReadsToolTip {display: block;}";
M+='#recommendedReadsToolTip {display: none;background: url("'+p.serviceBaseUrl+'assets/img/shadow-9x9.png") repeat scroll 0 0 transparent;color: #444444;font-size: 13px;';
M+="padding: 5px;position: absolute;right: 0;text-align: left;top: 20px;vertical-align: middle;z-index: 400;}";
M+="#recommendedReadsToolTipContent p {line-height: 21px;padding-top: 5px;padding-bottom: 0;}";
M+="</style>";
M+='<span id="rr-tooltip-container"><span id="recommendedReadsInfoIcon" class="ui-dialog-titlebar-info ui-corner-all" role="button">';
M+='<span class="ui-icon ui-icon-closethick">info</span>';
M+="</span>";
M+='<div id="recommendedReadsToolTipState">';
M+='<div class="roundedCorners" id="recommendedReadsToolTip">';
M+='<div id="recommendedReadsToolTipContent">';
M+=("<p>"+N.reason.tooltip+"</p></div></div></div></span><h3>Content recommended for you</h3>");
M+='<div class="recommends-row doubleColumn">';
M+=('<div class="recommends-box"><h4 class="reason-title">'+N.reason.title+'</h4><ul class="recommendations col-1">');
var O=null;
for(var L=0,K=Math.min(N.articles.length,N.maxItems);
L<K;
L+=1){O=N.articles[L];
M+=('<li data-pos="'+L+'"><a class="recommendation" href="'+O.href+'">'+O.title+"</a></li>");
if(L===4){M+='</ul></div><div class="recommends-box"><h4 class="reason-title">&nbsp;</h4><ul class="recommendations col-2">'
}}M+="</ul></div></div>";
return M
},render:function x(O){if(!$.isReady){$(function(){x(O)
});
return
}var M=$("#"+p.elementId),K=M.length>0,L=!O||O.articles.length===0;
if(K){M.remove()
}if(L){m(A.EMPTY_LIST);
return
}var N=$(p.insertAfter);
if(!N.length){return
}var P=a.getTemplate(O);
M=$(P).addClass("p13n").attr({id:p.elementId});
v(M);
N.after(M);
E("display",O.reason.abbr,O.articles.length)
},processRecommends:function g(O){var K=h(p.maxItems);
if(!O||O.status!=="ok"||typeof O.response.error!=="undefined"){return K
}K.reason=s[O.method];
var M=K.currentArticleUUID,P=window.location.href;
P=P.replace(/\#.*/g,"");
var L=l+"-"+K.reason.abbr+"-"+K.responseId,N="http://www.ft.com/cms/s/0/{uuid}.html";
K.articles=C(O.response.result,p.maxItems,function(R){var Q=/^http(s*):\/\//.test(R.cid),S=null;
if(!(R.cid===M||(Q&&R.cid===P))){S={title:R.headline,href:(Q?R.cid:N.replace(/\{uuid\}/,R.cid))+L}
}return S
});
return K
},processNewslinesFeed:function j(N){var K=h(p.maxItems);
if(!N){return K
}K.reason=c.ANNOTATIONS;
var L=l+"-"+K.reason.abbr+"-"+K.responseId,M=K.currentArticleUUID;
K.articles=C(N.items,p.maxItems,function(Q){var P=Q.link.match(/[0-9a-f\-]{36}/),O=M===(P.length&&P[0]);
if(!O){return{title:Q.title,href:Q.link+L}
}});
return K
},doArticleRecommends:function D(K,M,P){var L=K;
if(a.hasDoNotTrackFlag()){K=null
}if(a.hasMBANewslinesAnnotations()){P13n.logging.logEvent("personalisationProfileUpdate=updateOnlyAnnotations","annotations");
var N=a.getOrgName();
I(P,a.request.newslinesAnnotations(L,N),a.processNewslinesFeed);
a.request.articleRead(K,M,u.READ)
}else{if(!a.isCurrentSectionWhiteListed(p.whitelist)){P13n.logging.logEvent("personalisationProfileUpdate=updateOnlySectionNotWhitelisted","notWhitelisted");
a.request.articleRead(K,M,u.READ);
m(A.NOT_IN_WHITELIST)
}else{P13n.logging.logEvent("personalisationProfileUpdate=updateWithRecommendations","recommends");
I(P,a.request.articleRecommendations(M,K),a.processRecommends)
}}try{B()
}catch(O){}},setOptions:function k(K){p=$.extend(true,{},z,K||{})
}};
function B(){var K=window.location.hash||null;
if(K!==null&&K.indexOf(l)!==-1){var L=K.split("-");
if(typeof history.replaceState==="function"){history.replaceState("",window.document.title,window.location.pathname+window.location.search)
}E("read",L[1],parseInt(L[2],10))
}}function v(L){var M=function(){$("#recommendedReadsToolTipState").addClass("recommendedReadsToolTipOpen")
},O=function(){$("#recommendedReadsToolTipState").removeClass("recommendedReadsToolTipOpen")
},N=function(P){P.stopPropagation()
};
if(/^(iPhone|iPod|iPad)/i.test(navigator.platform)){var K=L.find("#recommendedReadsInfoIcon");
K.bind("touchend",function(P){M();
L.bind("touchstart touchend click",N);
$("body").one("touchend",function(){O();
L.unbind("touchstart touchend click",N)
})
})
}else{L.find("#rr-tooltip-container").hover(M,O)
}if($.browser.msie&&parseInt($.browser.version,10)===7){L.find("a.recommendation").click(N)
}}function I(L,M,K){L&&$.when(M).pipe(K).then(a.render,y)
}function m(K){E("nodisplay",K)
}function E(L,M,K){t.trigger(L+b,{text:M,num:K})
}function y(K,M,L){typeof window.console!=="undefined"&&console.log("ERROR: "+M)
}function r(){var L=(+new Date()).toString(),K=Math.floor(Math.random()*100000).toString();
return parseInt(K+L,10)
}function h(K){return{responseId:r(),currentArticleUUID:window.articleUUID||"",maxItems:K,reason:c.NONE,articles:[]}
}function f(K){if(!K){return undefined
}var L={"Anglia-Ruskin-University":"Anglia Ruskin University",CEIBS:"CEIBS","Curtin-Singapore":"Curtin Singapore","DePaul-University":"DePaul University","Grenoble-Ecole-de-Management":"Grenoble Ecole de Management","Hitotsubashi-University":"Hitotsubashi University","Hong-Kong-University---Faculty-of-Business-and-Economics":"Hong Kong University - Faculty of Business and Economics","IE-Business-School":"IE Business School","IESE-Business-School":"IESE Business School","IIM-Ahmedabad":"IIM Ahmedabad","IIM-Bangalore":"IIM Bangalore","IMI-Delhi":"IMI Delhi","Imperial-College":"Imperial College","Indian-Institute-of-Management-Tiruchirappalli--IIM-Trichy-":"Indian Institute of Management Tiruchirappalli (IIM-Trichy)","MIP-Politecnico-di-Milano-School-of-Management":"MIP Politecnico di Milano School of Management","Nanyang-Business-School":"Nanyang Business School","Nanyang-Technological-University--Nanyang-Business-School":"Nanyang Technological University, Nanyang Business School","National-University-of-Singapore--NUS-Business-School":"National University of Singapore, NUS Business School","Singapore-Management-University":"Singapore Management University","Sir-John-Cass-Business-School":"Sir John Cass Business School","SungKkyunkwan-University--SKK-Graduate-School-of-Business":"SungKkyunkwan University, SKK Graduate School of Business","The-Hong-Kong-University-of-Science---Technology--School-of-Business---Management":"The Hong Kong University of Science & Technology, School of Business & Management","University-of-Essex":"University of Essex","University-of-Greenwich":"University of Greenwich","University-of-St--Gallen":"University of St. Gallen","University-of-Technology-Sydney-Faculty-of--Business":"University of Technology Sydney Faculty of Business","Vlerick-Leuven-Gent-Management-School":"Vlerick Leuven Gent Management School","Test-Education---FT":"Test Education - FT","Duke-University-s-Fuqua-School-of-Business":"Duke University's Fuqua School of Business"};
return L[K]
}function d(L,M,N,K){return $.ajax({cache:!!K,data:M,dataType:"jsonp",global:false,jsonpCallback:"p13n_"+N+"_callback",timeout:p.timeout,type:"GET",url:L})
}function C(M,L,P){M=M&&M instanceof Array?M:[];
var R=null,N=0,K=M.length,Q=0,O=[];
while(Q<L&&N<K){R=P(M[N]);
if(R){O.push(R);
Q++
}N++
}return O
}function n(K){return $.Deferred().reject(undefined,K).promise()
}return a
}());
var P13n=P13n||{};
P13n.toolsprompt=(function(){var d={serviceBaseUrl:"",timeout:10000},b=d,g="eligibleForNbeToolsPrompt",p=".p13n.tp",a="ac",e=1000*60*60*6,q=$(window);
var i={reqDecisions:function k(r,t,u,s){if(typeof r!=="string"||!r.match(/^\d+$/)){return $.Deferred().reject(undefined,"Not a valid user").promise()
}var v={pageType:t||"",articleCount:u||"0",slider:s||""};
return $.ajax({url:(b.serviceBaseUrl?b.serviceBaseUrl:"/")+"decisions/"+r,type:"GET",global:false,dataType:"jsonp",timeout:b.timeout,data:v,cache:false,jsonpCallback:"p13n_ta_callback"})
},getErightsId:function c(){var r=P13n.cookie("FT_U");
if(!r){return null
}var s=r.match(/_EID=([^_]*)_/);
return(s)?(s[1]||null):null
},getPageType:function n(){return FT.isPage&&FT.isPage.Story===1?"article":"page"
},getClock:function l(){return +new Date()
},getArticleCount:function m(){var r=P13n.cookie(a)||"0";
return parseInt(r,10)
},getPropensitySlider:function h(){var s=P13n.cookie("AYSC");
if(!s){return""
}var r=s.match(/_47([^_]*)/);
return r&&r.length>1?r[1]:""
},saveVisitInfo:function j(){var r=new Date(i.getClock()+e);
var s=i.getArticleCount();
s=i.getPageType()==="article"?s+1:s;
if(!i.getErightsId()){s=0
}P13n.cookie(a,s,{expires:r,path:"/"})
},createDecisionHandler:function f(s,u,r){return function t(w){var x=w[s],v=x?u:r;
v&&v(x)
}
},init:function o(z){if(z){b=$.extend(true,{},d,z)
}i.saveVisitInfo();
var w=function(){q.trigger("show"+p)
},u=function(){q.trigger("dontshow"+p)
},v=function(){u()
},y=i.createDecisionHandler(g,w,u),x=i.getErightsId(),s=i.getPageType(),r=i.getArticleCount(),t=i.getPropensitySlider();
i.reqDecisions(x,s,r,t).then(y,v)
}};
return i
}());
var P13n=P13n||{};
P13n.tracking=(function(){var a=function a(e,d){this.setOptions(d);
this.structure={};
this.track=typeof e==="function"?e:function(){}
};
a.prototype={setOptions:function(d){this.options=$.extend(true,this.options||{},d)
},handleEvent:function(d,e){var f=this.getValue(d);
if(!f){return
}this.track(f,e)
},getValue:function(d){if(!this.isRegistered(d.type,d.namespace)){return null
}return this.structure[d.namespace][d.type]
},isRegistered:function(e,d){try{return !!this.structure[d][e]
}catch(f){}return false
},register:function(e,d){var f={};
f[e]=d;
this.structure=$.extend(true,this.structure,f)
}};
var c=new a(function(h,g){if(!h){return
}var d=this.options.erightsId,f=new Image(),e=g&&typeof g.num==="number"&&!isNaN(g.num)?g.num:null,k=g&&typeof g.text==="string"?g.text:null,i=Math.floor(Math.random()*100).toString()+(+new Date()).toString().substr(4),j=(this.options.serviceBaseUrl?this.options.serviceBaseUrl:"/")+"track?event="+h+"&_="+i;
if(d){j+=("&erightsId="+d)
}if(k){j+=("&text="+k)
}if(e!==null){j+=("&num="+e)
}f.src=j
});
var b=new a(function(d){window.SiTrackLink&&window.SiTrackLink(d)
});
return{EventTracker:a,p13nTracker:c,iJentoLinkTracker:b}
}());

(function(){if(typeof P13n.loaded!=="undefined"&&P13n.loaded===true){return
}P13n.loaded=true;
var e,j=[];
var s=$(window),m=$("#p13n_script"),a=m.attr("data-base"),r=a+"assets/",q=m.attr("data-features"),f=m.attr("data-enableSections"),d=m.attr("data-enableOntotextSections"),o=m.attr("data-loggingOptions"),h=P13n.features,k=P13n.logging,b=P13n.recommends,c=P13n.toolsprompt.getErightsId(),g=P13n.toolsprompt.getPageType()==="article",v=["show.p13n.tp","dontshow.p13n.tp","open.p13n.tp","close.p13n.tp","continue.p13n.tp","display.p13n.rr","nodisplay.p13n.rr","read.p13n.rr"],x=e;
u();
k.init(o);
h.init([["toolsPrompt","a",true],["toolsPromptView","b",true],["toolsPromptViewAlways","c",false],["tracking","d",true],["articleRecommends","e",true],["whitelistOff","k",false],["displayRecommends","m",true],["whitelistEmpty","n",false]]);
h.loadOverrides(q);
if(g&&h.tracking){var i=P13n.tracking.p13nTracker,p=P13n.tracking.iJentoLinkTracker;
i.setOptions({serviceBaseUrl:a,erightsId:c});
i.register("p13n.tp",{open:"nbeToolsPromptDisplayed",close:"nbeToolsPromptClosed","continue":"nbeToolsPromptContinue"});
i.register("p13n.rr",{display:"articleRecommendsDisplayed",nodisplay:"articleRecommendsNotDisplayed",read:"articleRecommendsClicked"});
p.register("p13n.tp",{"continue":"bottomSection/p13nNbeToolsPrompt/NbeToasterPopup/0/1"});
s.on(v.join(" "),function(y,z){i.handleEvent(y,z)
});
s.on("continue.p13n.tp",$.proxy(p.handleEvent,p))
}if(g&&h.articleRecommends&&window.articleUUID!=="undefined"){if(h.whitelistOff){x=e
}else{if(h.whitelistEmpty){x=j
}}b.setOptions({whitelist:x,serviceBaseUrl:a});
b.doArticleRecommends(c,window.articleUUID,h.displayRecommends);
if($.isReady){w()
}else{$(w)
}}function w(){if(typeof FT.analytics!=="undefined"&&typeof FT.analytics.clickPositionReporter!=="undefined"){FT.analytics.clickPositionReporter.init()
}}function l(z,C,A){var y=r+"js/modules/"+z+".min.js";
if($('script[src="'+r+"js/modules/"+z+'.min.js"]').length>0){return $.Deferred().reject().promise()
}if(C&&A){var B=C;
C=function(){$(B)
}
}return $.ajax({url:y,async:false,global:false,type:"GET",dataType:"script",cache:true,timeout:10000,success:C})
}function t(){l("toolsprompt-view",function(){$(function(){if(typeof P13n.toolsprompt.view!=="undefined"){P13n.toolsprompt.view.open({relativeTo:"#page-container",assetsBaseUrl:r})
}})
})
}function u(){if(!f){return
}x=f.split(",")
}if(h.toolsPromptViewAlways){t()
}if(h.toolsPrompt){s.on("show.p13n.tp",function n(){if(h.toolsPromptView){t()
}});
P13n.toolsprompt.init({serviceBaseUrl:a})
}}());
