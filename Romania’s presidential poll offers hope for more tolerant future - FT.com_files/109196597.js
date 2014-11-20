/* Unknown-Branch 14.11.18-1407-1407 (2014-11-18 22:07:04 GMT) */

rsinetsegs=['J07717_10554','J07717_11147','J07717_10545','J07717_10911','J07717_11004','J07717_11405','J07717_11406','J07717_11407','J07717_11630','J07717_0'];
var rsiExp=new Date((new Date()).getTime()+2419200000);
var rsiDom=location.hostname;
rsiDom=rsiDom.replace(/.*(\.[\w\-]+\.[a-zA-Z]{3}$)/,'$1');
rsiDom=rsiDom.replace(/.*(\.[\w\-]+\.\w+\.[a-zA-Z]{2}$)/,'$1');
rsiDom=rsiDom.replace(/.*(\.[\w\-]{3,}\.[a-zA-Z]{2}$)/,'$1');
var rsiSegs="";
var rsiPat=/.*_5.*/;
var i=0;
for(x=0;x<rsinetsegs.length&&i<20;++x){if(!rsiPat.test(rsinetsegs[x])){rsiSegs+='|'+rsinetsegs[x];++i;}}
document.cookie="rsi_segs="+(rsiSegs.length>0?rsiSegs.substr(1):"")+";expires="+rsiExp.toGMTString()+";path=/;domain="+rsiDom;
if(typeof(DM_onSegsAvailable)=="function"){DM_onSegsAvailable(['J07717_10554','J07717_11147','J07717_10545','J07717_10911','J07717_11004','J07717_11405','J07717_11406','J07717_11407','J07717_11630','J07717_0'],'j07717');} 