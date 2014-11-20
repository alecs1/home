var cookieConsent=cookieConsent||{};
cookieConsent.showCookieMessage=function(){var a=cookieConsent.config,f=cookieConsent.manageCookies,c=cookieConsent.listeners,b=cookieConsent.html,e=cookieConsent.tracking,d=b.elReferences=b.setupDomReferences();
if(f.cookiesAreDisabled()){if(a.displayDisabledLogic()){b.insertCookieDisabled()
}}else{if(a.displayOverlayLogic()){c.attachSubmitHandlerTo(d.cookieConsentOverlay);
b.showCookieConsentOverlay();
f.setConsentStatus("seen");
if(typeof e.track==="function"){e.track(a.tracking.messageSeen)
}}}};
cookieConsent.config={cookieName:"cookieconsent",consentType:"implied",countryList:["ch","sj","al","ad","at","be","bg","cy","dk","gi","gr","hu","is","ie","mt","mc","no","pl","pt","es","tr","rs","ee","lv","lt","si","ua","fr","de","sk","li","se","fi","om","ro","lu","it","nl","gb","cz","ru","hr","ba","by","md","mk","me","je","im","gg"],corporateExclusionList:["FTQA23456"],disableCountryDetection:true,disableCorporateDetection:false,displayOverlayLogic:function(){var f=false,e=cookieConsent,g=e.config.cookieName,a=e.config,b=a.consentType,i=e.manageCookies.getCookie(g),h=e.utility.isIeSix(),c=e.manageCookies.surpressOverlayFromList(e.manageCookies.getAYSCParam("15"),a.countryList,a.disableCountryDetection),d=e.manageCookies.surpressOverlayFromList(e.manageCookies.getAYSCParam("27"),a.corporateExclusionList,a.disableCorporateDetection);
if(i!=="accepted"&&b==="implied"&&c&&d&&h===false){f=true
}return f
},displayDisabledLogic:function(){var a=false;
if(FT&&FT.isPage&&FT.isPage.Front===1){a=true
}return a
},tracking:{messageSeen:"sawcookieconsent=true",cookiesAccepted:"acceptcookieconsent=true"}};
cookieConsent.listeners=(function(){function d(){cookieConsent.manageCookies.setConsentStatus("accepted");
cookieConsent.tracking.track(cookieConsent.config.tracking.cookiesAccepted);
cookieConsent.html.hideCookieConsentContainer();
return false
}function b(f,e){f.onclick=e
}function c(f){if(f.keyCode===32||f.keyCode===13){d()
}}function a(e){e.onsubmit=d;
if(document.addEventListener){document.addEventListener("keydown",c,false)
}else{document.attachEvent("keydown",c)
}}return{attachSubmitHandlerTo:a,attachClickHandlerTo:b,overlaySubmit:d,keySubmit:c}
}());
cookieConsent.utility=(function(){return{debug:function(a){if(window.console&&console.log){console.log(a)
}},getInternetExplorerVersion:function(){var a,b,c;
c=-1;
if(navigator.appName==="Microsoft Internet Explorer"){a=navigator.userAgent;
b=new RegExp("MSIE ([0-9]{1,}[.0-9]{0,})");
if(b.exec(a)!==null){c=parseFloat(RegExp.$1)
}}return c
},isIeSix:function(){var a=false;
if(this.getInternetExplorerVersion()===6){a=true
}return a
},isIeEightAndBelow:function(){var a=false,b=this.getInternetExplorerVersion();
if(b<=8&&b>=0){a=true
}return a
},isIos:function(){return window.navigator.userAgent.match(/(iPad|iPhone|iPod)/i)?true:false
},isInArray:function(b,c){var d=false,a;
if(b&&c){for(a=0;
a<c.length;
(a++)){if(b===c[a]){d=true;
break
}}}return d
}}
}());
cookieConsent.tracking=(function(){var b=function(c){if(typeof FT==="object"&&typeof FT.analytics==="object"&&typeof FT.analytics.siteIntelligence==="object"&&typeof FT.analytics.siteIntelligence.trackData==="function"){FT.analytics.siteIntelligence.trackData(c)
}};
function a(c){if(typeof b==="function"){b(c)
}}return{track:a,trackingCall:b}
}());
cookieConsent.manageCookies=(function(){var f=cookieConsent.config.cookieName;
function e(h){var j=new Date();
j.setTime(j.getTime()+(730*24*60*60*1000));
var g=j.toUTCString(),k=document.domain.match(/ft.com$/)?".ft.com":document.domain,i;
if(cookieConsent.utility.isIeSix()){i=f+"="+escape(h)+"; expires="+g+"; path=/"
}else{i=f+"="+escape(h)+"; expires="+g+"; path=/; domain="+k
}document.cookie=i
}function a(i){var g=new RegExp(i+"=([^;]*)"),h=document.cookie.match(g)?RegExp.$1:"";
return h
}function b(g){var h=a("AYSC"),i;
if(!h){return
}i=h.match(new RegExp("_"+g+"([^_]*)_"));
return(i)?i[1]:undefined
}function d(){var g=false,h="cookieTest=test; path=/";
document.cookie=h;
if(document.cookie.indexOf("cookieTest")===-1){g=true
}else{document.cookie=h+"; path=/; expires=Fri, 1 Aug 1984 20:47:11 UTC"
}return g
}function c(g,l,k){var h=true,j=cookieConsent.utility.isInArray,i=j(g,l);
if(k===false&&i===true){h=false
}return h
}return{setConsentStatus:e,cookiesAreDisabled:d,surpressOverlayFromList:c,getCookie:a,getAYSCParam:b}
}());
cookieConsent.css=(function(){var e={allBrowsers:["#cookie-consent-container {","display:none;","width:100%;","min-width:993px;","background-color:#161616;","font-size:12px;","color:#fff;","}","#cookie-consent-container a,","#cookie-consent-container a:hover {","color:#fff1df;","font-weight:bold;","}","#cookie-consent-container a:hover {","text-decoration:underline;","}","#cookie-consent-container p { ","font-size:13px;","}","#cookie-consent-wrapper { ","margin:0 auto;","width:973px;","text-align:center;","}","#cookie-consent-overlay {","text-align:left;","background:url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABcAAAAXCAIAAABvSEP3AAAACXBIWXMAAAsTAAALEwEAmpwYAAACWUlEQVR4nK2VP2gaURzHf57xCu8dtJTcuZkYC1WilaYNFVqhcdDJQZe62KGDtEuaBjNYbAk0NFBCC11aOnSoUMyig1l0sAWrcKCVnIpHwMS41QhJoHdDbunw5PDvoM13Ot7v9/3c7/37PQ3HcdAvBiO3y3n/3h3borl3vFIT83wpk839leQBi2aAEgz4/F4PRgjGSJLlRCodiydHUxiMdrYiC/NzANA+6SRS6f1qvXHUIlGT0WC3WvxeD8fOAsBh8zgc3VaL6lIYjGJf3mOEJFmOxZOJVHpcLX6vJxjwkcxgaJ2AtBhjAPj47rWeZSVZDkffFvjf4xAAUD9oFMvCitOBEVpesu2lf3QpwYDv4QMHQahTILJbzW6XUwPwp91RB0/PzglIz7KgAaEqUgxGfq8HAGLx5ACCwWgzshZ85NuMrDG4b70bRy2ywH6vh8GIcrucGCGynMP1k80auWWJVLp90sEIuV1O7fNnT/TcbGw3WT9oDORdKEqj2bp+7eqnr98HylR19/atKzQ9Q47WfrU+MqnAlwp8aWRIddkWzTPqPEfmZZLfyIfb93g4qrqocf+ZSJdKMRkNU5hVF1WpiQBgt1qmoBBXpSZSeb4EAOTgTSriyvMlKpPNSbLMsbOTgsj9lmQ5k81pdTRN0zq71WK5eaNYFk7PzvtyNSDURKEmClWxd9hkNGyshmidbjexVyxXup3h84c3C/NzIy/ksExGw87WS4zQYfP46YtXoHaGn7/45SWbnmVXnA5FUYZvQ+9ENlZDBBGObl8oClxyr1P1v31X1RRvwD9zTB2grOo2NAAAAABJRU5ErkJggg==) no-repeat 3px 17px;","padding:10px 48px;","position:relative;","}","#cookie-consent-overlay button {","background:url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAACXBIWXMAAAsTAAALEwEAmpwYAAABu0lEQVR4nGMUExNjgAE3J1trc2NLMyO4yPFT546ePLtr32G4CCNEAw83V09LpZKCPAM2cO/Bw5Ka9i9fv0E18HBzLZ7Vx83FhVU1BHz99i02rejL129MDAwMPS2V+FUzMDBwc3H1tFQyMDAwB/p6eLs7QURv3bl/7NR5BgYGESFBBgaG+w+fLFu7+d27DypK8gwMDIICAi9fv2Fxd7KFG3Pzzv11m3cJCwtkJEaws7E3dU35++8fsj3uTrYsutoacL6aiqKwsMDLV287Jszm4uT88/cvCzOzl4s9XIGutgYTsgHqKooZiRHs7Gw/f/56/+GjoAB/U1W+orwMshoUDQwMDP/+/mdhZoawv33//vPXTzQFKBoeP30+c8HK799/cHNxQuyZMX/FzTv3kdUwW1uYiIuJQDhFNe2fv3wVFRbKTY2xtTK+cfve69fvrty4baCrxcPNxcDAcPnqDaadyNHOyMjKylKQGa+moqiiqJAaFy4oyP/5y9ffv39DFOzcd5hRTExsRn8zrkTx4tWb379/S4iJsLKy3nvwMKOwlqyk8eXrt9i0onsPHuJSfe/BQ4hqBnhqhQBikjcAag20dJxdteYAAAAASUVORK5CYII=) no-repeat;","position:absolute;","width:16px;","height:16px;","top:21px;","right:0;","border:0;","text-indent:-9999px;","}","#cookie-modal-bg {","background-color:rgb(0,0,0);","width:100%;","position:fixed;","*position:absolute;","top:0;","left:0;","z-index:199999;","display:none;","opacity:0.3;","filter:alpha(opacity=30);","height:100%;","}","#cookie-disabled-overlay {","background-image: url(data:image/gif;base64,R0lGODlhDQBGAOYAAPPm1vTo1/Xo2O3i0u7i0/Lm1fLm1uvg0fXp2PHl1Orfz/bp2Ozh0u3h0e7j0+rez/Hk1Ore0Pbp1/Dk0/bo2PXn1/Dj0/bo1+7j0vXp1+rg0Onfz+/k1Onf0One0PDl1O/j1PDj1PHk1fTo1uvh0e/k0/Ll1uvf0fPm1e3i0/Pn1/Hm1e3i0e3g0ezg0u/i0/Dk1e7h0vLn1u3h0+3g0uvfz/Xo1uzg0O/i0u7i0u3h0vTn1+vf0O/j0+zh0fLl1fTn1vHl1evg0Ozg0fDk1PPn1vXo1+rf0OnezwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH/C1hNUCBEYXRhWE1QPD94cGFja2V0IGJlZ2luPSLvu78iIGlkPSJXNU0wTXBDZWhpSHpyZVN6TlRjemtjOWQiPz4gPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iQWRvYmUgWE1QIENvcmUgNS4wLWMwNjAgNjEuMTM0Nzc3LCAyMDEwLzAyLzEyLTE3OjMyOjAwICAgICAgICAiPiA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPiA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIiB4bWxuczp4bXA9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC8iIHhtbG5zOnhtcE1NPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvbW0vIiB4bWxuczpzdFJlZj0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL3NUeXBlL1Jlc291cmNlUmVmIyIgeG1wOkNyZWF0b3JUb29sPSJBZG9iZSBQaG90b3Nob3AgQ1M1IE1hY2ludG9zaCIgeG1wTU06SW5zdGFuY2VJRD0ieG1wLmlpZDozNjA1NDkxM0ExQkUxMUUxQjAwRTk5NjgwRTU3ODNBMCIgeG1wTU06RG9jdW1lbnRJRD0ieG1wLmRpZDozNjA1NDkxNEExQkUxMUUxQjAwRTk5NjgwRTU3ODNBMCI+IDx4bXBNTTpEZXJpdmVkRnJvbSBzdFJlZjppbnN0YW5jZUlEPSJ4bXAuaWlkOjM2MDU0OTExQTFCRTExRTFCMDBFOTk2ODBFNTc4M0EwIiBzdFJlZjpkb2N1bWVudElEPSJ4bXAuZGlkOjM2MDU0OTEyQTFCRTExRTFCMDBFOTk2ODBFNTc4M0EwIi8+IDwvcmRmOkRlc2NyaXB0aW9uPiA8L3JkZjpSREY+IDwveDp4bXBtZXRhPiA8P3hwYWNrZXQgZW5kPSJyIj8+Af/+/fz7+vn49/b19PPy8fDv7u3s6+rp6Ofm5eTj4uHg397d3Nva2djX1tXU09LR0M/OzczLysnIx8bFxMPCwcC/vr28u7q5uLe2tbSzsrGwr66trKuqqainpqWko6KhoJ+enZybmpmYl5aVlJOSkZCPjo2Mi4qJiIeGhYSDgoGAf359fHt6eXh3dnV0c3JxcG9ubWxramloZ2ZlZGNiYWBfXl1cW1pZWFdWVVRTUlFQT05NTEtKSUhHRkVEQ0JBQD8+PTw7Ojk4NzY1NDMyMTAvLi0sKyopKCcmJSQjIiEgHx4dHBsaGRgXFhUUExIREA8ODQwLCgkIBwYFBAMCAQAAIfkEAAAAAAAsAAAAAA0ARgAAB/+ASIKDhIVIG4Yegh4PghGCjYSRj4QKgxuWR0geHY8bRxseoZqdEQodnRuICg8dSA9HR5ywtBGxsEeNCkestry7Eb6ssbu7scQ1ChpHPMfLsTwazcc8Qss82MdCQtjYJ0cn3Njb29HNGtvh5NgH49sa7dUHQgft9Nv1+PdD9PND/0NIDLnx7wDAg/9I1BPog6GPIQ99PIT4TyJAiQwkuoDYgEGLFjoYiGTAUaIPHUMYuMDYgAZGHRhFNpiZsYGOmzZ1sLA5cyaLm0B1DAA6dIBRozpy5DgqNIXQASliKF06Y8DSHCmWElA6AEOOrTm8Etg6loBXDBgchDX7AiyOsUr/exBwAHesgxcOHPTIu7fvXgJ+A/cYTJgDiBJEJvQ4vHgxhxAWiFjgYKHHhBAlQFiYUDkEByIhEkMmQnnChMQlTE/4TNoCBAhEYsuODXs2EdgwEhD5QJv3hwS6P0AIEhvGBxHDgwR5TSTBaxEJRAQBnkA5dOtBfmSfrry79urVu3tP8KO8+RU/ChQw/wO9+QJB1P8woN6AARPrDZRfj98ECvUAFHBfevTNZ58BKNgHgIIALNjgf0U0WAR9BjQIQBFFyGBhhBheyOGFHmIoYogRAgFEESeeiCKKKbKogolA7ADEiyruIKOMJhaxQwAm2viijkAEEMCOO45g45E7BlBBeAUjBDBCjEM2acSUStq4pIw2GDHklDssGcCUWn6pJZhCLjmmAGCmqSaaU6KJJgIIZJCBERcgYAScAghwQZ57ZqCnAHFKwKafekpAgREZ1HmBBBJMKQECFCywgKCALgCppYxmECkCCwjA6AIURCqApJBeYKmkFEAaCAA7);","background-position:bottom;","background-repeat: repeat-x;","padding:82px 20px 20px 20px;","margin-bottom: 10px;","color: #666;","font-size: 13px;","padding-bottom: 17px;","}","#cookie-disabled-overlay h3 {","color:#FF0000;","float:left;","padding:2px 20px 0 0;","font-weight:bold;","font-size:14px;","height:31px;","line-height:1.2em;","}","#cookie-disabled-overlay .disabled-cookie-icon {","background: url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACMAAAAfCAYAAABtYXSPAAAFbUlEQVR42sVXa2xURRQeHpWCAoo/FF8IkvqIPxoBFSKSqpEfvqJCQoIaRBMoP6SoEYwYCOIjBkvUSFKTVjEVk/KIlWgKihUMFYpY2W0rNlH7/sXu3nv3/egez8yZmTt3e7e78Y8/TmbunPOd882Zc+6dyy4NXIBL/QEUHAcM6TdErwcMPWFi21+B7ML5EN/0QtmYYnGYUoYGzDEAIVwXIpwViMSEzxyHfGUlAGOQnzYNrO9aSmImisP4Yqg/4GU7IB32y5159AFQmOSqx4nIpElC0jX3lcRMFIeFjFRqtgOBgjQHXIdSIm2HID91qiCRevIRyE+ZAvnLKsD+/JOimFJxmGkckkxdMZ4HTf0FyCxfKojk5s+DUN+vkFl2Nz1j/RTDlIrDXKDpIOAv0ia6bw9lAsVp/FhgrK+baW3yZIjt3DYOU04cJpSDhqGfoC48SBK6eAZyN91ANfLgCtfmny5xXKKGLp8B4a6TLqbAV7E4RGYgKB4IGCwABCFskEls3gB5HhC7KPJjqwcTPt8O+RnThT753BqN0YRKxGHhIQUIapAydp9pHuHBeNFisNTaVb6Y+LY66i5sdfvYIVdfRhyXzJAB8ACDep56+jHKCj+GvnP+mD87IXfj9XSMD9e4PsqIwyKFQYeChiE5ieBoH/2KChSDxN/fKXTW6TZI1G2ExJZaMTotjWI9tmeXKGR+lE5zA2W1jDiML0bEQ1AbRIbUboLaQeaeRYJI9rYqUBinpUkUrJJEXS1hLp6F7OJqkcVc1ULtq1QcpiYRaaRGcx5rqKedYmaiXzZocLSQDGZIYbhOZ3LX61BOHBaRD36iDMeuniN2mXlohUdfSCa5pVZjItjq6ZUPCNzYnKvA+q29ZBwk0y0XcRyWIuZYEzhP1T5PRYu7tE8ddW1xtDuOjSPDMdYwObc6T8DYrJkiO8kXn50wDsewyDAZWBzMFXreDTb/KstWTm5cp/UmxnNMO14TazyQJXeb5O8lJMNJ2ccP+8ZRGKYWlAPlhBunH11JacZjUuxNJxxjkuHHJvwIXzTawZ8ht+BmQSiDre4XR2GYJSfaaJgkeuQL/a2Jf/SuXtfBipAhG9cPl/gHb7mt3trsiaOPlR+TYG8EsKXkbq+iVq6+U+usAjvLl8x4W6sPX4R33CqynF1U7Ylj2jFrZHyAOH9p8Vc6ZiZ2eL/U94CtbA1MfuYV48i4QVxMrPFDyg76Tby3w3dzzDKAIshf58Xrnu8ija9/k4DYzUgPmJjsvYtdMgebhL0v5u8uyC5dQjV43bVgd/3k2khhtk4Z7SK1fi1dA7CLnN9PCkdmSu0RN8UcE8WPYazlM5QmslXig4me+hbyV86m22Htek1UCXPBPWTMi5a38taXNGPTpvA59fImkZ306ifA7u0oiUltWEetji9Cp6PNsO8mMo40zN6/TKZxrl7zODaE6xP1uz0FzFu3FMbBa8jY3Gvoq77mKU8c5shJfP8+KjDeyvgt8nNmOhVHilkxyfAMlcLwMbV1M8XC4o+2HXSPyZGGYwvmUSvjRVutCQejcj4qxdD5ZaYURkj3acjdMp9afcld4OCFnGOYM9oLyXe2Uytj0ca+P+IFjsqdmY4Ro55VzWSwZpzeX8rC8HmCX+pVq+PcEWT+OAv5igr6KuNVUoBGer1ODGI0V/peiI72/DcM3md4VsSdB7PkdHcASz+zmloZuyj55quQfPsNSOzdDYlP96LU09jApV4/x/FOE/vmAMTw9kdyQEhczdtbIYpf7GjnDxA9x+UEjZ00d/DyxYnFsF543fAM8Tpi/C9QkPkfRcRHQixbsxzy2PMemT2L3sLFZHql+JUtKvIDW1T8yCDuX0ndqzdoE8n2AAAAAElFTkSuQmCC) no-repeat;","float:left;","margin-right:20px;","margin-top:3px;","width:35px;","height:31px;","}"],ieSpecific:["#cookie-modal-bg {","height:expression(0+((e=document.body.scrollHeight)?e:document.body.scrollHeight)+'px');","}","#cookie-disabled-overlay {","padding:20px 20px 17px 20px;","margin-top:-22px;","}","#cookie-disabled-overlay .disabled-cookie-icon {","padding-top:5px;","}"]};
function b(f){return e[f].join("")
}function a(j,i){var f=document.createElement("style"),h=b(j),g=document.getElementsByTagName("head")[0];
f.setAttribute("type","text/css");
if(f.styleSheet){f.styleSheet.cssText=h
}else{f.textContent=h
}g.appendChild(f);
f.onload=function(){if(typeof(i)==="function"){i()
}}
}function d(f,g){if(!window.getComputedStyle){return{getPropertyValue:function(i){var h=/(\-([a-zA-Z]){1})/g;
if(i=="float"){i="styleFloat"
}if(h.test(i)){i=i.replace(h,function(){return arguments[2].toUpperCase()
})
}return f.currentStyle[i]?f.currentStyle[i]:null
}}
}else{return window.getComputedStyle(f,g)
}}function c(l){var m=cookieConsent.css.getComputedStyle(l).getPropertyValue("background-position"),k={top:0,bottom:100,left:0,right:100};
if(m===null){var i,g,h,f;
i=cookieConsent.css.getComputedStyle(l).getPropertyValue("background-position-x")||"0px";
g=cookieConsent.css.getComputedStyle(l).getPropertyValue("background-position-y")||"0px";
h=i.match(/([0-9]+|left|right)(.*)/);
f=g.match(/([0-9]+|top|bottom)(.*)/);
return{x:!isNaN(h[1])?parseInt(h[1],10):k[h[1]],xMetric:!isNaN(h[1])?h[2]:"%",y:!isNaN(f[1])?parseInt(f[1],10):k[f[1]],yMetric:!isNaN(f[1])?f[2]:"%"}
}else{if(m){var j=m.match(/([0-9]+|top|bottom)([^ ]*) ([0-9]+|left|right)(.*)/);
return{x:!isNaN(j[1])?parseInt(j[1],10):k[j[1]],xMetric:!isNaN(j[1])?j[2]:"%",y:!isNaN(j[3])?parseInt(j[3],10):k[j[3]],yMetric:!isNaN(j[3])?j[4]:"%"}
}}}return{getStyles:b,insertStyles:a,getComputedStyle:d,getBackgroundPosition:c}
}());
cookieConsent.html=(function(){var f={overlay:['<div id="cookie-consent-container" style="display: none">','<div id="cookie-consent-wrapper">','<form id="cookie-consent-overlay">','<p>By continuing to use this site you consent to the use of cookies on your device as described in our <a href="http://www.ft.com/cookiepolicy">cookie policy</a> unless you have disabled them. You can change your <a href="http://www.ft.com/cookiemanagement">cookie settings</a> at any time but parts of our site will not function correctly without them.</p>','<button type="submit">Close</button>',"</form>","</div>","</div>"],modalBg:['<div id="cookie-modal-bg"></div>'],cookiesDisabled:['<div id="cookie-disabled-overlay">','<div class="disabled-cookie-icon"></div>','<h3 class="cookie-disabled">COOKIES <br/>DISABLED</h3>',"<p>Some areas of the site might not function properly without cookies. To get the most from FT.com, please enable your cookies.</p>",'<p>For help with this, view our information pages on <a href="http://www.ft.com/cookiemanagement">How to manage cookies.</a></p>',"</div>"]},j={};
function i(){var l=cookieConsent.html.elReferences.cookieConsentContainer;
l.style.display="none";
if(document.removeEventListener){document.removeEventListener("keydown",cookieConsent.listeners.keySubmit,false)
}else{document.detachEvent("keydown",cookieConsent.listeners.keySubmit)
}document.body.style.backgroundPosition=""
}function h(){var l=cookieConsent.html.elReferences.cookieConsentContainer;
l.style.display="block";
var m=cookieConsent.css.getBackgroundPosition(document.body);
if((m.y>0&&m.yMetric==="px")||(m.y===0)){document.body.style.backgroundPosition=m.x+m.xMetric+" "+(m.y+parseInt(l.scrollHeight,10))+"px"
}}function d(){var l=cookieConsent.html.elReferences.cookiesDisabledOverlay;
l.style.display="block";
l.parentNode.style.display="block"
}function c(){var l={};
l.root=document.body;
l.cookieConsentContainer=document.getElementById("cookie-consent-container");
l.cookieConsentOverlay=document.getElementById("cookie-consent-overlay");
l.cookiesDisabledOverlay=document.getElementById("cookie-disabled-overlay");
l.cookiePolicyLink=document.getElementById("ccf-cookiepolicylink");
l.modalBg=document.getElementById("cookie-modal-bg");
l.cookieDisabledOverlayClose=document.getElementById("cdf-close");
return l
}function e(l){return f[l].join("")
}function k(n,m){var o=document.getElementById(m),p=o.innerHTML,l=e(n);
o.innerHTML=l+p
}function g(m,n,l){if(!l){l=false
}setTimeout(function(){a(m,n,l)
},100)
}function a(n,o,m){var l;
if(m&&m!==false){if(document.getElementById(m)===null){g(n,o,m);
return false
}k(n,m)
}else{if(document.body===null){g(n,o);
return false
}l=document.createElement("div");
l.id=n+"-wrapper";
l.innerHTML=e(n);
document.body.insertBefore(l,document.body.firstChild)
}if(o&&typeof o==="function"){o()
}}function b(){var l,n,m,o;
o=document.getElementById("navigation");
if(o!==null){n=o.parentNode;
l=document.createElement("div");
l.innerHTML=e("cookiesDisabled");
m=document.getElementById("navigation").nextSibling;
n.insertBefore(l,m)
}else{setTimeout(function(){b()
},100)
}}return{getHtml:e,insertContent:a,pollDom:g,elReferences:j,setupDomReferences:c,hideCookieConsentContainer:i,showCookiesDisabledOverlay:d,showCookieConsentOverlay:h,insertCookieDisabled:b,updateElementContent:k}
}());
cookieConsent.browser=(function(){var e={width:null,height:null};
function d(){if(document.body&&document.body.offsetWidth){e.width=document.body.offsetWidth;
e.height=document.body.offsetHeight
}if(document.compatMode==="CSS1Compat"&&document.documentElement&&document.documentElement.offsetWidth){e.width=document.documentElement.offsetWidth;
e.height=document.documentElement.offsetHeight
}if(window.innerWidth&&window.innerHeight){e.width=window.innerWidth;
e.height=window.innerHeight
}return e
}function b(){return this.getViewport().height
}function c(){return this.getViewport().width
}function a(g,h){var j=g.document,f=false,i=function(){if(!f){f=true;
h()
}};
(function(){try{j.documentElement.doScroll("left")
}catch(k){setTimeout(arguments.callee,50);
return
}i()
}());
j.onreadystatechange=function(){if(j.readyState==="complete"){j.onreadystatechange=null;
i()
}}
}return{getViewport:d,height:b,width:c,iEContentLoaded:a}
}());
cookieConsent.init=function(){if(cookieConsent.utility.isIos()!==true){var a=cookieConsent.config.containerId;
cookieConsent.css.insertStyles("allBrowsers",function(){if(cookieConsent.utility.isIeEightAndBelow()){cookieConsent.css.insertStyles("ieSpecific")
}cookieConsent.html.insertContent("overlay",function(){cookieConsent.showCookieMessage()
},a)
})
}};