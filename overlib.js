<DIV ID="overDiv" STYLE="position:absolute; visibility:hidden; z-index:1000;"></DIV>	
<SCRIPT LANGUAGE=javascript>

<!--

// This file is for use with Smarty's {overlib_init} function. Place this where
// Smarty can access it, mostly likely the same location as Smarty.addons.php
		
//\//////////////////////////////////////////////////////////////////////////////////
//\  overLIB 3.33  --  This notice must remain untouched at all times.
//\  Copyright Erik Bosrup 1998-2001. All rights reserved.
//\
//\  By Erik Bosrup (erik@bosrup.com).  Last modified 2001-01-26.
//\  Portions by Dan Steinman (dansteinman.com). Additions by other people are
//\  listed on the overLIB homepage.
//\
//\  Get the latest version at http://www.bosrup.com/web/overlib/
//\
//\  This script is published under an open source license. Please read the license
//\  agreement online at: http://www.bosrup.com/web/overlib/license.html
//\  If you have questions regarding the license please contact erik@bosrup.com.
//\
//\  This script library was originally created for personal use. By request it has
//\  later been made public. This is free software. Do not sell this as your own
//\  work, or remove this copyright notice. For full details on copying or changing
//\  this script please read the license agreement at the link above.
//\
//\  Please give credit on sites that use overLIB and submit changes of the script
//\  so other people can use them as well. This script is free to use, don't abuse.
//\//////////////////////////////////////////////////////////////////////////////////

var INARRAY=1;
var CAPARRAY=2;
var STICKY=3;
var BACKGROUND=4;
var NOCLOSE=5;
var CAPTION=6;
var LEFT=7;
var RIGHT=8;
var CENTER=9;
var OFFSETX=10;
var OFFSETY=11;
var FGCOLOR=12;
var BGCOLOR=13;
var TEXTCOLOR=14;
var CAPCOLOR=15;
var CLOSECOLOR=16;
var WIDTH=17;
var BORDER=18;
var STATUS=19;
var AUTOSTATUS=20;
var AUTOSTATUSCAP=21;
var HEIGHT=22;
var CLOSETEXT=23;
var SNAPX=24;
var SNAPY=25;
var FIXX=26;
var FIXY=27;
var FGBACKGROUND=28;
var BGBACKGROUND=29;
var PADX=30;
var PADY=31;
var PADX2=32;
var PADY2=33;
var FULLHTML=34;
var ABOVE=35;
var BELOW=36;
var CAPICON=37;
var TEXTFONT=38;
var CAPTIONFONT=39;
var CLOSEFONT=40;
var TEXTSIZE=41;
var CAPTIONSIZE=42;
var CLOSESIZE=43;
var FRAME=44;
var TIMEOUT=45;
var FUNCTION=46;
var DELAY=47;
var HAUTO=48;
var VAUTO=49;
if(typeof ol_fgcolor=='undefined'){var ol_fgcolor="#CCCCFF";}
if(typeof ol_bgcolor=='undefined'){var ol_bgcolor="#333399";}
if(typeof ol_textcolor=='undefined'){var ol_textcolor="#000000";}
if(typeof ol_capcolor=='undefined'){var ol_capcolor="#FFFFFF";}
if(typeof ol_closecolor=='undefined'){var ol_closecolor="#9999FF";}
if(typeof ol_textfont=='undefined'){var ol_textfont="Verdana,Arial,Helvetica";}
if(typeof ol_captionfont=='undefined'){var ol_captionfont="Verdana,Arial,Helvetica";}
if(typeof ol_closefont=='undefined'){var ol_closefont="Verdana,Arial,Helvetica";}
if(typeof ol_textsize=='undefined'){var ol_textsize="1";}
if(typeof ol_captionsize=='undefined'){var ol_captionsize="1";}
if(typeof ol_closesize=='undefined'){var ol_closesize="1";}
if(typeof ol_width=='undefined'){var ol_width="200";}
if(typeof ol_border=='undefined'){var ol_border="1";}
if(typeof ol_offsetx=='undefined'){var ol_offsetx=10;}
if(typeof ol_offsety=='undefined'){var ol_offsety=10;}
if(typeof ol_text=='undefined'){var ol_text="Default Text";}
if(typeof ol_cap=='undefined'){var ol_cap="";}
if(typeof ol_sticky=='undefined'){var ol_sticky=0;}
if(typeof ol_background=='undefined'){var ol_background="";}
if(typeof ol_close=='undefined'){var ol_close="Close";}
if(typeof ol_hpos=='undefined'){var ol_hpos=8;}
if(typeof ol_status=='undefined'){var ol_status="";}
if(typeof ol_autostatus=='undefined'){var ol_autostatus=0;}
if(typeof ol_height=='undefined'){var ol_height=-1;}
if(typeof ol_snapx=='undefined'){var ol_snapx=0;}
if(typeof ol_snapy=='undefined'){var ol_snapy=0;}
if(typeof ol_fixx=='undefined'){var ol_fixx=-1;}
if(typeof ol_fixy=='undefined'){var ol_fixy=-1;}
if(typeof ol_fgbackground=='undefined'){var ol_fgbackground="";}
if(typeof ol_bgbackground=='undefined'){var ol_bgbackground="";}
if(typeof ol_padxl=='undefined'){var ol_padxl=1;}
if(typeof ol_padxr=='undefined'){var ol_padxr=1;}
if(typeof ol_padyt=='undefined'){var ol_padyt=1;}
if(typeof ol_padyb=='undefined'){var ol_padyb=1;}
if(typeof ol_fullhtml=='undefined'){var ol_fullhtml=0;}
if(typeof ol_vpos=='undefined'){var ol_vpos=36;}
if(typeof ol_aboveheight=='undefined'){var ol_aboveheight=0;}
if(typeof ol_caption=='undefined'){var ol_capicon="";}
if(typeof ol_frame=='undefined'){var ol_frame=self;}
if(typeof ol_timeout=='undefined'){var ol_timeout=0;}
if(typeof ol_function=='undefined'){var ol_function=Function();}
if(typeof ol_delay=='undefined'){var ol_delay=0;}
if(typeof ol_hauto=='undefined'){var ol_hauto=0;}
if(typeof ol_vauto=='undefined'){var ol_vauto=0;}
var ol_texts=new Array("Array Text 0", "Array Text 1");
var ol_caps=new Array("Array Caption 0", "Array Caption 1");
var otext="";
var ocap="";
var osticky=0;
var obackground="";
var oclose="Close";
var ohpos=8;
var ooffsetx=2;
var ooffsety=2;
var ofgcolor="";
var obgcolor="";
var otextcolor="";
var ocapcolor="";
var oclosecolor="";
var owidth=100;
var oborder=1;
var ostatus="";
var oautostatus=0;
var oheight=-1;
var osnapx=0;
var osnapy=0;
var ofixx=-1;
var ofixy=-1;
var ofgbackground="";
var obgbackground="";
var opadxl=0;
var opadxr=0;
var opadyt=0;
var opadyb=0;
var ofullhtml=0;
var ovpos=36;
var oaboveheight=0;
var ocapicon="";
var otextfont="Verdana,Arial,Helvetica";
var ocaptionfont="Verdana,Arial,Helvetica";
var oclosefont="Verdana,Arial,Helvetica";
var otextsize="1";
var ocaptionsize="1";
var oclosesize="1";
var oframe=self;
var otimeout=0;
var otimerid=0;
var oallowmove=0;
var ofunction=Function();
var odelay=0;
var odelayid=0;
var ohauto=0;
var ovauto=0;
var ox=0;
var oy=0;
var oallow=0;
var oshowingsticky=0;
var oremovecounter=0;
var over=null;
var ns4=(document.layers)? true:false;
var ns6=(document.getElementById)? true:false;
var ie4=(document.all)? true:false;
var ie5=false;
if(ie4){
if(navigator.userAgent.indexOf('MSIE 5')>0){
ie5=true;
}
if(ns6){
ns6=false;
}
}
if((ns4)||(ie4)||(ns6)){
document.onmousemove=mouseMove
if(ns4)document.captureEvents(Event.MOUSEMOVE)
}else{
overlib=no_overlib;
nd=no_overlib;
ver3fix=true;
}
function no_overlib(){
return ver3fix;
}
function overlib(){
otext=ol_text;
ocap=ol_cap;
osticky=ol_sticky;
obackground=ol_background;
oclose=ol_close;
ohpos=ol_hpos;
ooffsetx=ol_offsetx;
ooffsety=ol_offsety;
ofgcolor=ol_fgcolor;
obgcolor=ol_bgcolor;
otextcolor=ol_textcolor;
ocapcolor=ol_capcolor;
oclosecolor=ol_closecolor;
owidth=ol_width;
oborder=ol_border;
ostatus=ol_status;
oautostatus=ol_autostatus;
oheight=ol_height;
osnapx=ol_snapx;
osnapy=ol_snapy;
ofixx=ol_fixx;
ofixy=ol_fixy;
ofgbackground=ol_fgbackground;
obgbackground=ol_bgbackground;
opadxl=ol_padxl;
opadxr=ol_padxr;
opadyt=ol_padyt;
opadyb=ol_padyb;
ofullhtml=ol_fullhtml;
ovpos=ol_vpos;
oaboveheight=ol_aboveheight;
ocapicon=ol_capicon;
otextfont=ol_textfont;
ocaptionfont=ol_captionfont;
oclosefont=ol_closefont;
otextsize=ol_textsize;
ocaptionsize=ol_captionsize;
oclosesize=ol_closesize;
otimeout=ol_timeout;
ofunction=ol_function;
odelay=ol_delay;
ohauto=ol_hauto;
ovauto=ol_vauto;
if((ns4)||(ie4)||(ns6)){
oframe=ol_frame;
if(ns4)over=oframe.document.overDiv
if(ie4)over=oframe.overDiv.style
if(ns6)over=oframe.document.getElementById("overDiv");
}
var c=-1;
var ar=arguments;
for(i=0;i < ar.length;i++){
if(c==0){
if(ar[i]==1){c=1;}
if(ar[i]==2){c=2;}
if(ar[i]==3){c=z3(ar[i]);}
if(ar[i]==4){c=4;}
if(ar[i]==5){c=z5(ar[i]);}
if(ar[i]==6){c=6;}
if(ar[i]==7){c=zHPOS(ar[i]);}
if(ar[i]==8){c=zHPOS(ar[i]);}
if(ar[i]==9){c=zHPOS(ar[i]);}
if(ar[i]==10){c=10;}
if(ar[i]==11){c=11;}
if(ar[i]==12){c=12;}
if(ar[i]==13){c=13;}
if(ar[i]==14){c=14;}
if(ar[i]==15){c=15;}
if(ar[i]==16){c=16;}
if(ar[i]==17){c=17;}
if(ar[i]==18){c=18;}
if(ar[i]==19){c=19;}
if(ar[i]==20){c=z20(ar[i]);}
if(ar[i]==21){c=z21(ar[i]);}
if(ar[i]==22){c=22;}
if(ar[i]==23){c=23;}
if(ar[i]==24){c=24;}
if(ar[i]==25){c=25;}
if(ar[i]==26){c=26;}
if(ar[i]==27){c=27;}
if(ar[i]==28){c=28;}
if(ar[i]==29){c=29;}
if(ar[i]==30){c=30;}
if(ar[i]==31){c=31;}
if(ar[i]==34){c=z34(ar[i]);}
if(ar[i]==35){c=zVPOS(ar[i]);}
if(ar[i]==36){c=zVPOS(ar[i]);}
if(ar[i]==37){c=37;}
if(ar[i]==38){c=38;}
if(ar[i]==39){c=39;}
if(ar[i]==40){c=40;}
if(ar[i]==41){c=41;}
if(ar[i]==42){c=42;}
if(ar[i]==43){c=43;}
if(ar[i]==44){c=44;}
if(ar[i]==45){c=45;}
if(ar[i]==46){c=46;}
if(ar[i]==47){c=47;}
if(ar[i]==48){c=z48(ar[i]);}
if(ar[i]==49){c=z49(ar[i]);}
}else{
if(c < 0){
if(ar[i]==1){
c=1;
}else{
otext=ar[i];
c=0;
}
}else{
if(c==1){c=z1(ar[i]);}
if(c==2){c=z2(ar[i]);}
if(c==4){c=z4(ar[i]);}
if(c==6){c=z6(ar[i]);}
if(c==10){c=z10(ar[i]);}
if(c==11){c=z11(ar[i]);}
if(c==12){c=z12(ar[i]);}
if(c==13){c=z13(ar[i]);}
if(c==14){c=z14(ar[i]);}
if(c==15){c=z15(ar[i]);}
if(c==16){c=z16(ar[i]);}
if(c==17){c=z17(ar[i]);}
if(c==18){c=z18(ar[i]);}
if(c==19){c=z19(ar[i]);}
if(c==22){c=z22(ar[i]);}
if(c==23){c=z23(ar[i]);}
if(c==24){c=z24(ar[i]);}
if(c==25){c=z25(ar[i]);}
if(c==26){c=z26(ar[i]);}
if(c==27){c=z27(ar[i]);}
if(c==28){c=z28(ar[i]);}
if(c==29){c=z29(ar[i]);}
if(c==32){c=z32(ar[i]);}// must be before 30
if(c==33){c=z33(ar[i]);}// must be before 31
if(c==30){c=z30(ar[i]);}
if(c==31){c=z31(ar[i]);}
if(c==37){c=z37(ar[i]);}
if(c==38){c=z38(ar[i]);}
if(c==39){c=z39(ar[i]);}
if(c==40){c=z40(ar[i]);}
if(c==41){c=z41(ar[i]);}
if(c==42){c=z42(ar[i]);}
if(c==43){c=z43(ar[i]);}
if(c==44){c=z44(ar[i]);}
if(c==45){c=z45(ar[i]);}
if(c==46){c=z46(ar[i]);}
if(c==47){c=z47(ar[i]);}
}
}
}
if(odelay==0){
return overlib333();
}else{
odelayid=setTimeout("overlib333()", odelay);
if(osticky){
return false;
}else{
return true;
}
}
}
function nd(){
if(oremovecounter >=1){oshowingsticky=0};
if((ns4)||(ie4)||(ns6)){
if(oshowingsticky==0){
oallowmove=0;
if(over !=null)hideObject(over);
}else{
oremovecounter++;
}
}
return true;
}
function overlib333(){
var layerhtml;
if(obackground !="" || ofullhtml){
layerhtml=ol_content_background(otext, obackground, ofullhtml);
}else{
if(ofgbackground !=""){
ofgbackground="BACKGROUND=\""+ofgbackground+"\"";
}
if(obgbackground !=""){
obgbackground="BACKGROUND=\""+obgbackground+"\"";
}
if(ofgcolor !=""){
ofgcolor="BGCOLOR=\""+ofgcolor+"\"";
}
if(obgcolor !=""){
obgcolor="BGCOLOR=\""+obgcolor+"\"";
}
if(oheight > 0){
oheight="HEIGHT=" + oheight;
}else{
oheight="";
}
if(ocap==""){
layerhtml=ol_content_simple(otext);
}else{
if(osticky){
layerhtml=ol_content_caption(otext, ocap, oclose);
}else{
layerhtml=ol_content_caption(otext, ocap, "");
}
}
}
if(osticky){
oshowingsticky=1;
oremovecounter=0;
}
layerWrite(layerhtml);
if(oautostatus > 0){
ostatus=otext;
if(oautostatus > 1){
ostatus=ocap;
}
}
oallowmove=0;
if(otimeout > 0){
if(otimerid > 0)clearTimeout(otimerid);
otimerid=setTimeout("cClick()", otimeout);
}
disp(ostatus);
if(osticky){
oallowmove=0;
return false;
}else{
return true;
}
}
function ol_content_simple(text){
txt="<TABLE WIDTH="+owidth+" BORDER=0 CELLPADDING="+oborder+" CELLSPACING=0 "+obgcolor+" "+oheight+"><TR><TD><TABLE WIDTH=100% BORDER=0 CELLPADDING=2 CELLSPACING=0 "+ofgcolor+" "+ofgbackground+" "+oheight+"><TR><TD VALIGN=TOP><FONT FACE=\""+otextfont+"\" COLOR=\""+otextcolor+"\" SIZE=\""+otextsize+"\">"+text+"</FONT></TD></TR></TABLE></TD></TR></TABLE>";
set_background("");
return txt;
}
function ol_content_caption(text, title, close){
closing="";
if(close !=""){
closing="<TD ALIGN=RIGHT><A HREF=\"/\" onMouseOver=\"cClick();\"><FONT COLOR=\""+oclosecolor+"\" FACE=\""+oclosefont+"\" SIZE=\""+oclosesize+"\">"+close+"</FONT></A></TD>";
}
if(ocapicon !=""){
ocapicon="<IMG SRC=\""+ocapicon+"\"> ";
}
txt="<TABLE WIDTH="+owidth+" BORDER=0 CELLPADDING="+oborder+" CELLSPACING=0 "+obgcolor+" "+obgbackground+" "+oheight+"><TR><TD><TABLE WIDTH=100% BORDER=0 CELLPADDING=0 CELLSPACING=0><TR><TD><B><FONT COLOR=\""+ocapcolor+"\" FACE=\""+ocaptionfont+"\" SIZE=\""+ocaptionsize+"\">"+ocapicon+title+"</FONT></B></TD>"+closing+"</TR></TABLE><TABLE WIDTH=100% BORDER=0 CELLPADDING=2 CELLSPACING=0 "+ofgcolor+" "+ofgbackground+" "+oheight+"><TR><TD VALIGN=TOP><FONT COLOR=\""+otextcolor+"\" FACE=\""+otextfont+"\" SIZE=\""+otextsize+"\">"+text+"</FONT></TD></TR></TABLE></TD></TR></TABLE>";
set_background("");
return txt;
}
function ol_content_background(text, picture, hasfullhtml){
if(hasfullhtml){
txt=text;
}else{
txt="<TABLE WIDTH="+owidth+" BORDER=0 CELLPADDING=0 CELLSPACING=0 HEIGHT="+oheight+"><TR><TD COLSPAN=3 HEIGHT="+opadyt+"></TD></TR><TR><TD WIDTH="+opadxl+"></TD><TD VALIGN=TOP WIDTH="+(owidth-opadxl-opadxr)+"><FONT FACE=\""+otextfont+"\" COLOR=\""+otextcolor+"\" SIZE=\""+otextsize+"\">"+text+"</FONT></TD><TD WIDTH="+opadxr+"></TD></TR><TR><TD COLSPAN=3 HEIGHT="+opadyb+"></TD></TR></TABLE>";
}
set_background(picture);
return txt;
}
function set_background(pic){
if(pic==""){
if(ie4)over.backgroundImage="none";
if(ns6)over.style.backgroundImage="none";
}else{
if(ns4){
over.background.src=pic;
}else if(ie4){
over.backgroundImage="url("+pic+")";
}else if(ns6){
over.style.backgroundImage="url("+pic+")";
}
}
}
function disp(statustext){
if((ns4)||(ie4)||(ns6)){
if(oallowmove==0){
placeLayer();
showObject(over);
oallowmove=1;
}
}
if(statustext !=""){
self.status=statustext;
}
}
function placeLayer(){
var placeX, placeY;
if(ofixx > -1){
placeX=ofixx;
}else{
winoffset=(ie4)? oframe.document.body.scrollLeft : oframe.pageXOffset;
if(ie4)iwidth=oframe.document.body.clientWidth;
if(ns4)iwidth=oframe.innerWidth;// was screwed in mozilla, fixed now?
if(ns6)iwidth=oframe.outerWidth;
if(ohauto==1){
if((ox - winoffset)>((eval(iwidth))/ 2)){
ohpos=7;
}else{
ohpos=8;
}
}
if(ohpos==9){// Center
placeX=ox+ooffsetx-(owidth/2);
}
if(ohpos==8){// Right
placeX=ox+ooffsetx;
if((eval(placeX)+ eval(owidth))>(winoffset + iwidth)){
placeX=iwidth + winoffset - owidth;
if(placeX < 0)placeX=0;
}
}
if(ohpos==7){// Left
placeX=ox-ooffsetx-owidth;
if(placeX < winoffset)placeX=winoffset;
}
if(osnapx > 1){
var snapping=placeX % osnapx;
if(ohpos==7){
placeX=placeX -(osnapx + snapping);
}else{
placeX=placeX +(osnapx - snapping);
}
if(placeX < 0)placeX=0;
}
}
if(ofixy > -1){
placeY=ofixy;
}else{
if(ovauto==1){
if(ie4)iheight=oframe.document.body.clientHeight;
if(ns4)iheight=oframe.innerHeight;
if(ns6)iheight=oframe.outerHeight;
iheight=(eval(iheight))/ 2;
if(oy > iheight){
ovpos=35;
}else{
ovpos=36;
}
}
if(oaboveheight > 0 && ovpos==35){
placeY=oy -(oaboveheight + ooffsety);
}else{
placeY=oy + ooffsety;
}
if(osnapy > 1){
var snapping=placeY % osnapy;
if(oaboveheight > 0 && ovpos==35){
placeY=placeY -(osnapy + snapping);
}else{
placeY=placeY +(osnapy - snapping);
}
if(placeY < 0)placeY=0;
}
}
repositionTo(over, placeX, placeY);
}
function mouseMove(e){
if((ns4)||(ns6)){ox=e.pageX;oy=e.pageY;}
if(ie4){ox=event.x;oy=event.y;}
if(ie5){ox=event.x+oframe.document.body.scrollLeft;oy=event.y+oframe.document.body.scrollTop;}
if(oallowmove==1){
placeLayer();
}
}
function cClick(){
hideObject(over);
oshowingsticky=0;
}
function compatibleframe(frameid){
if(ns4){
if(typeof frameid.document.overDiv=='undefined')return false;
}else if(ie4){
if(typeof frameid.document.all["overDiv"]=='undefined')return false;
}else if(ns6){
if(frameid.document.getElementById('overDiv')==null)return false;
}
return true;
}
function layerWrite(txt){
txt +="\n";
if(ns4){
var lyr=oframe.document.overDiv.document
lyr.write(txt)
lyr.close()
}else if(ie4){
oframe.document.all["overDiv"].innerHTML=txt
}else if(ns6){
range=oframe.document.createRange();
range.setStartBefore(over);
domfrag=range.createContextualFragment(txt);
while(over.hasChildNodes()){
over.removeChild(over.lastChild);
}
over.appendChild(domfrag);
}
}
function showObject(obj){
if(ns4)obj.visibility="show";
else if(ie4)obj.visibility="visible";
else if(ns6)obj.style.visibility="visible";
}
function hideObject(obj){
if(ns4)obj.visibility="hide";
else if(ie4)obj.visibility="hidden";
else if(ns6)obj.style.visibility="hidden";
if(otimerid > 0)clearTimeout(otimerid);
if(odelayid > 0)clearTimeout(odelayid);
otimerid=0;
odelayid=0;
self.status="";
}
function repositionTo(obj,xL,yL){
if((ns4)||(ie4)){
obj.left=xL;
obj.top=yL;
}else if(ns6){
obj.style.left=xL + "px";
obj.style.top=yL+ "px";
}
}
function z1(id){
otext=ol_texts[id];
return 0;
}
function z2(id){
ocap=ol_caps[id];
return 0;
}
function z3(unused){
osticky=1;
return 0;
}
function z4(file){
obackground=file;
return 0;
}
function z5(unused){
oclose="";
return 0;
}
function z6(text){
ocap=text;
return 0;
}
function zHPOS(pos){
ohpos=pos;
return 0;
}
function z10(offset){
ooffsetx=offset;
return 0;
}
function z11(offset){
ooffsety=offset;
return 0;
}
function z12(clr){
ofgcolor=clr;
return 0;
}
function z13(clr){
obgcolor=clr;
return 0;
}
function z14(clr){
otextcolor=clr;
return 0;
}
function z15(clr){
ocapcolor=clr;
return 0;
}
function z16(clr){
oclosecolor=clr;
return 0;
}
function z17(pixels){
owidth=pixels;
return 0;
}
function z18(pixels){
oborder=pixels;
return 0;
}
function z19(text){
ostatus=text;
return 0;
}
function z20(val){
oautostatus=1;
return 0;
}
function z21(val){
oautostatus=2;
return 0;
}
function z22(pixels){
oheight=pixels;
oaboveheight=pixels;
return 0;
}
function z23(text){
oclose=text;
return 0;
}
function z24(pixels){
osnapx=pixels;
return 0;
}
function z25(pixels){
osnapy=pixels;
return 0;
}
function z26(pos){
ofixx=pos;
return 0;
}
function z27(pos){
ofixy=pos;
return 0;
}
function z28(picture){
ofgbackground=picture;
return 0;
}
function z29(picture){
obgbackground=picture;
return 0;
}
function z30(pixels){
opadxl=pixels;
return 32;
}
function z31(pixels){
opadyt=pixels;
return 33;
}
function z32(pixels){
opadxr=pixels;
return 0;
}
function z33(pixels){
opadyb=pixels;
return 0;
}
function z34(unused){
ofullhtml=1;
return 0;
}
function zVPOS(pos){
ovpos=pos;
return 0;
}
function z37(icon){
ocapicon=icon;
return 0;
}
function z38(fontname){
otextfont=fontname;
return 0;
}
function z39(fontname){
ocaptionfont=fontname;
return 0;
}
function z40(fontname){
oclosefont=fontname;
return 0;
}
function z41(fontsize){
otextsize=fontsize;
return 0;
}
function z42(fontsize){
ocaptionsize=fontsize;
return 0;
}
function z43(fontsize){
oclosesize=fontsize;
return 0;
}
function z44(frm){
oframe=compatibleframe(frm)? frm : ol_frame;
if((ns4)||(ie4 ||(ns6))){
if(ns4)over=oframe.document.overDiv;
if(ie4)over=oframe.overDiv.style;
if(ns6)over=oframe.document.getElementById("overDiv");
}
return 0;
}
function z45(maxtime){
otimeout=maxtime;
return 0;
}
function z46(callme){
otext=callme()
return 0;
}
function z47(waittime){
odelay=waittime;
return 0;
}
function z48(onoff){
if(ohauto==0){
ohauto=1;
}else{
ohauto=0;
}
return 0;
}
function z49(onoff){
if(ovauto==0){
ovauto=1;
}else{
ovauto=0;
}
return 0;
}

// -->

</SCRIPT>
