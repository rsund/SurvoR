var background = [
"#f8f8f8", "#f8f8f8", "#f8f8f8", "#f8f8f8", "#00008b", "#ffff00", "#f8f8f8", "#0000ff",
"#00008b", "#f8f8f8", "#f8f8f8", "#f8f8f8", "#f8f8f8", "#f8f8f8", "#f8f8f8", "#f8f8f8",
"#f8f8f8", "#f8f8f8", "#f8f8f8", "#f8f8f8", "#f8f8f8", "#f8f8f8", "#f8f8f8", "#f8f8f8",
"#f8f8f8", "#f8f8f8", "#f8f8f8", "#f8f8f8", "#f8f8f8", "#f8f8f8", "#f8f8f8", "#f8f8f8",
"#ffffff", "#00008b", "#00008b", "#00008b", "#00008b", "#00008b", "#00008b", "#00008b",
"#00008b", "#00008b", "#00008b", "#00008b", "#00008b", "#00008b", "#00008b", "#00008b",
"#006400", "#ffffff", "#ffffff", "#ffffff", "#00008b", "#ffff00", "#ffffff", "#0000ff",
"#00008b", "#ffffff", "#006400", "#006400", "#006400", "#006400", "#006400", "#006400",
"#008b8b", "#008b8b", "#008b8b", "#008b8b", "#008b8b", "#008b8b", "#008b8b", "#008b8b",
"#008b8b", "#008b8b", "#008b8b", "#008b8b", "#008b8b", "#008b8b", "#008b8b", "#008b8b",
"#8b0000", "#8b0000", "#8b0000", "#8b0000", "#8b0000", "#8b0000", "#8b0000", "#8b0000",
"#8b0000", "#8b0000", "#8b0000", "#8b0000", "#8b0000", "#8b0000", "#8b0000", "#8b0000",
"#8b008b", "#8b008b", "#8b008b", "#8b008b", "#8b008b", "#8b008b", "#8b008b", "#8b008b",
"#8b008b", "#8b008b", "#8b008b", "#8b008b", "#8b008b", "#8b008b", "#8b008b", "#8b008b",
"#8b8b00", "#8b8b00", "#8b8b00", "#8b8b00", "#8b8b00", "#8b8b00", "#8b8b00", "#8b8b00",
"#8b8b00", "#8b8b00", "#8b8b00", "#8b8b00", "#8b8b00", "#8b8b00", "#8b8b00", "#8b8b00",
"#a9a9a9", "#a9a9a9", "#a9a9a9", "#a9a9a9", "#a9a9a9", "#a9a9a9", "#a9a9a9", "#a9a9a9",
"#a9a9a9", "#a9a9a9", "#a9a9a9", "#a9a9a9", "#a9a9a9", "#a9a9a9", "#a9a9a9", "#a9a9a9",
"#bebebe", "#bebebe", "#bebebe", "#bebebe", "#bebebe", "#bebebe", "#bebebe", "#bebebe",
"#bebebe", "#bebebe", "#bebebe", "#bebebe", "#bebebe", "#bebebe", "#bebebe", "#bebebe",
"#0000ff", "#0000ff", "#0000ff", "#0000ff", "#0000ff", "#0000ff", "#0000ff", "#0000ff",
"#0000ff", "#0000ff", "#0000ff", "#0000ff", "#0000ff", "#0000ff", "#0000ff", "#0000ff",
"#00ff00", "#00ff00", "#00ff00", "#00ff00", "#00ff00", "#00ff00", "#00ff00", "#00ff00",
"#00ff00", "#00ff00", "#00ff00", "#00ff00", "#00ff00", "#00ff00", "#00ff00", "#00ff00",
"#00ffff", "#00ffff", "#00ffff", "#00ffff", "#00ffff", "#00ffff", "#00ffff", "#00ffff",
"#00ffff", "#00ffff", "#00ffff", "#00ffff", "#00ffff", "#00ffff", "#00ffff", "#00ffff",
"#ff0000", "#ff0000", "#ff0000", "#ff0000", "#ff0000", "#ff0000", "#ff0000", "#ff0000",
"#ff0000", "#ff0000", "#ff0000", "#ff0000", "#ff0000", "#ff0000", "#ff0000", "#ff0000",
"#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000",
"#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000",
"#ffff00", "#ffff00", "#ffff00", "#ffff00", "#ffff00", "#ffff00", "#ffff00", "#ffff00",
"#ffffff", "#ffff00", "#ffff00", "#ffff00", "#ffff00", "#ffff00", "#ffff00", "#ffff00"
];

var foreground = [
"#000000", "#ff0000", "#a9a9a9", "#0000ff", "#bebebe", "#000000", "#228b22", "#ffffff",
"#ffff00", "#a9a9a9", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000",
"#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000",
"#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000",
"#000000", "#00008b", "#006400", "#008b8b", "#8b0000", "#8b008b", "#8b8b00", "#a9a9a9",
"#bebebe", "#0000ff", "#00ff00", "#00ffff", "#ff0000", "#ff00ff", "#ffff00", "#ffffff",
"#000000", "#ff0000", "#a9a9a9", "#0000ff", "#bebebe", "#000000", "#228b22", "#ffffff",
"#ffff00", "#a9a9a9", "#00ff00", "#00ffff", "#ff0000", "#ff00ff", "#ffff00", "#ffffff",
"#000000", "#00008b", "#006400", "#008b8b", "#8b0000", "#8b008b", "#8b8b00", "#a9a9a9",
"#bebebe", "#0000ff", "#00ff00", "#00ffff", "#ff0000", "#ff00ff", "#ffff00", "#ffffff",
"#000000", "#00008b", "#006400", "#008b8b", "#8b0000", "#8b008b", "#8b8b00", "#a9a9a9",
"#bebebe", "#0000ff", "#00ff00", "#00ffff", "#ff0000", "#ff00ff", "#ffff00", "#ffffff",
"#000000", "#00008b", "#006400", "#008b8b", "#8b0000", "#8b008b", "#8b8b00", "#a9a9a9",
"#bebebe", "#0000ff", "#00ff00", "#00ffff", "#ff0000", "#ff00ff", "#ffff00", "#ffffff",
"#000000", "#00008b", "#006400", "#008b8b", "#8b0000", "#8b008b", "#8b8b00", "#a9a9a9",
"#bebebe", "#0000ff", "#00ff00", "#00ffff", "#ff0000", "#ff00ff", "#ffff00", "#ffffff",
"#000000", "#00008b", "#006400", "#008b8b", "#8b0000", "#8b008b", "#8b8b00", "#a9a9a9",
"#bebebe", "#0000ff", "#00ff00", "#00ffff", "#ff0000", "#ff00ff", "#ffff00", "#ffffff",
"#000000", "#00008b", "#006400", "#008b8b", "#8b0000", "#8b008b", "#8b8b00", "#a9a9a9",
"#bebebe", "#0000ff", "#00ff00", "#00ffff", "#ff0000", "#ff00ff", "#ffff00", "#ffffff",
"#000000", "#00008b", "#006400", "#008b8b", "#8b0000", "#8b008b", "#8b8b00", "#a9a9a9",
"#bebebe", "#0000ff", "#00ff00", "#00ffff", "#ff0000", "#ff00ff", "#ffff00", "#ffffff",
"#000000", "#00008b", "#006400", "#008b8b", "#8b0000", "#8b008b", "#8b8b00", "#a9a9a9",
"#bebebe", "#0000ff", "#00ff00", "#00ffff", "#ff0000", "#ff00ff", "#ffff00", "#ffffff",
"#000000", "#00008b", "#006400", "#008b8b", "#8b0000", "#8b008b", "#8b8b00", "#a9a9a9",
"#ffffff", "#0000ff", "#00ff00", "#00ffff", "#ff0000", "#ff00ff", "#ffff00", "#ffffff",
"#000000", "#00008b", "#006400", "#008b8b", "#8b0000", "#8b008b", "#8b8b00", "#a9a9a9",
"#bebebe", "#0000ff", "#00ff00", "#00ffff", "#ff0000", "#ff00ff", "#ffff00", "#ffffff",
"#000000", "#00008b", "#006400", "#008b8b", "#8b0000", "#8b008b", "#8b8b00", "#a9a9a9",
"#bebebe", "#0000ff", "#00ff00", "#00ffff", "#ff0000", "#ff00ff", "#ffff00", "#ffffff",
"#000000", "#00008b", "#006400", "#008b8b", "#8b0000", "#8b008b", "#8b8b00", "#a9a9a9",
"#ffffff", "#0000ff", "#00ff00", "#00ffff", "#ff0000", "#ff00ff", "#ffff00", "#ffffff"
];


var cwidth=10;
var cheight=18.75;

writeText = function(stri,ctx) {
  var sarr=stri.split("\t");
  var len=sarr.pop();
  var i;
  ctx.textBaseline="top";
  for (i=0; i<=len; i++)
    {
    var sha=sarr.shift();
    var txt=sarr.shift();
    var txtx=sarr.shift()*cwidth;
    var txty=sarr.shift()*cheight;
    ctx.fillStyle=background[sha];
    ctx.fillRect(txtx,txty,ctx.measureText(txt).width,cheight);
    ctx.fillStyle=foreground[sha];
    ctx.fillText(txt,txtx,txty);
    }
}

var block = 0;

function loadDocChar(url, para, cfunc) {
  if (block == 0) {
  block=1;
  var xhttp;
  xhttp=new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
      if (xhttp.readyState == 4 && xhttp.status == 200) {
          block=0;
          cfunc(xhttp);
      }
      else {
document.getElementById("status").innerHTML = xhttp.readyState;
document.getElementById("status2").innerHTML = xhttp.status;
}
    };
  xhttp.open("POST", url, true);
  xhttp.setRequestHeader("Content-type", "text/plain");
  xhttp.send(para);
 }
}


function myFunction(xhttp) {
  writeText(xhttp.responseText,ctx);
}


var PIXEL_RATIO = (function () {
    var ctx = document.createElement("canvas").getContext("2d"),
        dpr = window.devicePixelRatio || 1,
        bsr = ctx.webkitBackingStorePixelRatio ||
              ctx.mozBackingStorePixelRatio ||
              ctx.msBackingStorePixelRatio ||
              ctx.oBackingStorePixelRatio ||
              ctx.backingStorePixelRatio || 1;

    return dpr / bsr;
})();


createHiPPICanvas = function(w, h, ratio) {
    if (!ratio) { ratio = PIXEL_RATIO; }
    var can = document.createElement("canvas");
    can.width = w * ratio;
    can.height = h * ratio;
    can.style.width = w + "px";
    can.style.height = h + "px";
    can.getContext("2d").setTransform(ratio, 0, 0, ratio, 0, 0);
    can.position="absolute";
    can.left="0px";
    can.top="0px";    
    return can;
}

var canvas = createHiPPICanvas(1000, 18.75*25);
document.body.appendChild(canvas);
var ctx = canvas.getContext("2d");
ctx.font = "15px/18.75px Lucida Sans Typewriter";
cwidth = ctx.measureText(" ").width;




document.onkeypress = function(e) {
var char=" ";
if (e.which == null)
     char= String.fromCharCode(e.keyCode);    // old IE
  else if (e.which != 0 && e.charCode != 0)
     char= String.fromCharCode(e.which);        // All others
  else
     char=null;
   document.getElementById("demo").innerHTML = char;
   loadDocChar("survo_ajax.txt","disp=0&char=".concat(char),myFunction);

}
