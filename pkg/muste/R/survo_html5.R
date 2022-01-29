websurvo <- function()
{
# library(httpuv)
.muste.command(c("Require","httpuv"),force=TRUE)	    	
if (requireNamespace("RCurl",quietly=TRUE)) {

cat("Starting server on port 8080...\n")
.muste$webedit<-as.integer(1)
app <-
  list(
  call = function(req) {
    urlBody <- ifelse(is.null(req$HTTP_HOST), req$SERVER_NAME, req$HTTP_HOST)
    wsUrl <- paste(sep='','"',"ws://",urlBody,'"')
    list(
      status = 200L,
      headers = list(
        'Content-Type' = 'text/html'
      ),
      body = paste(
        sep = "\r\n",
##########################################################################################        
'        
<!DOCTYPE html>
<html>
<head><meta charset="UTF-8">
<style type="text/css">
body, html { margin: 0px; padding: 0px; border: 0; overflow: hidden; display: block; }
</style>
</head>
<body>
<script>

var background = [
"#f8f8f8", "#f8f8f8", "#f8f8f8", "#f8f8f8", "#00008b", "#ffff00", "#f8f8f8", "#f8f8f8",
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
"#000000", "#ff0000", "#a9a9a9", "#0000ff", "#bebebe", "#000000", "#228b22", "#000000",
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

writeText = function(stri,ctx) {
  var sarr=stri.split("\t");
  var len=sarr.pop();
  if (len.includes("cursor")) {
    offsetY=sarr.shift();
    offsetX=sarr.shift();
    setCursorPos(offsetX,offsetY);
  } else {
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
}

var cwidth=10;
var cheight=18.75;

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

var editor = document.createElement("div");
editor.style.position="relative";
editor.style.display="inline-block"
editor.style.overflow="auto";
// editor.tabIndex = 0;
// editor.addEventListener("focus", editorFocus(), false);
document.body.appendChild(editor);

var canvas = createHiPPICanvas(1000, 18.75*25);
editor.appendChild(canvas);
var ctx = canvas.getContext("2d");
ctx.font = "15px Courier";
cwidth = ctx.measureText(" ").width;
',
########################################################
        sprintf("var ws = new WebSocket(%s);", wsUrl),
########################################################        
'
ws.onmessage = function(msg) {
 connectionStatus(1);
 heartBeatCount = 0;
 if (msg.data==="*SurvoR*") { return; }
 writeText(msg.data,ctx);
 }
 
 ws.onerror = function(error) {
 connectionStatus(0);
 }
 
 ws.onclose = function(event) {
  connectionStatus(0);
 }

ws.onopen = function(e) {
 connectionStatus(1);
 heartBeatInterval = setInterval(heartBeatMsg, heartBeatTime);
}

document.onkeypress = function(e) {
var char=" ";
if (e.which == null)
     char= String.fromCharCode(e.keyCode);    // old IE
  else if (e.which != 0 && e.charCode != 0)
     char= String.fromCharCode(e.which);        // All others
  else
     char=null;
   document.getElementById("demo").innerHTML = char;
   ws.send(char);
} 


var heartBeatCount=0;
var heartBeatInterval=0;
var heartBeatTime=5000;

function heartBeatMsg() {
  heartBeatCount++;
  if (heartBeatCount>2) { connectionStatus(0); }
  else { ws.send("*SurvoR*"); }
}


function connectionStatus(status) {
  if (status) { 
  document.getElementById("status").innerHTML = "Connected";
  editor.style.opacity = 1;
  }
  else { 
  document.getElementById("status").innerHTML = "Not connected";
  editor.style.opacity = 0.2;
  setCursorVisibility(0); 
  }
}

var cursorVisibility = 1;
var cursorInterval;
var blinkTime = 500;
var cursorOpacity = 1;
var cursorHeight = 0.3;
var cursorWidth = 1;
var cursor = document.createElement("div");
cursor.style.position = "absolute";
cursor.style.width = cursorWidth*cwidth + "px";
cursor.style.height = cursorHeight*cheight + "px";
cursor.style.backgroundColor = "#000";
cursor.style.display = "block";
cursor.style.opacity = cursorOpacity;
editor.appendChild(cursor);
setCursorPos(10,10);
setCursorVisibility(1);

function setCursorPos(offsetX,offsetY) {
     document.getElementById("demo").innerHTML = offsetX + " " + offsetY;
    cursor.style.left = offsetX*cwidth + "px";
    cursor.style.top = offsetY*cheight + (1-cursorHeight)*cheight + "px";
    if (cursorVisibility) {
        cursor.style.opacity = cursorOpacity;
        clearInterval(cursorInterval);
        cursorInterval = setInterval(blinkCursor, blinkTime);
        }
}

function setCursorVisibility(visibility) {
  if (visibility) {
    cursor.style.display = "block";
    cursor.style.opacity = cursorOpacity;
    clearInterval(cursorInterval);
    cursorInterval = setInterval(blinkCursor, blinkTime);
  } else {
    cursor.style.display = "none";
  }
  cursorVisibility = visibility;
}

function blinkCursor() {
  if (cursor.style.opacity>0) {
    cursor.style.opacity = 0;
  } else {
    cursor.style.opacity = cursorOpacity;
  }
}
     

</script>

<p id="demo"></p>
<p id="status"></p>
<p id="status2"></p>

</body>
</html>
'
##########################################################################################
      )
    )
  },
    onHeaders = function(req) {
      # Print connection headers
      # cat("onHeaders:",capture.output(str(as.list(req))), sep = "\n")
    },
    onWSOpen = function(ws) {

#as.list(.muste$ws$request)$HTTP_SEC_WEBSOCKET_KEY       
      ws$onMessage(function(binary, message) {
      if (message=="*SurvoR*") ws$send("*SurvoR*")
      else {
       cat("Server received message:", message, "\n")
    .muste$event.time<-as.integer(.muste$event.time+1)
    .muste$event.type<-as.integer(1)  # KEY_EVENT
    .muste$key.char<-message
    .muste$key.keysym<-as.integer(0)
    .muste$key.status<-as.integer(1)
#.muste$redraw <- as.integer(2)      
    invisible(.Call("Muste_Eventloop",.muste$eventloopargs,PACKAGE="muste"))        
   ws$send(.muste$ajaxmsg)
       }
      })
      ws$onClose(function() {
        .muste$ws <- .muste$ws[sapply(.muste$ws, function(x) !is.null(x$handle))] # Remove closed connections from the list
        cat("Connection closed. ",length(.muste$ws),"active connection(s).\n")
      })
   
      .muste$ws <- append(.muste$ws, ws) # Add new connection to the list
        cat("Connection opened. ",length(.muste$ws),"active connection(s).\n")  
#      .muste$ws <- .muste$ws[sapply(.muste$ws, function(x) !is.null(x$handle))] # Remove closed connections from the list
      .muste$redraw <- as.integer(2)
      invisible(.Call("Muste_Eventloop",.muste$eventloopargs,PACKAGE="muste")) 

    }
  )
  
   httpuv::stopAllServers()
   serv <- httpuv::startServer("0.0.0.0", 8080, app)    
 
survobrowser("http://localhost:8080")
}
}

survobrowser <- function(survourl="http://localhost:8080") {
viewer <- getOption("viewer")
    if (!is.null(viewer))
       viewer(survourl)
    else {  
        utils::browseURL(survourl)     
     }
     }

.muste$ws <- list()

survo.sendajax <- function() {
if (length(.muste$ws)>0)
  for (i in 1:length(.muste$ws)) .muste$ws[[i]]$send(.muste$ajaxmsg)
}

