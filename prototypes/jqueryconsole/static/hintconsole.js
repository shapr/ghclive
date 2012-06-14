function encodeHex(str){
    var result = "";
    for (var i=0; i<str.length; i++){
        result += "%" + pad(toHex(str.charCodeAt(i)&0xff),2,'0');
    }
    return result;
}

function pad(str, len, pad){
    var result = str;
    for (var i=str.length; i<len; i++){
        result = pad + result;
    }
    return result;
}


function toHex(n){
    var result = ''
    var start = true;
    for (var i=32; i>0;){
        i-=4;
        var digit = (n>>i) & 0xf;
        if (!start || digit != 0){
            start = false;
            result += digitArray[digit];
        }
    }
    return (result==''?'0':result);
}

var digitArray = new Array('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f');


$(document).ready(function(){
    $('#fileurl').val('http://www.ScannedInAvian.com/~shae/Demo.hs');
    $('#expr').val('map (+1) [1,3,5] -- expression, hint enter at the end of this line');

    var showTypes = false;
    function jsonp(url,func) {
        var script = $('<script type="text/javascript" src="'+url+'"></script>');
        handleJSON = function(r){
            script.remove();
            func(r);
        };
        script.attr('src',url);
        $('body').append(script);
    }



    var console = $('<div class="console">');
    $('body').append(console);
    var controller = console.console({
        promptLabel: "hint> ",
        commandValidate:function(line){
            if (line == "") return false;
            else return true;
        },

        //     commandHandle:function(line){
        //         try { var ret = eval(line);
        //               if (typeof ret != 'undefined') return ret.toString();
        //               else return true; }
        //         catch (e) { return e.toString(); }
        //     },
        //     autofocus:true,
        //     animateScroll:true,
        //     promptHistory:true,
        //     welcomeMessage:'Loaded http://www.ScannedInAvian.com/~shae/Demo.hs',
        // });

        commandHandle:function(line,report){
            controller.ajaxloader = $('<p class="ajax-loader">Loading...</p>');
            var commandRef = {};
            controller.currentLine = line;
            controller.commandRef = commandRef;
            controller.report = report;
            // if (tellAboutRet) tellAboutRet.fadeOut(function(){
            //     $(this).remove();
            // });
            // if (libTrigger(line,report)) return;
            controller.inner.append(controller.ajaxloader);
            controller.scrollToBottom();
            jsonp("http://tryhaskell.org/haskell.json?method=eval&pad=handleJSON&expr=" + encodeHex(line) + "&random=" + Math.random(),
                  function(resp){
                      if (commandRef.ignore) { return; }
                      controller.finishCommand();
                      var result = resp;
                      // if (pageTrigger > -1 && result.expr) {
                      //     triggerTutorialPage(pageTrigger,result); }
                      if (result.type) {
                          // if (pageTrigger == 24) showTypes = true;
                          handleSuccess(report,result,showTypes);
                      } else if (result.error) {
                          report(
                              [{msg:result.error,
                                className:"jquery-console-message-error jquery-console-message-compile-error"}]
                          );
                          notice('compile-error',
                                 "A compile-time error! "+
                                 "It just means the expression wasn't quite right. " +
                                 "Try again.",
                                 'prompt');
                      } else if (result.exception) {
                          var err = limitsError(result.exception);
                          report(
                              [{msg:err,
                                className:"jquery-console-message-error jquery-console-message-exception"}]
                          );
                          if (err == result.exception) {
                              notice('compile-error',
                                     "A run-time error! The expression was right but the"+
                                     " result didn't make sense. Check your expression and try again.",
                                     'prompt');
                          }
                      } else if (result.internal) {
                          report(
                              [{msg:limitsError(result.internal),
                                className:"jquery-console-message-error jquery-console-message-internal"}]
                          );
                      } else if (result.bind) {
                          report();
                      } else if (result.result) {
                          if (result.expr.match(/^:modules/)) {
                              report(
                                  [{msg:result.result.replace(/[\["\]]/g,'')
                                    .replace(/,/g,', '),
                                    className:"jquery-console-message-type"}]);
                          }
                      }
                  });
        },
    });

    controller.finishCommand = function() {
        controller.ajaxloader.remove();
        $('.jquery-console-prompt :last').each(function(){
            lastLine = controller.currentLine;
            if (!$(this).hasClass('prompt-done')) {
                $(this).addClass('prompt-done');
                $(this).click(function(){
                    controller.promptText(controller.currentLine);
                });
            }
        });
    }

    function handleSuccess(report,result,showType) {
        if (result.type.match(/^Graphics\.Raphael\.Raphael[\r\n ]/)) {
            runRaphael(result.result);
            report();
        } else {
            if (result.result) {
                var type = [];
                if (showType) {
                    type = [{msg:':: ' + result.type,
                             className:"jquery-console-message-type"}];
                }
                report(
                    [{msg:'=> ' + result.result,
                      className:"jquery-console-message-value"}].concat(type)
                );
            } else {
                report(
                    [{msg:':: ' + result.type,
                      className:"jquery-console-message-type"}]
                );
            }
        }
    };

});