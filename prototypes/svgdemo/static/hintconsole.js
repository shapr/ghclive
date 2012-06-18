
if (typeof console == "undefined" || typeof console.log == "undefined") var console = { log: function() {} }; 

report = alert

function getResult(fileurl, expr){
    var res = null;
    $.ajax({
        url: '/hint',
        type: 'get',
        data : { 'expr':expr,'fileurl':fileurl},
        dataType : 'json',
        async: false,
        success: function(data) {
            res = data;
        }
    });
    return res;
}

$(document).ready(function(){
    $('#fileurl').val('http://www.ScannedInAvian.com/~shae/Demo.hs');

    var console1 = $('<div class="console">');
    $('body').append(console1);
    var controller = console1.console({
        welcomeMessage:'Enter Haskell expressions to evaluate.',
        promptLabel: "hint> ",
        commandValidate:function(line){
            if (line == "") return false;
            else return true;
        },

        commandHandle:function(line,report){
            var fileurl = $('#fileurl').val();
            // don't let this name collide with the jquery-console name!
            console.log('inside commandHandle. input is ' + line + ' fileurl is ' + fileurl);
            res = getResult(fileurl,line);
            return $(res.result);
        },

        autofocus:true,
        animateScroll:true,
        promptHistory:true,
        // welcomeMessage:'Loaded http://www.ScannedInAvian.com/~shae/Demo.hs',
    });
    controller.promptText('map (+1) [1,2,5]');
});