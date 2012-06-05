
// what's the problem?

if (typeof console == "undefined" || typeof console.log == "undefined") var console = { log: function() {} }; 

function evalHs(code, expr, success) {
    var data = { 'method': 'eval', 'contents': code, 'expr': expr };
    var call = {
        'url' : 'http://localhost:3000/clock',
        'data' : data,
        'dataType' : 'jsonp',
        'success' : success
    };
    $.ajax(call);
}

function output(s) {
    if (s) {
        $('#output').val($('#output').val() + s + '\n');
    }
    var outputEl = document.getElementById('output');
    outputEl.scrollTop = outputEl.scrollHeight - outputEl.clientHeight;
}

$(function() {
    $('#expr').focus();
    output();
    $('form').submit(function() {
        var code = $('#code').val();
        var expr = $('#expr').val();
        $('#expr').val('');
        output('> ' + expr);
        evalHs(code, expr, function(result) {
            if (console) {
                console.log(result);
            }
            output(result.result);
            if (result.type) {
                output(':: ' + result.type);
            }
            if (result.result && result.result.length == 1024) {
                output('(The result is too long, so I truncated it for you.)');
            }
            output(result.error);
            output(result.exception);
        });
        return false;
    });
});
