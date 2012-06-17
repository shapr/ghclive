
var GHCi = function (serverURL) {
    this.server = serverURL;
}
// Load module, synchronous
GHCi.prototype.load = function (fileurl) {
    this.fileurl = fileurl;
}
// send for evaluation, synchronous
GHCi.prototype.eval = function (expr, success) {
    console.log('reached eval with ' + expr);
    $.ajax({
        url: this.server + '/hint',
        type: 'get',
        data : { 'expr':expr, 'fileurl': this.fileurl },
        dataType : 'json',
        success: success
    });
}

// this is executed when the page is finished loading
$(document).ready(function(){
    ghci = new GHCi('http://localhost:3000');

    // add an input element to our form
    var inputEl = $('<input class="expr" id="expr" type="text" />');
    $('#ghci').append(inputEl);
    // hook the input element to submit code to the server
    $('#ghci').submit(function() {
        console.log('in the submit function, expr is ' + expr);
        var expr = $('.expr').filter(":last");
        var fileurl = $('#fileurl');
        // send to the server
        ghci.load(fileurl.val());
        ghci.eval(expr.val(), function(result) {
            // display the result
            var newEl = $('<div/>');
            newEl.html(result.result);
            $('#ghci').append(newEl);

            // disable the input element
            expr.attr("disabled", "disabled");
            // add an input element to our form
            var inputEl = $('<input class="expr" id="expr" type="text" />');
            $('#ghci').append(inputEl);
        });
        // don't use the default method for submitting the form!
        return false;
    });
});