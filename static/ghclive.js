function wsLocation() {
    return window.location.href.replace(/^([a-z]+)\:/i, "ws:");
}

var mainLayout;

var ignoreChange = false;

function handleChange(editor, change) {
    if(ignoreChange) { return; }
    var msgs = doc.replace(change.from, change.to, change.text);
    if(msgs.length > 0) {
        sock.send(JSON.stringify({ actions: msgs }));
    }
}

function refreshEditor() {
    ignoreChange = true;
    // fixme: try to preserve cursor position and selection better if other users insert lines etc
    var sel = cm.somethingSelected();
    var selStart;
    var selEnd;
    var cursor = cm.getCursor();
    if(sel) {
        selStart = cm.getCursor(start);
        selEnd   = cm.getCursor(end);
    }
    cm.setValue(doc.currentVersion());
    cm.setCursor(cursor);
    if(sel) {
        cm.setSelection(selStart,selEnd);
    }
    ignoreChange = false;
}

doc = new Document();

cm = CodeMirror.fromTextArea($('#editor')[0],
                             { mode:         'text/x-haskell'
                               , lineWrapping: true
                               , lineNumbers:  true
                               , fixedGutter:  true
                               , onChange:     handleChange
                             }
                            );
sock = new WebSocket(wsLocation());
sock.onmessage = function(evt) {
    evtJson(evt,function(msg) {
        if(msg.time) {
            updateTimestamp(msg.time);
        }
        var refresh = false;
        var r;
        if(msg.actions) {
            for(var i=0;i<msg.actions.length;i++) {
                var a = msg.actions[i];
                if(a.action === "doc") {
                    doc.setDocument(a.doc);
                    refresh = true;
                } else if(a.action === "clientid") {
                    setClientId(a.clientId);
                } else if(a.action === "insert") {
                    r = doc.applyOp(a);
                    if(r) { refresh = true; }
                } else if(a.action === "remove") {
                    r = doc.applyOp(a);
                    if(r) { refresh = true; }
                }
            }
        }
        if(msg.refreshoutput) {
            // refreshOutput();
            // alert('need to refresh output');
            outputit();
        }
        if(refresh) {
            refreshEditor();
        }
    });
}

function makeResultSlot(expr) {
    var slot = $('<div><span class="prompt">hint&gt;</span> <span class="expr">empty expr</span><div class="result">...</div></div>');
    slot.find('.expr').text(expr);
    return slot;
}

function fillInResultSlot(slot, res) {
    var node = slot.find('.result');
    if (res.error) {
        formatErrors(node, res.error);
    } else {
//        node.text('');
//        node.append(res.result.result);
        node.empty();
        for(var i=0; i<res.result.length; i++) {
            node.append(convertRes(res.result[i]));
        }
    }
}

function convertRes (res) {
    switch(res.t) {
        case "svg":
           return $(res.r); // fixme add clickable zoom function
        case "html":
           return $(res.r);
        case "text":
            var r = $("<span></span>");
            r.text(res.r);
            return r;
    }
}

function formatResult (res) {
    var slot = makeResultSlot(res.expr);
    fillInResultSlot(slot, res);
    return slot;
}

function formatErrors(node, message) {
    node.empty();
    var lines = message.split("\n");
    for (var i = 0; i < lines.length; ++i) {
        // console.log('line ' + i + ': ' + lines[i]);
        node.append(spacify(lines[i]));
        if (lines[i+1])
            node.append($('<br>'));
    }
}

// Change leading spaces into nbsp entities.
function spacify(line) {
    var i;
    for (i = 0; i < line.length && line[i] === " "; ++i)
        ;
    var result = "";
    for (; 0 < i; --i)
        result += "&nbsp;";
    return result + line.substring(i);
}

function scrollToBottom(elem) {
    // var elem = document.getElementById(elem);
    // elem.scrollTop = elem.scrollHeight;
    $(elem).scrollTo('max')
}

function load(success) {
    $.get('/loader', function(res) {
        if ("" + res === "Main,Helper") {
            $("#editormessages").text("");
//            $("#editor-messages").hide()
        } else if (typeof res === "string") {
            formatErrors($("#editormessages"), res);
//            $("#editor-messages").show()
        } else { // just paranoia
            $("#editormessages").text("" + res);
//            $("#editor-messages").show()
        }
        mainLayout.sizeContent("center");
        cm.refresh();
        if (success) success();
    });
}

function outputit(){
    $.ajax({
        type: "GET",
        url: "/results",
        success: function(results) {
            // clear the output area
            $("#output").empty();
            // map formatResult over the results
            for (var i = 0; i < results.length; i++) {
                $("#output").append(formatResult(results[i]));
            }
            scrollToBottom('#output');
            scrollToBottom('ui-layout-center');
        }
    }); // end ajax call
    return false;
}


$(function () {
    // $("#tabs").tabs();
    $( "input:submit, button").button();
    $(document).ready(function () {
      mainLayout = $('body').layout({
          name: "main-layout"
        , applyDefaultStyles: true
        , west: { initHidden: true }
        , east: { initHidden: true }
        , north: { size: 300 }
        , north__onresize: function() { cm.refresh(); }
      });
    });

    function mentionit() {
      var slot = $('<div><span class="prompt">hint&gt;</span> <span class="expr">:load</span><div class="result"></div></div>');
      $("#output").append(slot)
      scrollToBottom('#output');
      scrollToBottom('ui-layout-center');
    }

    $("#load").click(function() {
        load();
        $("#expr").select();
        $("#expr").focus();
        mentionit();
        return false;
    });

    function evalit() {
        var expr = $("#expr").val();
        var slot = makeResultSlot(expr);
        $("#output").append(slot);
        load(function() {
            $.ajax({
                type: "GET",
                url: "/eval",
                data: {expr: expr},
                success: function(res) {
                    // XXX This is probably pointless now that each evaluation
                    // causes the server to send out a refreshoutput message.
                    // But maybe it still gets you a result display with lower
                    // latency (one less round trip, and not rewriting the whole
                    // output history) so I'm not nuking this yet.
                    fillInResultSlot(slot, res);
                    scrollToBottom('#output');
                    scrollToBottom('ui-layout-center');
                }
            });
        });
        $("#expr").select();
        return false;
    }
    $("#evalform").submit(function(e) {
      e.preventDefault();
      evalit();
    });
    $("#evalit").click(evalit);
    $("#outputit").click(outputit);
});
