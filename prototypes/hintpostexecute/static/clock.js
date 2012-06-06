// so simple..
$(document).ready(
function updateClock(){
    $.getJSON('clock',function(data){
        $('#clock').html('<p>' + data +  '</p>');
        setTimeout(updateClock, 1000);
    });
});

// this is what we need to execute
// how to hook it to the submit button?
// how to set the result data in output?
// $.get("/clock",{expr:$('#expr').val(),fileurl:('#fileurl').val());