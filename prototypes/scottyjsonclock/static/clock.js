// so simple..
$(document).ready(
function updateClock(){
    $.getJSON('clock',function(data){
        $('#clock').html('<p>' + data +  '</p>');
        setTimeout(updateClock, 1000);
    });
});

