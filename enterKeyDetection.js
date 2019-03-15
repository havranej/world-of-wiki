
// Big thanks to paul for sharing this piece of code
// https://community.rstudio.com/t/textbox-in-shiny/19053/2


$(document).keyup(function(event) {
    if ($("#pagename").is(":focus") && (event.keyCode == 13)) {
        $("#recalculate").click();
    }
});