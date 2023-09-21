$(document).ready(function() {
    // Function to highlight the active page
    function highlightActiveLink() {
        let currentURL = window.location.href;

        // Iterate over each nav link
        $('.nav-link').each(function() {
            if (this.href === currentURL) {
                $(this).addClass('active');
            } else {
                $(this).removeClass('active');
            }
        });
    }
    
    // Highlight the appropriate nav link when the page first loads
    highlightActiveLink();

    // Update the highlight when a nav link is clicked
    $('.nav-link').on('click', function(event) {
        $('.nav-link').removeClass('active');
        $(this).addClass('active');
    });
});