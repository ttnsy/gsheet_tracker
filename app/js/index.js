$(document).ready(function(){
    const navLinks = document.querySelectorAll('.nav-link');
    navLinks.forEach((item) => {
        if (item.href === window.location.href || item.href.replace(/#!\/$/,'') == window.location.href) {
          item.classList.add('active')
        }
    });

    $('.nav-link').click(function(){
      $('.nav-link').removeClass("active");
      $(this).addClass("active");
    });
});