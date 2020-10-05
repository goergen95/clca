jQuery(window).on("load resize",function(e) {
  var more = document.getElementById("js-centered-more");

  if (jQuery(more).length > 0) {
    var windowWidth = jQuery(window).width();
    var moreLeftSideToPageLeftSide = jQuery(more).offset().left;
    var moreLeftSideToPageRightSide = windowWidth - moreLeftSideToPageLeftSide;

    if (moreLeftSideToPageRightSide < 330) {
      jQuery("#js-centered-more .submenu .submenu").removeClass("fly-out-right");
      jQuery("#js-centered-more .submenu .submenu").addClass("fly-out-left");
    }

    if (moreLeftSideToPageRightSide > 330) {
      jQuery("#js-centered-more .submenu .submenu").removeClass("fly-out-left");
      jQuery("#js-centered-more .submenu .submenu").addClass("fly-out-right");
    }
  }

  var menuToggle = jQuery("#js-navbar--top_mobile-toggle").unbind();
  jQuery("#js-navbar-top-menu").removeClass("show");

  menuToggle.on("click", function(e) {
    e.preventDefault();
    jQuery("#js-navbar-top-menu").slideToggle(function(){
      if(jQuery("#js-navbar-top-menu").is(":hidden")) {
        jQuery("#js-navbar-top-menu").removeAttr("style");
      }
    });
  });
});
