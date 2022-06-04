$(function () {

  // Help popover
  var $pop = $('body');
  $pop.popover({
    trigger: 'hover focus',
    selector: '[data-bs-toggle="popover"]'
  });

});