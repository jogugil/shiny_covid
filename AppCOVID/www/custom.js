location.reload( forceGet )
$(document).ready(
  function(){
    if (window.location.hash) {
 
      var hash_val = '#' + window.location.hash.replace('#','');
alert(hash_val)
      $('a', $('.sidebar')).each(function() {
alert( window.location.hash)
          if(this.getAttribute('href') == hash_val) {
            this.click();
			alert(this.getAttribute('href'))
          };
      });
    }
})