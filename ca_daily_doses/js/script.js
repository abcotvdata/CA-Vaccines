/* script.js 
   Author:
   Date:
*/

counties = ["All CA Counties", "Alameda","Alpine","Amador","Butte","Calaveras","Colusa","Contra Costa","Del Norte","El Dorado","Fresno","Glenn","Humboldt","Imperial","Inyo","Kern","Kings","Lake","Lassen","Los Angeles","Madera","Marin","Mariposa","Mendocino","Merced","Modoc","Mono","Monterey","Napa","Nevada","Orange","Placer","Plumas","Riverside","Sacramento","San Benito","San Bernardino","San Diego","San Francisco","San Joaquin","San Luis Obispo","San Mateo","Santa Barbara","Santa Clara","Santa Cruz","Shasta","Sierra","Siskiyou","Solano","Sonoma","Stanislaus","Sutter","Tehama","Trinity","Tulare","Tuolumne","Ventura","Yolo","Yuba"]
	// DROPDOWN ______________________________________

//JQUERY PUTTING THE COUNTIES IN THE DROPDOWN
$(document).ready(function(){
	var countiesleng = counties.length;

	for (var i=0; i<countiesleng; i++) {
		//console.log(counties[i])
		$('.countydrop').append('<p class="'+ counties[i] +'" numb='+i+'>'+ counties[i] +'</p>')

	}
});

	function myFunction() {
	  document.getElementById("myDropdown").classList.toggle("show");
	}

	function filterFunction() {
	  var input, filter, ul, li, a, i;
	  input = document.getElementById("myInput");
	  filter = input.value.toUpperCase();
	  div = document.getElementById("myDropdown");
	  p = div.getElementsByTagName("p");
	  for (i = 0; i < p.length; i++) {
	    txtValue = p[i].textContent || p[i].innerText;
	    if (txtValue.toUpperCase().indexOf(filter) > -1) {
	      p[i].style.display = "";
	    } else {
	      p[i].style.display = "none";
	    }
	  }
	}

$(document).ready(function(){ // begin document.ready block

	
	$('.countydrop p').click(function(){

    		var numbers = []
    		var posnumbers = []

    		$('#myDropdown').removeClass('show')
			var clickedcounty = Number($(this).attr('numb'));
			var clickedcountyname = counties[clickedcounty]
			var countynospace = clickedcountyname.replace(/ /g,"_");
			console.log(clickedcountyname)
			console.log(countynospace)

			$('.iframe').html('<iframe src="plots/vax_with_ave_'+countynospace+'.html"></iframe>')

			$('.graph-title h3').html(clickedcountyname)


	});






















}); //end document.ready block
