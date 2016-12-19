/* Taken from http://www.akchauhan.com/create-simple-left-tree-menu-using-jquery/ */

$(document).ready(function() {
	$("#root_tree_menu ul").each(function() {$(this).css("display", "block");});
	$("#root_tree_menu .category").click(function() {
		var childid = "#" + $(this).attr("childid");
		if ($(childid).css("display") == "none") {$(childid).css("display", "block");}
		else {$(childid).css("display", "none");}
		if ($(this).hasClass("cat_close")) {$(this).removeClass("cat_close").addClass("cat_open");}
		else{$(this).removeClass("cat_open").addClass("cat_close");}
	});
});

