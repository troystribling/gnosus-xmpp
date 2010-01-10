/**********************************************************************************
data edit controls
**********************************************************************************/
function init_data_edit_row() {
    if (!$('tr.data-edit').length) { 
       return; 
    }
    $('tr.data-edit').hover(
        function() { 
            $(this).addClass('data-edit-select'); 
            $(this).find('.data-edit-controls').each(function(i) {
                $(this).show();
            });
        }, 
        function() { 
            $(this).removeClass('data-edit-select'); 
            $(this).find('.data-edit-controls').each(function(i) {
                $(this).hide();
            });
        }
    ); 
}
