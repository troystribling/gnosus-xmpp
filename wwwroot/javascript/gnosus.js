function init_data_edit_row() {
    if (!$('tr.data-edit').length) { 
       return; 
    }
    $('tr.data-edit').hover(function() { 
        $(this).addClass('data-edit-select'); 
    }, 
    function() { 
        $(this).removeClass('data-edit-select'); 
    }); 
}

$(document).ready(function() { 
    init_data_edit_row();
}); 
