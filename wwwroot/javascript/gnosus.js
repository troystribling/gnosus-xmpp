/**********************************************************************************
data edit controls
**********************************************************************************/
function init_data_edit_row() {
    if (!$('tr.data-edit').length) { 
       return; 
    }
    $('tr.data-edit').hover(
        function() { 
            $(this).addClass('data-edit-select').find('.data-edit-controls').show();
        },
        function() { 
            $(this).removeClass('data-edit-select').find('.data-edit-controls').hide();
        }
    ); 
}

/*-------------------------------------------------------------------------------*/
function setPageSize() {
    var doc_height     = $(document).height(),
        page_height    = $('#page-wrapper').height(),
        win_height     = $(window).height(),
        win_width      = $(window).width(),
        flash_offset   = $('#page__flash').height(),
        toolbar_offset = $('#toolbar-wrapper').height(),
        title_margin   = $('#title-wrapper h1').css('marginBottom'),
        title_offset   = $('#title-wrapper').height(),
        nav_offset     = $('#navigation-wrapper').height() + parseInt($('#subtitle-wrapper').css('marginBottom').substr(0,2)) + 2,
        footer_offset  = $('#footer-wrapper').height() + parseInt($('#footer-wrapper').css('marginTop').substr(0,2)) + 1,
        page_width     = 0.75*win_width,
        page_height    = doc_height-nav_offset-footer_offset-flash_offset-toolbar_offset-title_offset;  
    if (title_margin) {
        page_height = page_height - parseInt(title_margin.substr(0,2));              
    }  
    // $('#page-wrapper').height(page_height);
    $('#page-wrapper').width(page_width);
    $('#title-wrapper').width(page_width);
    $('#toolbar-wrapper').width(page_width);
    $('#navigation').width(page_width);
    $('#subtitle').width(page_width);
    $('#page__flash').width(page_width);
    $('#footer-wrapper').width(page_width);
}