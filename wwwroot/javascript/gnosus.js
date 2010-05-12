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
        title_height   = $('#title-wrapper').height(),
        title_offset   = 0,
        toolbar_height = $('#toolbar-wrapper').height(),
        toolbar_offset = 0,
        nav_offset     = 87,
        footer_offset  = 61,
        page_width = 0.75*win_width;  
    if (title_height) {
        title_offset = 50;
    } 
    if (toolbar_height) {
        toolbar_offset = 30;
        var browser = navigator.appVersion;
        var webkit_match = new RegExp("WebKit");
        if (webkit_match.test(browser)) {
            footer_offset  = 71;
        }        
    }
    var page_height    = doc_height-nav_offset-footer_offset-toolbar_offset-title_offset;  
    $('#page-wrapper').height(page_height);
    $('#page-wrapper').width(page_width);
    $('#title-wrapper').width(page_width);
    $('#toolbar-wrapper').width(page_width);
    $('#navigation').width(page_width);
    $('#subtitle').width(page_width);
    $('#page__flash').width(page_width);
    $('#footer-wrapper').width(page_width);
}