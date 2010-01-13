/**********************************************************************************
ui displays
**********************************************************************************/
function GnosusUi() {
    this.handlers = {},
    this.client = '#client';
    this.client_items_display = '#client-items-display';
    this.client_items_toolbar = '#client-items-toolbar';
    this.client_item_selected = '#client-item-selected';
    this.client_item_choices = '#client-item-choices';
    this.client_messages_display = '#client-messages-display';
    this.client_messages_toolbar = '#client-messages-toolbar';
    $('#client').splitter({type: "v", outline: true, minLeft: 250, sizeLeft: 250, minRight: 500, cookie: "vsplitter"});
    this.show_roster();
}

/*--------------------------------------------------------------------------------*/    
GnosusUi.prototype = {
    
    /*-------------------------------------------------------------------------------*/    
    unbind: function() {
        for (var evt in this.handlers) {
            $(document).unbind(evt, this.handlers[evt]);
        } 
        this.handlers = {};                   
    },
    
    /*-------------------------------------------------------------------------------*/    
    show_roster: function () {
        this.handlers['roster_changed'] = function (ev, roster) {
            $(this.client_items_display).empty();
            var html = ["<ul>"];
            $.each(Gnosus.find_all_contacts(), function (jid) {
                html.push('<li><div class="contact ' + this.show() + '">');
                html.push(this.name || jid);
                html.push("</div></li>");
            });
            html.push("</ul>");
            $(this.client_items_display).append(html.join(''));
        }
        this.show_items_toolbar('roster', false);
        $(document).bind('roster_changed', this.handlers['roster_changed'].bind(this));
    },
    
    /*-------------------------------------------------------------------------------*/    
    show_items_toolbar: function(selected, add_item) {
        $(this.client_items_toolbar).empty();
        var item_select = '<div id="client-item-selector">'+ 
                              '<div id="client-item-selected">' + selected + '</div>' +
                              '<ul id="client-item-choices" style="display: none">' +
                                  '<li>roster</li>' +
                                  '<li>resources</li>' +
                                  '<li>subscriptions</li>' +
                                  '<li>publications</li>' +
                              '</ul>' +
                          '</div>';  
        $(this.client_items_toolbar).append(item_select);
        var item_choices = this.client_item_choices;
        $(this.client_item_selected).click(function() {
            $(this).toggleClass('open')
            $(item_choices).toggle();
        });
        this.select_item();
    }, 
    
    /*-------------------------------------------------------------------------------*/    
    select_item: function() {
        $(this.client_item_choices+' li').hover(
            function() {$(this).addClass('choice');}, 
            function() {$(this).removeClass('choice');}
        ); 
        var item_selected = this.client_item_selected;
        var item_choices = this.client_item_choices;
        $(this.client_item_choices+' li').click(function() {
            $(item_selected).toggleClass('open')
            $(item_selected).text($(this).text());
            $(item_choices).toggle();
        }); 
    }           
}
