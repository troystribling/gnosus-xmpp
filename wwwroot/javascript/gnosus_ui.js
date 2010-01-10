/**********************************************************************************
ui
**********************************************************************************/
function GnosusUi() {
    this.handlers = {},
    this.client = '#client';
    this.client_items_display = '#client-items-display';
    this.client_messages_display = '#client-messages-display';
    this.client_items_toolbar = '#client-items-toolbar';
    this.client_messages_toolbar = '#client-messages-toolbar';
    $('#client').splitter({type: "v", outline: true, minLeft: 200, sizeLeft: 200, minRight: 500, cookie: "vsplitter"});
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
        $(document).bind('roster_changed', this.handlers['roster_changed'].bind(this));
    }        
}
