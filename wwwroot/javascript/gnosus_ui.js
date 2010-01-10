/**********************************************************************************
ui manager
**********************************************************************************/
GnosusUiMgr = {
    
    /*----------------------------------------------------------------------------*/    
    clients: {},

    /*----------------------------------------------------------------------------*/ 
    add_client: function(client_id) { 
        GnosusUiMgr.clients[client_id] = new GnosusUi(client_id);       
    }   
    remove_client: function(client_id) {
        GnosusUiMgr.clients[client_id].unbind();
        delete(GnosusUiMgr.clients[client_id])      
    }   
}

/**********************************************************************************
ui
**********************************************************************************/
function GnosusUi(client_id) {
    this.handlers = {},
    this.client_id = client_id,
    this.client_display = '#client-display-'+this.client_id;
    this.client_display_toolbar = '#client-display-toolbar-'+this.client_id;
    this.client_toolbar = '#client-toolbar-'+this.client_id;
}

/*--------------------------------------------------------------------------------*/    
GnosusUi.prototype = {
    
    /*-------------------------------------------------------------------------------*/    
    unbind: function() {
        for (var evt in this.handlers) {
            $(this.client_display).unbind(evt, this.handlers[evt]);
        } 
        this.handlers = {};                   
    },
    
    /*-------------------------------------------------------------------------------*/    
    show_roster: function () {
        this.handlers['roster_changed'] = function (ev, roster) {
            $(this.client_display).empty();
            var html = ["<ul>"];
            $.each(Gnosus.find_all_contacts(), function (jid) {
                html.push("<li><div class='item'" + this.show() + "'>");
                html.push(this.name || jid);
                html.push("</div></li>");
            });
            html.push("</ul>");
            $(this.client_display).append(html.join(''));
        }
        $(document).bind('roster_changed', this.handlers['roster_changed']);
    }
        
}
