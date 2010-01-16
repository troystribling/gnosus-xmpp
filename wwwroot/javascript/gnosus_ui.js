/**********************************************************************************
ui displays
**********************************************************************************/
function GnosusUi(num) {
    this.handlers = {},
    this.client = '#client-'+num;
    this.client_items_display = '#client-items-display-'+num;
    this.client_items_toolbar = '#client-items-toolbar-'+num;
    this.client_item_type_selected = '#client-item-type-selected-'+num;
    this.client_item_type_choices = '#client-item-type-choices-'+num;
    this.client_items_add = '#client-items-add-'+num;
    this.client_items_home = '#client-items-home-'+num;
    this.client_messages_display = '#client-messages-display-'+num;
    this.client_messages_list = '#client-messages-list-'+num;
    this.client_messages_toolbar = '#client-messages-toolbar';
    this.client_messages_tools = '#client-messages-tools-'+num;
    $(this.client).splitter({type: "v", outline: true, minLeft: 250, sizeLeft: 250, minRight: 500, cookie: "vsplitter"});
    this.show_items();
    this.show_all_messages_display();
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
    to_id: function(str) {return str.replace('#','');},
    
    /*-------------------------------------------------------------------------------  
     *items
     *-------------------------------------------------------------------------------*/    
    show_items: function () {
        this.handlers['roster_item'] = function (ev, roster) {
            $(this.client_items_display).empty();
            var items = ["<ul>"];
            $.each(Gnosus.find_all_contacts(), function () {
                items.push('<li><div class="contact ' + this.show() + '">');
                items.push(this.name || this.jid);
                items.push("</div></li>");
            });
            items.push("</ul>");
            $(this.client_items_display).append(items.join(''));
        }
        this.show_items_toolbar('contacts', false);
        $(document).bind('roster_item', this.handlers['roster_item'].bind(this));
    },
    
    /*-------------------------------------------------------------------------------*/    
    show_items_toolbar: function(selected, add_item) {
        $(this.client_items_toolbar).empty();
        var toolbar = '<div id="'+this.to_id(this.client_items_home)+'" class="client-items-home"/>' +        
                      '<div class="client-item-type-selector">'+ 
                          '<div id="'+this.to_id(this.client_item_type_selected)+'" class="client-item-type-selected">' + selected + '</div>' +
                          '<ul id="'+this.to_id(this.client_item_type_choices)+'" style="display: none" class="client-item-type-choices">' +
                               '<li>contacts</li>' +
                               '<li>resources</li>' +
                               '<li>subscriptions</li>' +
                               '<li>publications</li>' +
                          '</ul>' +
                      '</div>' +
                      '<div id="'+this.to_id(this.client_items_add)+'" class="client-items-add"/>';  
        $(this.client_items_toolbar).append(toolbar);
        $(this.client_item_add).click(function() {            
        });
        $(this.client_item_home).click(function() {            
        });
        var type_choices = this.client_item_type_choices;
        $(this.client_item_type_selected).click(function() {
            $(this).toggleClass('open')
            $(type_choices).toggle();
        });
        this.select_item_type();
    }, 

    /*-------------------------------------------------------------------------------*/    
    select_item: function() {
        $(this.client_item_type_choices+' li').hover(
            function() {$(this).addClass('choice');}, 
            function() {$(this).removeClass('choice');}
        ); 
        $(type_choices+' li').click(function() {
        }); 
    }, 
        
    /*-------------------------------------------------------------------------------*/    
    select_item_type: function() {
        $(this.client_item_type_choices+' li').hover(
            function() {$(this).addClass('choice');}, 
            function() {$(this).removeClass('choice');}
        ); 
        var type_selected = this.client_item_type_selected;
        var type_choices = this.client_item_type_choices;
        $(type_choices+' li').click(function() {
            $(type_selected).toggleClass('open')
            $(type_selected).text($(this).text());
            $(type_choices).toggle();
        }); 
    }, 
    
    /*-------------------------------------------------------------------------------  
     *messages
     *-------------------------------------------------------------------------------*/    
    show_contact_messages_toolbar: function() {
        $(this.client_messages_toolbar).empty();
        var toolbar = '<ul id="'+this.to_id(this.client_messages_tools)+'" class="client-messages-tools">' +
                          '<li>chat</li>' +
                          '<li>commands</li>' +
                          '<li>resources</li>' +
                          '<li>publications</li>' +
                      '</ul>';
        $(this.client_items_toolbar).append(toolbar);
    }, 

    /*-------------------------------------------------------------------------------*/    
    show_resource_messages_display: function() {
        $(this.client_messages_display).empty();
    }, 

    /*-------------------------------------------------------------------------------*/    
    show_resource_messages_toolbar: function() {
        $(this.client_messages_toolbar).empty();
    }, 

    /*-------------------------------------------------------------------------------*/    
    show_all_messages_display: function() {
        $(this.client_messages_display).empty();
        var msgs = ['<ul id="'+this.to_id(this.client_messages_list)+'" class="client-messages-list">'];
        $.each(Gnosus.find_all_messages(), function () {
            msgs.push('<li><div class="chat-message">');
            msgs.push(this.text);
            msgs.push('</div></li>');
        });
        msgs.push('</ul>')
        $(this.client_messages_display).append(msgs.join(''));
        this.handlers['chat_message'] = function (ev, msg) {
        }
        $(document).bind('chat_message', this.handlers['chat_message'].bind(this));
    },               

    /*-------------------------------------------------------------------------------*/    
    show_all_messages_toolbar: function() {
        $(this.client_messages_toolbar).empty();
    },               

    /*-------------------------------------------------------------------------------*/    
    show_command_menu: function() {
        $(this.client_messages_toolbar).empty();
    },
}
