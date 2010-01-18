/**********************************************************************************
ui displays
**********************************************************************************/
function GnosusUi(num) {
    this.handlers = {},
    this.item_type_choices = {publications:'contacts', contacts:'resources', resources:'subscriptions', subscriptions:'publications'};
    this.client                    = '#client-'+num;
    this.client_items_display      = '#client-items-display-'+num;
    this.client_items_toolbar      = '#client-items-toolbar-'+num;
    this.client_item_type_selected = '#client-item-type-selected-'+num;
    this.client_items_add          = '#client-items-add-'+num;
    this.client_items_home         = '#client-items-home-'+num;
    this.client_messages_display   = '#client-messages-display-'+num;
    this.client_messages_list      = '#client-messages-list-'+num;
    this.client_messages_toolbar   = '#client-messages-toolbar';
    this.client_messages_tools     = '#client-messages-tools-'+num;
    $(this.client).splitter({type: "v", outline: true, minLeft: 250, sizeLeft: 250, minRight: 500, cookie: "vsplitter"});
    this.show_items('contacts');
    this.show_messages_display('contacts');
}

/*--------------------------------------------------------------------------------*/    
GnosusUi.prototype = {
    
    /*-------------------------------------------------------------------------------*/    
    unbind: function() {
        for (var evt in this.handlers) {
            $(document).unbind(evt, this.handlers[evt]);
        } 
        delete(this.handlers);
        this.handlers = {};                   
    },
    
    /*-------------------------------------------------------------------------------*/    
    to_id: function(str) {return str.replace('#','');},

    /*-------------------------------------------------------------------------------  
     * items
     *-------------------------------------------------------------------------------*/    
     show_items: function (item_type) {
         this.handlers['roster_item'] = function (ev, roster) {
             $(this.client_items_display).empty();
             var items = ['<ul>'];
             $.each(Gnosus['find_all_'+item_type](), function () {
                 items.push('<li><div class="'+item_type+' item ' + this.show() + '">');
                 items.push(this.name);
                 items.push('</div><div style="display: none" class="controls">');
                 items.push('<img src="/images/data-delete.png"/>')
                 items.push('</div></li>');
             });
             items.push('</ul>');
             var client_ui = this;
             $(this.client_items_display).append(items.join(''));
             $(this.client_items_display+' ul li').hover(
                 function() {$(this).addClass('selected').find('.controls').show();},
                 function() {$(this).removeClass('selected').find('.controls').hide();}
             ); 
             $(this.client_items_display+' ul li').find('.item').click(function() {
                 var item_type = $(this).attr('class').split(' ')[0];
                 client_ui['display_'+item_type]($(this).text());
             }); 
             $(this.client_items_display+' ul li').find('img').click(function() {            
                 var item_type = $(client_ui.client_item_type_selected).text();
                 client_ui['delete_'+item_type]($(this).parents('li').eq(0).text());
             }); 
         }
         this.show_items_toolbar('contacts', false);
         $(document).bind('roster_item', this.handlers['roster_item'].bind(this));
     },

     /*-------------------------------------------------------------------------------*/    
     show_items_toolbar: function(item_type, add_item) {
         $(this.client_items_toolbar).empty();
         var toolbar = '<div id="'+this.to_id(this.client_items_home)+'" class="client-items-home"/>' +        
                       '<div class="client-item-type-selector">'+ 
                           '<div id="'+this.to_id(this.client_item_type_selected)+'" class="client-item-type-selected">' + item_type + '</div>' +
                       '</div>' +
                       '<div id="'+this.to_id(this.client_items_add)+'" class="client-items-add"/>';  
         $(this.client_items_toolbar).append(toolbar);
         var client_ui = this;
         $(this.client_items_add).click(function() { 
             var item_type = $(client_ui.client_item_type_selected).text();
             client_ui['add_'+item_type]();
         });
         $(this.client_items_home).click(function() {            
             var item_type = $(client_ui.client_item_type_selected).text();
             client_ui['home_'+item_type]();
         });
         var type_choices = this.item_type_choices;
         $(this.client_item_type_selected).click(function() {
             $(this).text(type_choices[$(this).text()]);
         });
     }, 

    /*-------------------------------------------------------------------------------*/    
    display_contacts: function(item) {
    },
    
    /*-------------------------------------------------------------------------------*/    
    display_resources: function(item) {
    },

    /*-------------------------------------------------------------------------------*/    
    display_subscriptions: function(item) {
    },

    /*-------------------------------------------------------------------------------*/    
    display_publications: function(item) {
    },

    /*-------------------------------------------------------------------------------*/    
    delete_contacts: function(item) {
    },
    
    /*-------------------------------------------------------------------------------*/    
    delete_resources: function(item) {
    },

    /*-------------------------------------------------------------------------------*/    
    delete_subscriptions: function(item) {
    },

    /*-------------------------------------------------------------------------------*/    
    delete_publications: function(item) {
    },

    /*-------------------------------------------------------------------------------*/    
    add_contacts: function() {
    },

    /*-------------------------------------------------------------------------------*/    
    add_resources: function() {
    },

    /*-------------------------------------------------------------------------------*/    
    add_subscriptions: function() {
    },

    /*-------------------------------------------------------------------------------*/    
    add_publication: function() {
    },

    /*-------------------------------------------------------------------------------*/    
    home_contacts: function() {
    },

    /*-------------------------------------------------------------------------------*/    
    home_resources: function() {
    },

    /*-------------------------------------------------------------------------------*/    
    home_subscriptions: function() {
    },

    /*-------------------------------------------------------------------------------*/    
    home_publications: function() {
    },

    /*-------------------------------------------------------------------------------  
     * messages
     *-------------------------------------------------------------------------------*/    
     show_messages_display: function(item_type) {
         $(this.client_messages_display).empty();
         var msgs = ['<ul id="'+this.to_id(this.client_messages_list)+'" class="client-messages-list">'];
         var client_ui = this;
         $.each(Gnosus.find_all_messages(), function () {
             msgs.push(client_ui['build_'+self.type+'_'+self.content_type+'_message'](this));
         });
         msgs.push('</ul>')
         $(this.client_messages_display).append(msgs.join(''));
         this.handlers['chat_message'] = function (ev, msg) {
             var msg_model = Gnosus.add_chat_message(msg);
             var chat_msg = client_ui.build_chat_text_message(msg_model)
             $(client_ui.client_messages_list).prepend(chat_msg);
         }
         $(document).bind('chat_message', this.handlers['chat_message'].bind(this));
     },               

    /*-------------------------------------------------------------------------------*/    
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
    show_all_messages_toolbar: function() {
        $(this.client_messages_toolbar).empty();
    },               

    /*-------------------------------------------------------------------------------*/    
    show_command_menu: function() {
        $(this.client_messages_toolbar).empty();
    },
    
    /*-------------------------------------------------------------------------------*/    
    build_chat_text_message: function(msg) {
        var account_rexp = new RegExp(Gnosus.account.jid, 'g');
        var from_to = msg.from.match(account_rexp) ? ('&gt&gt '+Strophe.getBareJidFromJid(msg.to)) : ('&lt&lt '+Strophe.getBareJidFromJid(msg.from));
        var chat = '<li><div class="chat-text-message">' +
                       '<div class="info">' +
                           '<div class="from-to">'+from_to+'</div>' +
                           '<div class="date">'+msg.created_at_as_string()+'</div>' +
                       '</div>' +
                       '<div class="text">'+msg.text+'</div>' +
                   '</div></li>';
        return chat;
    }
    
}
