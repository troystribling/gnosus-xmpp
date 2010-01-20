/**********************************************************************************
ui displays
**********************************************************************************/
function GnosusUi(num) {
    this.handlers = {},
    this.item_type_choices = {publications:'contacts', contacts:'resources', resources:'subscriptions', subscriptions:'publications'};
    this.client                    = '#client-'+num;
    this.client_items_content      = '#client-items-content-'+num;
    this.client_items_toolbar      = '#client-items-toolbar-'+num;
    this.client_item_type_selected = '#client-item-type-selected-'+num;
    this.client_items_add          = '#client-items-add-'+num;
    this.client_items_home         = '#client-items-home-'+num;
    this.client_display_content    = '#client-display-content-'+num;
    this.client_display_list       = '#client-display-list-'+num;
    this.client_display_toolbar    = '#client-display-toolbar-'+num;
    this.contact_display_modes      = '#contact-display-modes-'+num;
    $(this.client).splitter({type: "v", outline: true, minLeft: 250, sizeLeft: 250, minRight: 500, cookie: "vsplitter"});
    this.show_items('contacts');
    this.show_display('all_messages');
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
             $(this.client_items_content).empty();
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
             $(this.client_items_content).append(items.join(''));
             $(this.client_items_content+' ul li').hover(
                 function() {$(this).addClass('selected').find('.controls').show();},
                 function() {$(this).removeClass('selected').find('.controls').hide();}
             ); 
             $(this.client_items_content+' ul li').find('.item').click(function() {
                 var item_type = $(this).attr('class').split(' ')[0];
                 $(this).parent('li').find('.selected').removeClass('selected');
                 $(this).addClass('selected')
                 client_ui['show_'+item_type+'_display']($(this).text());
             }); 
             $(this.client_items_content+' ul li').find('img').click(function() {            
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
             $(this.client_items_content+' ul li').find('.selected').removeClass('selected');
             client_ui['home_'+item_type]();
         });
         var type_choices = this.item_type_choices;
         $(this.client_item_type_selected).click(function() {
             $(this).text(type_choices[$(this).text()]);
         });
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
        this.show_all_messages_display();
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
     show_display: function(item_type) {
         this['show_'+item_type+'_display']();
     },               

     /*-------------------------------------------------------------------------------*/    
     show_all_messages_display: function() {
         $(this.client_display_toolbar).empty();
         this.build_content_list(Gnosus.find_all_messages());
         this.unbind();
         this.handlers['chat'] = function (ev, msg) {
             $(this.client_display_list).prepend(this.build_chat_text_message(msg));
         }
         $(document).bind('chat', this.handlers['chat'].bind(this));
     },               

     /*-------------------------------------------------------------------------------*/    
     show_contacts_display: function(contact_name) {
         this.show_contacts_toolbar();
         var display_type = $(this.contact_display_modes+' li.selected').text();
         this['show_contacts_'+display_type+'_display'](contact_name);
     },               

     /*-------------------------------------------------------------------------------*/    
     show_contacts_chat_display: function(contact_name) {
         var contact = Gnosus.find_contact_by_name(contact_name);
         this.build_content_list(Gnosus.find_messages_by_jid_and_type(contact.jid, 'chat'));
         this.unbind();
         this.handlers['chat'] = function (ev, msg) {
             contact_name = $(this.client_items_content+' ul li').find('.selected');
             if (contact_name == msg.name) {
                 $(this.client_display_list).prepend(client_ui.build_chat_text_message(msg));
             }
         }
         $(document).bind('chat', this.handlers['chat'].bind(this));
     },               

    /*-------------------------------------------------------------------------------*/    
    show_contacts_toolbar: function() {
        $(this.client_display_toolbar).empty();
        var toolbar = '<ul id="'+this.to_id(this.contact_display_modes)+'" class="contact-display-modes">' +
                          '<li class="selected">chat</li>' +
                          '<li>commands</li>' +
                          '<li>resources</li>' +
                          '<li>publications</li>' +
                      '</ul>';
        $(this.client_display_toolbar).append(toolbar);
        var client_ui = this;
        $(this.contact_display_modes+' li').click(function() {
            $(this).siblings('li.selected').removeClass('selected');
            $(this).addClass('selected');
            var mode = $(this).text();
            client_ui['show_contacts_'+mode+'_display']();
        });
    }, 

    /*-------------------------------------------------------------------------------*/    
    show_resource_display: function() {
        $(this.client_messages_display).empty();
        this.unbind();
        this.show_resource_toolbar();
    }, 

    /*-------------------------------------------------------------------------------*/    
    show_resource_toolbar: function() {
        $(this.client_messages_toolbar).empty();
    }, 

    /*-------------------------------------------------------------------------------
     * utils 
     *-------------------------------------------------------------------------------*/  
     build_content_list: function(content_list) {
        $(this.client_display_content).empty();
        var msgs = ['<ul id="'+this.to_id(this.client_display_list)+'" class="client-display-list">'];
        var client_ui = this;
        $.each(content_list, function () {
            msgs.push(client_ui['build_'+this.type+'_'+this.content_type+'_message'](this));
        });
        msgs.push('</ul>')
        $(this.client_display_content).append(msgs.join(''));
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
