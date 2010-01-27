/**********************************************************************************
ui displays
**********************************************************************************/
function GnosusUi(num) {
    this.items_handlers = {},
    this.display_handlers = {},
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
    this.contact_display_modes     = '#contact-display-modes-'+num;
    this.client_display_input      = '#client-display-input-'+num;
    this.item_dialog               = '#item-dialog-'+num;
    this.showItems('contacts');
    this.showDisplay('AllMessages');
}

/*--------------------------------------------------------------------------------*/    
GnosusUi.prototype = {
    
    /*-------------------------------------------------------------------------------  
     * utils
     *-------------------------------------------------------------------------------*/    
    itemsUnbind: function() {
        for (var evt in this.items_handlers) {
            $(document).unbind(evt);
            delete(this.items_handlers[evt]);
        } 
    },
    
    /*-------------------------------------------------------------------------------*/    
    displayUnbind: function() {
        for (var evt in this.display_handlers) {
            $(document).unbind(evt);
            delete(this.display_handlers[evt]);
        } 
    },

    /*-------------------------------------------------------------------------------*/    
    capitalize: function(str) {return str.charAt(0).toUpperCase()+str.substr(1);},

    /*-------------------------------------------------------------------------------*/    
    singular: function(str) {return str.replace(/s$/,'');},

    /*-------------------------------------------------------------------------------*/    
    toId: function(str) {return str.replace('#','');},

    /*-------------------------------------------------------------------------------*/    
    contactOpen: function () {
        return $(this.client_items_content+' ul li.open').children('.item').text();
    },

    /*-------------------------------------------------------------------------------*/    
    contactDisplayMode: function () {
        return $(this.contact_display_modes+' li.selected').text();
    },

    /*-------------------------------------------------------------------------------*/    
    itemTypeSelected: function () {
        return $(this.client_item_type_selected).text();
    },
    
    /*-------------------------------------------------------------------------------  
     * items
     *-------------------------------------------------------------------------------*/    
     showItems: function (item_type) {
         this.showItemsToolbar('contacts', false);
         this.itemsUnbind();
         this.items_handlers['roster_init'] = function (ev, roster) {
             $(this.client_items_content).empty();
             var items = '<ul>';
             $.each(Gnosus['findAll'+this.capitalize(item_type)](), function () {
                 items += this.buildItemListItem(this.name, item_type, this.show());
             });
             items += '</ul>';
             $(this.client_items_content).append(items);
             var client_ui = this;
             $(this.client_items_content+' ul li').hover(
                 function() {$(this).addClass('selected').find('.controls').show();},
                 function() {$(this).removeClass('selected').find('.controls').hide();}
             ); 
             $(this.client_items_content+' ul li').find('.item').click(function() {
                 var item_type = $(this).attr('class').split(' ')[0];
                 $(this).parent('li').siblings('.open').removeClass('open');
                 $(this).parent('li').addClass('open')
                 client_ui['show'+client_ui.capitalize(item_type)+'Display']($(this).text());
             }); 
             $(this.client_items_content+' ul li').find('img').click(function() {            
                 var item_type = client_ui.itemTypeSelected();
                 client_ui['delete'+client_ui.capitalize(client_ui.singular(item_type))]($(this).parents('li').eq(0).text());
             }); 
         }
         $(document).bind('roster_init', this.items_handlers['roster_init'].bind(this));
         this.items_handlers['roster_add'] = function (ev, contact) {
             var items = this.buildItemListItem(contact.name, item_type, contact.show());
             $(this.client_items_content).append(items);
             $(this.client_items_content+' ul li:last').hover(
                 function() {$(this).addClass('selected').find('.controls').show();},
                 function() {$(this).removeClass('selected').find('.controls').hide();}
             ); 
             $(this.client_items_content+' ul li:last').find('.item').click(function() {
                 var item_type = $(this).attr('class').split(' ')[0];
                 $(this).parent('li').siblings('.open').removeClass('open');
                 $(this).parent('li').addClass('open')
                 client_ui['show'+client_ui.capitalize(item_type)+'Display']($(this).text());
             }); 
             $(this.client_items_content+' ul li:last').find('img').click(function() {            
                 var item_type = client_ui.itemTypeSelected();
                 client_ui['delete'+client_ui.capitalize(client_ui.singular(item_type))]($(this).parents('li').eq(0).text());
             }); 
         }
         $(document).bind('roster_add', this.items_handlers['roster_add'].bind(this));
         this.items_handlers['presence'] = function (ev, contact) {
             $(this.client_items_content+' ul li').find('.item:contains('+contact.name+')')
                .removeClass('online').removeClass('offline').addClass(contact.show());
         }
         $(document).bind('presence', this.items_handlers['presence'].bind(this));
         this.items_handlers['roster_remove'] = function (ev, contact) {
             $(this.client_items_content+' ul li').find('.item:contains('+contact.name+')').remove();
         }
         $(document).bind('roster_remove', this.items_handlers['roster_remove'].bind(this));
     },

     /*-------------------------------------------------------------------------------*/    
     showItemsToolbar: function(item_type, add_item) {
         $(this.client_items_toolbar).empty();
         var toolbar = '<div id="'+this.toId(this.client_items_home)+'" class="client-items-home"/>' +        
                       '<div class="client-item-type-selector">'+ 
                           '<div id="'+this.toId(this.client_item_type_selected)+'" class="client-item-type-selected">' + item_type + '</div>' +
                       '</div>' +
                       '<div id="'+this.toId(this.client_items_add)+'" class="client-items-add"/>';  
         $(this.client_items_toolbar).append(toolbar);
         var client_ui = this;
         $(this.client_items_add).click(function() { 
             var item_type = client_ui.itemTypeSelected();
             client_ui['add'+client_ui.capitalize(client_ui.singular(item_type))+'Dialog']();
         });
         $(this.client_items_home).click(function() {            
             var item_type = client_ui.itemTypeSelected();
             $(client_ui.client_items_content+' ul li').removeClass('open');
             client_ui['home'+client_ui.capitalize(item_type)]();
         });
         var type_choices = this.item_type_choices;
         $(this.client_item_type_selected).click(function() {
             $(this).text(type_choices[$(this).text()]);
         });
     }, 

    /*-------------------------------------------------------------------------------*/    
    homeContacts: function() {
        this.showAllMessagesDisplay();
    },

    /*-------------------------------------------------------------------------------*/    
    homeResources: function() {
    },

    /*-------------------------------------------------------------------------------*/    
    homeSubscriptions: function() {
    },

    /*-------------------------------------------------------------------------------*/    
    homePublications: function() {
    },

    /*-------------------------------------------------------------------------------*/    
    deleteContact: function(item) {
    },
    
    /*-------------------------------------------------------------------------------*/    
    deleteSubscription: function(item) {
    },

    /*-------------------------------------------------------------------------------*/    
    deletePublication: function(item) {
    },

    /*-------------------------------------------------------------------------------*/    
    addContactDialog: function() {
        var dialog = '<div id="'+this.toId(this.item_dialog)+'" title="add contact"/>' +        
                     '</div>'; 
        $(this.client).append(dialog); 
        $(this.item_dialog).dialog({modal:true, resizable:false,
            buttons:{'send':this.addContact.bind(this)}});            
    },

    /*-------------------------------------------------------------------------------*/    
    addSubscriptionDialog: function() {
    },

    /*-------------------------------------------------------------------------------*/    
    addPublicationDialog: function() {
    },

    /*-------------------------------------------------------------------------------*/    
    addContact: function() {
        this.removeItemDialog();            
    },

    /*-------------------------------------------------------------------------------*/    
    addSubscription: function() {
    },

    /*-------------------------------------------------------------------------------*/    
    addPublication: function() {
    },

    /*-------------------------------------------------------------------------------*/    
    removeItemDialog: function() {
        $(this.item_dialog).remove();            
    },


    /*-------------------------------------------------------------------------------  
     * messages
     *-------------------------------------------------------------------------------*/    
     showDisplay: function(item_type) {
         this['show'+this.capitalize(item_type)+'Display']();
     },               

     /*-------------------------------------------------------------------------------*/    
     showAllMessagesDisplay: function() {
         $(this.client_display_content).empty();
         this.displayUnbind();
         $(this.client_display_toolbar).empty();
         this.buildContentList('no-input', Gnosus.findAllMessages());
         this.display_handlers['chat'] = function (ev, msg) {
             $(this.client_display_list).prepend(this.buildChatTextMessage(msg));
         }
         $(document).bind('chat', this.display_handlers['chat'].bind(this));
     },               

     /*-------------------------------------------------------------------------------*/    
     showContactsDisplay: function(contact_name) {
         this.showContactsToolbar();
         var display_type = this.contactDisplayMode();
         this['showContacts'+this.capitalize(display_type)+'Display'](contact_name);
     },               

     /*-------------------------------------------------------------------------------*/    
     showContactsChatDisplay: function(contact_name) {
         $(this.client_display_content).empty();
         this.displayUnbind();
         var enter_msg = 'enter message',
             contact = Gnosus.findContactByName(contact_name),
             send_message = '<div id="'+this.toId(this.client_display_input)+'"class ="client-display-input">'+
                                '<textarea class="init">'+enter_msg+'</textarea>'+
                            '</div>';
         $(this.client_display_content).append(send_message);
         this.buildContentList('input', Gnosus.findMessagesByJidAndType(contact.jid, 'chat'));
         var client_ui = this,
             textarea = $(this.client_display_input+' textarea'),
             orig_textarea_height = textarea.height();
         textarea.autoResize();
         textarea.keyup(function(evt) {
             var input = evt.keyCode;
             if (input == '13') {
                 var contact_name = client_ui.contactOpen(),
                     contact = Gnosus.findContactByName(contact_name),
                     msg = $(this).val().replace(/\n$/,'');
                 $(this).val('');
                 $(this).height(orig_textarea_height);
                 $(this).css('overflow','hidden');
                 $(client_ui.client_display_list).prepend(client_ui.buildChatTextMessage(GnosusXmpp.chatTextMessage(contact.jid, msg)));
            }
         });
         textarea.blur(function() {
             if ($(this).val() == '') {
                 $(this).val(enter_msg);
                 $(this).addClass('init');
                 $(this).height(orig_textarea_height);
             }
         });
         textarea.focus(function() {
             if ($(this).hasClass('init')) {
                 $(this).removeClass('init');
                 $(this).val('');
             }
         });
         this.display_handlers['chat'] = function (ev, msg) {
             var contact_name = client_ui.contactOpen(),
                 contact = Gnosus.findContactByName(contact_name);
             if (msg.from.match(new RegExp(contact.jid, 'g'))) {
                 $(this.client_display_list).prepend(this.buildChatTextMessage(msg));
             }
         }
         $(document).bind('chat', this.display_handlers['chat'].bind(this));
     },               

     /*-------------------------------------------------------------------------------*/    
     showContactsResourcesDisplay: function(contact_name) {
     },               

     /*-------------------------------------------------------------------------------*/    
     showContactsCommandsDisplay: function(contact_name) {
     },               

     /*-------------------------------------------------------------------------------*/    
     showContactsPublicationsDisplay: function(contact_name) {
     },               

    /*-------------------------------------------------------------------------------*/    
    showContactsToolbar: function() {
        $(this.client_display_toolbar).empty();
        var toolbar = '<ul id="'+this.toId(this.contact_display_modes)+'" class="contact-display-modes">' +
                          '<li class="selected">chat</li>' +
                          '<li>commands</li>' +
                          '<li>resources</li>' +
                          '<li>publications</li>' +
                      '</ul>';
        $(this.client_display_toolbar).append(toolbar);
        var client_ui = this;
        $(this.contact_display_modes+' li').click(function() {
            var contact_name = client_ui.contactOpen()
            $(this).siblings('li.selected').removeClass('selected');
            $(this).addClass('selected');
            var mode = $(this).text();
            client_ui['showContacts'+this.capitalize(mode)+'Display'](contact_name);
        });
    }, 

    /*-------------------------------------------------------------------------------*/    
    showResourceDisplay: function() {
        $(this.client_messages_display).empty();
    }, 

    /*-------------------------------------------------------------------------------*/    
    showResourceToolbar: function() {
        $(this.client_messages_toolbar).empty();
    }, 

    /*-------------------------------------------------------------------------------
     * utils 
     *-------------------------------------------------------------------------------*/  
     buildContentList: function(list_type, content_list) {
        var msgs = ['<ul id="'+this.toId(this.client_display_list)+'" class="client-display-list '+list_type+'">'];
        var client_ui = this;
        $.each(content_list, function () {
            msgs.push(client_ui['build'+client_ui.capitalize(this.type)+client_ui.capitalize(this.content_type)+'Message'](this));
        });
        msgs.push('</ul>')
        $(this.client_display_content).append(msgs.join(''));
    },   
      
    /*-------------------------------------------------------------------------------*/    
    buildChatTextMessage: function(msg) {
        var account_rexp = new RegExp(Gnosus.account.jid, 'g'),
            from = msg.from,
            chat = '<li><div class="chat-text-message">' +
                       '<div class="info">' +
                           '<div class="from">'+from+'</div>' +
                           '<div class="date">'+msg.createdAtAsString()+'</div>' +
                       '</div>' +
                       '<div class="text">'+msg.text+'</div>' +
                   '</div></li>';
        return chat;
    }
    
    /*-------------------------------------------------------------------------------*/ 
    buildItemListItem: function (item_name, item_type, item_status) : { 
        var status = item_status || '',  
            item = '<li>' +
                       '<div class="'+item_type+' item ' + status + '">' + 
                           item_name +
                       '</div>' +
                       '<div style="display: none" class="controls">' +
                           '<img src="/images/data-delete.png"/>' +
                       '</div>' +
                   '</li>';
        return item;
    }
    
};

/**********************************************************************************
 plugins
/**********************************************************************************/
(function($){    
    $.fn.autoResize = function() {
        this.filter('textarea').each(function(){
            var textarea = $(this).css({resize:'none','overflow':'hidden'}),
                origHeight = textarea.height(),
                extraSpace = 9,
                limit = 109;
                clone = (function(){
                    var props = {};
                    $.each(['height','width','lineHeight','textDecoration','letterSpacing'], function(i, prop){
                        props[prop] = textarea.css(prop);
                    });
                    return textarea.clone().removeAttr('id').removeAttr('name').css({
                        position: 'absolute',
                        top: 0,
                        left: -9999
                    }).css(props).attr('tabIndex','-1').insertBefore(textarea);
                             
                })();
                updateSize = function() {
                    clone.height(0).val($(this).val()).scrollTop(10000);
                    var height = Math.max(clone.scrollTop(), origHeight);
                    if (height >= limit) {
                        $(this).css('overflow-y','scroll');
                        return;
                    } else if (height > origHeight) {
                        height += extraSpace;
                    }
                    $(this).height(height);
                };
                $(this).keyup(updateSize);            
        });
        return this;        
    };    
})(jQuery);