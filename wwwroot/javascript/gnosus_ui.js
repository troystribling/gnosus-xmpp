/**********************************************************************************
ui displays
**********************************************************************************/
function GnosusUi(num) {
    this.items_handlers = {},
    this.display_handlers = {},
    this.item_type_choices = {publications:'contacts', contacts:'resources', resources:'subscriptions', subscriptions:'publications'};
    this.client                         = '#client-'+num;
    this.client_items_content           = this.client+' .client-items-content';
    this.client_items_toolbar           = this.client+' .client-items-toolbar';
    this.client_item_type_selected      = this.client+' .client-item-type-selected';
    this.client_items_add               = this.client+' .client-items-add';
    this.client_items_history           = this.client+' .client-items-history';
    this.client_display_content         = this.client+' .client-display-content';
    this.client_display_list            = this.client+' .client-display-list';
    this.client_display_toolbar         = this.client+' .client-display-toolbar';
    this.client_display_content_control = this.client+' .client-display-toolbar div.control';
    this.contact_display_modes          = this.client+' .contact-display-modes';
    this.client_display_input           = this.client+' .client-display-input';
    this.item_dialog                    = '#item-dialog-'+num;
    this.showItems('contacts');
    this.showDisplay('AllMessages');
    this.block('connecting')
}

/*--------------------------------------------------------------------------------*/    
GnosusUi.prototype = {
    
    /*-------------------------------------------------------------------------------  
     * utils
     *-------------------------------------------------------------------------------*/    
    itemsUnbind: function() {
        $.each(this.items_handlers, function(e, h) {
            $(document).unbind(e);
            delete(h);
        });
    },
    
    /*-------------------------------------------------------------------------------*/    
    displayUnbind: function() {
        $.each(this.display_handlers, function(e,h) {
            $(document).unbind(e);
            delete(h);
        });
    },

    /*-------------------------------------------------------------------------------*/    
    capitalize: function(str) {return str.charAt(0).toUpperCase()+str.substr(1);},

    /*-------------------------------------------------------------------------------*/    
    camelize: function(str) {
        var client_ui = this;
        return $.map(str.split('_'), function(s,i) {
                   return client_ui.capitalize(s);
               }).join('');
    },

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
         this['addEvents'+this.capitalize(item_type)]();
     },

     /*-------------------------------------------------------------------------------*/    
     showItemsToolbar: function(item_type, add_item) {
         $(this.client_items_toolbar).empty();
         var toolbar = '<div class="client-items-history"/>' +        
                       '<div class="client-item-type-selector">'+ 
                           '<div class="client-item-type-selected">' + item_type + '</div>' +
                       '</div>' +
                       '<div class="client-items-add"/>';  
         $(this.client_items_toolbar).append(toolbar);
         var client_ui = this;
         $(this.client_items_add).click(function() { 
             var item_type = client_ui.itemTypeSelected();
             client_ui['add'+client_ui.capitalize(client_ui.singular(item_type))+'Dialog']();
         });
         $(this.client_items_history).click(function() {            
             var item_type = client_ui.itemTypeSelected();
             $(client_ui.client_items_content+' ul li').removeClass('open');
             client_ui['history'+client_ui.capitalize(item_type)]();
         });
         var type_choices = this.item_type_choices;
         $(this.client_item_type_selected).click(function() {
             $(this).text(type_choices[$(this).text()]);
         });
     }, 

     /*-------------------------------------------------------------------------------*/  
     addEventsContacts: function() {  

         /*---- roster messages ----*/
         this.items_handlers['roster_init_result'] = function (ev, roster) {
             $(this.client_items_content).empty();
             var items = '<ul>';
             var client_ui = this;
             $.each(Gnosus.findAllContacts(), function () {
                 items += client_ui.buildItemListItem(this.name, 'contacts', this.show());
             });
             items += '</ul>';
             $(this.client_items_content).append(items);
             this.addItemListEvents($(this.client_items_content+' ul li'));
             this.unblock();             
         }
         $(document).bind('roster_init_result', this.items_handlers['roster_init_result'].bind(this));

         /****/
         this.items_handlers['roster_init_error'] = function (ev, roster) {
             this.unblock();             
             this.errorDialog('roster init failed');
         }
         $(document).bind('roster_init_error', this.items_handlers['roster_init_error'].bind(this));
         
         /****/
         this.items_handlers['roster_item_add'] = function (ev, contact) {
             var items = this.buildItemListItem(contact.name, 'contacts', contact.show());
             $(this.client_items_content+' ul').append(items);
             this.addItemListEvents($(this.client_items_content+' ul li:last'));             
         }
         $(document).bind('roster_item_add', this.items_handlers['roster_item_add'].bind(this));
         
         /****/
         this.items_handlers['roster_item_remove'] = function (ev, contact) {
             $(this.client_items_content+' ul li').find('.item:contains('+contact.name+')').remove();
         }
         $(document).bind('roster_item_remove', this.items_handlers['roster_item_remove'].bind(this));
         
         /****/
         this.items_handlers['roster_item_add_response'] = function (ev, iq) {
             this.unblock();
         }
         $(document).bind('roster_item_add_response', this.items_handlers['roster_item_add_response'].bind(this));
         
         /****/
         this.items_handlers['roster_item_add_error'] = function (ev, iq) {
             this.unblock();
             this.errorDialog('failed to add <strong>'+$(iq).attr('from')+'</strong>');
         }
         $(document).bind('roster_item_add_error', this.items_handlers['roster_item_add_error'].bind(this));
         
         /****/
         this.items_handlers['roster_remove_response'] = function (ev, iq) {
             this.unblock();
         }
         $(document).bind('roster_item_remove_response', this.items_handlers['roster_remove_response'].bind(this));
         
         /****/
         this.items_handlers['roster_remove_error'] = function (ev, iq) {
             this.unblock();
             this.errorDialog('failed to remove <strong>'+$(iq).attr('from')+'</strong>');
         }
         $(document).bind('roster_item_remove_error', this.items_handlers['roster_remove_error'].bind(this));
         
         /*---- presence messages ----*/
         this.items_handlers['presence'] = function (ev, contact) {
             $(this.client_items_content+' ul li').find('.item:contains('+contact.name+')')
                .removeClass('online').removeClass('offline').addClass(contact.show());
         }
         $(document).bind('presence', this.items_handlers['presence'].bind(this));
         
         /****/
         this.items_handlers['presence_unavailable'] = function (ev, contact) {
             $(this.client_items_content+' ul li').find('.item:contains('+contact.name+')')
                .removeClass('online').removeClass('offline').addClass(contact.show());
         }
         $(document).bind('presence_unavailable', this.items_handlers['presence_unavailable'].bind(this));
         
         /****/
         this.items_handlers['presence_subscribe'] = function (ev, jid) {
             this.subscriptionRequestDialog(jid);
         }
         $(document).bind('presence_subscribe', this.items_handlers['presence_subscribe'].bind(this));
         
         /****/
         this.items_handlers['presence_unsubscribed'] = function (ev, contact) {
             $(this.client_items_content+' ul li').find('.item:contains('+contact.name+')')
                .removeClass('online').removeClass('offline').addClass(contact.show());
         }
         $(document).bind('presence_unsubscribed', this.items_handlers['presence_unsubscribed'].bind(this));
     },

     /*-------------------------------------------------------------------------------*/    
     errorDialog: function(msg) {
         var dialog = '<div id="'+this.toId(this.item_dialog)+'" title="error" class="ui-widget">'+ 
 			               '<div class="ui-state-error ui-corner-all" style="padding: 0 .7em;">'+ 
				               '<p>'+msg+'</p>'+
			               '</div>'+
                     '</div>'; 
         $(this.item_dialog).remove();            
         $(this.client).append(dialog); 
         $(this.item_dialog).dialog({modal:true, resizable:false, width:480,
             buttons:{'ok':this.cancelItemDialog.bind(this)}});
     },

    /*-------------------------------------------------------------------------------*/    
    historyContacts: function() {
        this.showAllMessagesDisplay();
    },

    /*-------------------------------------------------------------------------------*/    
    historyResources: function() {
    },

    /*-------------------------------------------------------------------------------*/    
    historySubscriptions: function() {
    },

    /*-------------------------------------------------------------------------------*/    
    historyPublications: function() {
    },

    /*-------------------------------------------------------------------------------*/    
    deleteContactDialog: function(item) {
        var dialog = '<div id="'+this.toId(this.item_dialog)+'" title="delete contact?">'+ 
                        '<p>'+item+'</p>'+
                     '</div>'; 
        $(this.item_dialog).remove();            
        $(this.client).append(dialog); 
        $(this.item_dialog).dialog({modal:true, resizable:false, width:380,
            buttons:{'cancel':this.cancelItemDialog.bind(this), 
                     'delete':function() {
                         this.block('deleting contact');
                         var contact = Gnosus.findContactByName($(this.item_dialog+' p').text());
                         GnosusXmpp.removeContact(contact.jid);
                         this.cancelItemDialog();            
                     }.bind(this)}});
    },
    
    /*-------------------------------------------------------------------------------*/    
    deleteSubscriptionDialog: function(item) {
    },

    /*-------------------------------------------------------------------------------*/    
    deletePublicationDialog: function(item) {
    },

    /*-------------------------------------------------------------------------------*/    
    addContactDialog: function() {
        var dialog = '<div id="'+this.toId(this.item_dialog)+'" class="form" title="add contact">'+  
                         '<label for="jid">jid</label><input type="text" name="jid" class="jid"/></br>'+
                     '</div>'; 
        $(this.item_dialog).remove();            
        $(this.client).append(dialog); 
        $(this.item_dialog).dialog({modal:true, resizable:false,
            buttons:{'cancel':this.cancelItemDialog.bind(this), 
                     'send':function() {
                                var jid = $(this.item_dialog+' input.jid').val();
                                this.cancelItemDialog(); 
                                if (jid.match(/\@/)) {
                                    this.block('adding contact')
                                    GnosusXmpp.addContact(jid, null, []); 
                                } else if (jid =='') {
                                    this.errorDialog('JID missing');
                                } else {
                                    this.errorDialog('<strong>'+jid+'</strong> is an invalid JID');
                                }        
                            }.bind(this)},
            dialogClass:'add-contact-dialog', width:380});            
    },

    /*-------------------------------------------------------------------------------*/    
    cancelItemDialog: function() {
        $(this.item_dialog).dialog("close");        
        $(this.item_dialog).remove();            
    },

    /*-------------------------------------------------------------------------------*/    
    subscriptionRequestDialog: function(jid) {
        var dialog = '<div id="'+this.toId(this.item_dialog)+'" title="contact request from?">'+ 
                        '<p>'+jid+'</p>'+
                     '</div>'; 
        $(this.item_dialog).remove();            
        $(this.client).append(dialog); 
        $(this.item_dialog).dialog({modal:true, resizable:false, width:380,
            buttons:{'accept':function() {
                                  this.block('adding contact')
                                  GnosusXmpp.addContact(jid, null, []);
                                  GnosusXmpp.acceptSubscriptionRequest($(this.item_dialog+' p').text());
                                  this.cancelItemDialog();            
                              }.bind(this),
                     'reject':function() {
                                  GnosusXmpp.rejectSubscriptionRequest($(this.item_dialog+' p').text());
                                  this.cancelItemDialog();            
                              }.bind(this),}});
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
         $(this.client_display_toolbar).empty();
         this.displayUnbind();
         this.buildContentList('no-input', Gnosus.findAllMessages());
         this.display_handlers['chat'] = function (ev, msg) {
             $(this.client_display_list).prepend(this.buildChatTextMessage(msg));
         }
         $(document).bind('chat', this.display_handlers['chat'].bind(this));
     },               

     /*-------------------------------------------------------------------------------*/    
     showContactsDisplay: function(contact_name) {
         this.showContactsToolbar();
         this.showContactsContentDisplay(contact_name);
     },               

     /*-------------------------------------------------------------------------------*/    
     showContactsContentDisplay: function(contact_name) {
         $(this.client_display_content_control).empty();
         $(this.client_display_content).empty();
         this.displayUnbind();
         var display_type = this.contactDisplayMode();
         this['showContacts'+this.capitalize(display_type)+'Display'](contact_name);
     },               

     /*-------------------------------------------------------------------------------*/    
     showContactsChatDisplay: function(contact_name) {
         var enter_msg = 'enter message',
             contact = Gnosus.findContactByName(contact_name),
             send_message = '<div class ="client-display-input">'+
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
     showContactsCommandsDisplay: function(contact_name) {
          var toolbar = '<div class="add"></div>',
              contact = Gnosus.findContactByName(contact_name);
         $(this.client_display_content_control).append(toolbar);
         contact.deleteCommands();
         this.block('retrieving command list');
         $.each(contact.resources, function(j,r) {
             GnosusXmpp.getCommandList(r.jid);
         });
         $(this.client_display_content_control+' .add').click(function() {
             this.contactCommandsDialog(contact);
         }.bind(this));
         this.display_handlers['command_list_response'] = function (ev, jid) {
             if (Gnosus.areCommandsAvailable(contact.jid)) {
                 this.unblock();
                 this.buildContentList('no-input', Gnosus.findMessagesByJidAndContentType(contact.jid, 'command'));
             }
         }
         $(document).bind('command_list_response', this.display_handlers['command_list_response'].bind(this));
         this.display_handlers['command_list_error'] = function (ev, jid) {
             this.unblock();
             this.errorDialog('failed to retrieve command list');
         }
         $(document).bind('command_list_error', this.display_handlers['command_list_error'].bind(this));
         this.display_handlers['command_response'] = function (ev, msg) {
             $(this.client_display_list).prepend(this.buildXCommandMessage(msg));
             this.unblock();
         }
         $(document).bind('command_response', this.display_handlers['command_response'].bind(this));
         this.display_handlers['command_error'] = function (ev, jid) {
             this.unblock();
             this.errorDialog('command request failed');
         }
         $(document).bind('command_error', this.display_handlers['command_error'].bind(this));
     },               

     /*-------------------------------------------------------------------------------*/    
     contactCommandsDialog: function(contact) {
         var commands  = Gnosus.findAllCommands(contact.jid),
             dialog    = '<div id="'+this.toId(this.item_dialog)+'" title="commands">'+'<div class="commands">',
             cats      = {},
             client_ui = this;
         $.each(commands, function(n,c) {
             var parts = n.split('/'),
                 cat = 'ungrouped',
                 cmd = n;
             if (parts.length > 1) {
                 cat = parts.shift();
                 cmd = parts.join('/');
             }
             if (!cats[cat]) {
                 cats[cat]= [];
             }
             cats[cat].push(cmd);
         });
         $.each(cats, function(cat, cmd) {
             dialog += '<h3><a href="#" class="command-category">'+cat+'</a></h3>'+
                       '<div>';
             $.each(cmd, function() {
                 dialog += '<div class="command">'+this+'</div>';
             });
             dialog += '</div>';
         });
         dialog += '</div></div>';
         $(this.item_dialog).remove();            
         $(this.client).append(dialog); 
         $(this.item_dialog).dialog({modal:true, resizable:false, 
             buttons:{'cancel':this.cancelItemDialog.bind(this)},
             dialogClass:'command-dialog', width:400, height:400}); 
         $(this.item_dialog+' .commands').accordion({collapsible: true});                
         $(this.item_dialog+' .command').click(function() {
             var node = $(this).parent('div').prev('h3').text()+'/'+$(this).text().replace(/ /g,'_');
             $.each(contact.resources, function(jid, r) {
                 $(client_ui.client_display_list).prepend(client_ui.buildCommandRequestCommandMessage(GnosusXmpp.sendCommand({to:jid, node:node})));
             });
             client_ui.cancelItemDialog();
             client_ui.block('command request pending');
         });
     },

     /*-------------------------------------------------------------------------------*/    
     showContactsResourcesDisplay: function(contact_name) {
     },               

     /*-------------------------------------------------------------------------------*/    
     showContactsPublicationsDisplay: function(contact_name) {
     },               

    /*-------------------------------------------------------------------------------*/    
    showContactsToolbar: function() {
        $(this.client_display_toolbar).empty();
        var toolbar = '<ul class="contact-display-modes">'+
                          '<li class="selected">chat</li>'+
                          '<li>commands</li>'+
                          '<li>resources</li>'+
                          '<li>publications</li>'+
                      '</ul>' +
                      '<div class="control"></div>';
        $(this.client_display_toolbar).append(toolbar);
        var client_ui = this;
        $(this.contact_display_modes+' li').click(function() {
            var contact_name = client_ui.contactOpen()
            $(this).siblings('li.selected').removeClass('selected');
            $(this).addClass('selected');
            client_ui.showContactsContentDisplay(contact_name);
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
        var msgs = ['<ul class="client-display-list '+list_type+'">'];
        var client_ui = this;
        $.each(content_list, function () {
            var msg = 'build'+client_ui.camelize(this.type)+client_ui.camelize(this.content_type)+'Message';
            msgs.push(client_ui['build'+client_ui.camelize(this.type)+client_ui.camelize(this.content_type)+'Message'](this));
        });
        msgs.push('</ul>')
        $(this.client_display_content).append(msgs.join(''));
    },   
      
    /*-------------------------------------------------------------------------------*/ 
    buildItemListItem: function (item_name, item_type, item_status) { 
        var status = item_status || '',  
            item = '<li>' +
                       '<div class="'+item_type+' item '+status+'">'+ 
                           item_name+
                       '</div>'+
                       '<div style="display: none" class="controls">'+
                           '<img src="/images/data-delete.png"/>'+
                       '</div>'+
                   '</li>';
        return item;
    },
    
    /*-------------------------------------------------------------------------------*/ 
    addItemListEvents: function(select_item) {
        var client_ui = this;
        select_item.hover(
            function() {$(this).addClass('selected').find('.controls').show();},
            function() {$(this).removeClass('selected').find('.controls').hide();}
        ); 
        select_item.find('.item').click(function() {
            $(this).parents('li').siblings('.open').removeClass('open');
            $(this).parents('li').addClass('open')
            client_ui['showContactsDisplay']($(this).text());
        }); 
        select_item.find('img').click(function() {            
            var item_type = client_ui.itemTypeSelected();
            client_ui['delete'+client_ui.capitalize(client_ui.singular(item_type))+'Dialog']($(this).parents('li').eq(0).text());
        }); 
    },
        
    /*-------------------------------------------------------------------------------*/ 
    block: function(msg) {
        $.blockUI({message: msg, 
                   overlayCSS: {backgroundColor: '#000', opacity: 0.75}, 
                   css: {border: 'none', padding: '15px', backgroundColor: '#000', '-moz-border-radius': '10px',
                         '-webkit-border-radius': '10px', opacity: .75, color: '#fff'}});  
    },
    
    /*-------------------------------------------------------------------------------*/ 
    unblock: function() {
        $.unblockUI();
    },
    
    /*-------------------------------------------------------------------------------
     * message displays 
     *-------------------------------------------------------------------------------*/ 
    messageInfo: function(msg) {
        var from = msg.from;
        return '<div class="message-info">'+
                   '<div class="from">'+from+'</div>'+
                   '<div class="date">'+msg.createdAtAsString()+'</div>'+
                '</div>';
    },

    /*-------------------------------------------------------------------------------*/    
    buildChatTextMessage: function(msg) {
        return '<li><div class="chat-text-message">'+
                   this.messageInfo(msg)+
                   '<div class="text">'+msg.text+'</div>'+
               '</div></li>';
    },

    /*-------------------------------------------------------------------------------
     * x data displays 
     *-------------------------------------------------------------------------------*/ 
    buildCommandRequestCommandMessage: function(msg) {
        return '<li><div class="command-request-message">'+
                   this.messageInfo(msg)+
                   '<div class="header">command request</div>'+
                   '<div class="node">'+msg.text+'</div>'+
               '</div></li>';
    },
    
    /*-------------------------------------------------------------------------------*/    
    buildXCommandMessage: function(msg) {
        return '<li><div class="x-message">'+
                   this.messageInfo(msg)+
                   '<div class="node">'+msg.node+'</div>'+
                   this.buildXDataBody(msg.text)+
               '</div></li>';
    },
    
    /*-------------------------------------------------------------------------------*/    
    buildXDataBody: function(data) {
        var fields    = $(data).find('field'),
            items     = $(data).find('item'),
            data_type = 'Scalar';
        if (items.length == 0) {
            var vals = fields.eq(0).find('value');
            if (fields.length > 1) {
                if (vals.length > 1) {
                    data_type= 'HashArray';
                } else {
                    data_type = 'Hash';
                }
            } else {
                if (vals.length > 1) {
                    data_type= 'ScalarArray';
                }
            }           
        } else {
            fields = $(items).find('field');
            vals = $(fields).eq(0).find('value');
            if (vals.length > 1) {
                data_type = 'ArrayHashArray';
            } else {
                data_type = 'ArrayHash';
            }
        }
        return this['buildXData'+data_type](data);
    },

    /*-------------------------------------------------------------------------------*/ 
    buildXDataScalar: function(data) {
        return '<div class="scalar">'+$(data).find('value').eq(0).text()+'</div>';
    },

    /*-------------------------------------------------------------------------------*/ 
    buildXDataScalarArray: function(data) {
        var vals = $.map($(data).find('value'), function(v,i) {
                       return $(v).text();
                   }).join(', ');
        return '<div class="scalar-array">'+vals+'</div>';
    },

    /*-------------------------------------------------------------------------------*/ 
    buildXDataHash: function(data) {
        var tab = '<table class="hash">'+ 
                      $.map($(data).find('field'), function(f,i) {
                          var row = '<tr>'+
                                        '<td class="attr">'+$(f).attr('var')+'</td>'+
                                        '<td class="val">'+$(f).find('value').eq(0).text()+'</td>'+
                                    '</tr>';
                          return row;
                      }).join('')+
                  '</table>';
        return tab;
    },

    /*-------------------------------------------------------------------------------*/ 
    buildXDataArrayHash: function(data) {
        var attrs = $.map($(data).find('reported').find('field'), function(a,i) {
                        return $(a).attr('var');
                    }),
            vals  = $.map($(data).find('item'), function(t,i) {
                        var hsh = {};
                        $(t).find('field').each(function() {
                            var attr = $(this).attr('var');
                            var val = $(this).find('value').eq(0).text();
                            hsh[attr] = val;
                        });
                        return hsh;         
                    });
            
        var tab = '<table class="array-hash">'+ 
                      $.map(attrs, function(a,i) {
                          return '<th>'+a+'</th>';
                      }).join('')+
                      $.map(vals, function(h,i) {
                          var row = '<tr>';
                          $.each(attrs, function(i,a) {
                              row += '<td>'+h[a]+'</td>';
                          });
                          row += '</tr>';
                          return row;
                      }).join('')+
                  '</table>';
        return tab;
    },

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