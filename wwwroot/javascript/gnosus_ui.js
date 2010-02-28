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
    this.history('contacts');
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
    camelize: function(str, del) {
        var delmiter = del || '_',
            client_ui = this;
        return $.map(str.split(delmiter), function(s,i) {
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
         this.showItemsToolbar(item_type);
         $(this.client_items_content).empty();
         this.itemsUnbind();
         this['show'+this.capitalize(item_type)+'Items']();
     },

     /*-------------------------------------------------------------------------------*/    
     showItemsToolbar: function(item_type) {
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
             client_ui.history(item_type);
         });
         var type_choices = this.item_type_choices;
         $(this.client_item_type_selected).click(function() {
             var next_item = type_choices[$(this).text()];
             $(this).text(next_item);
             client_ui.showItems(next_item);
             client_ui.history(next_item);
         });
     }, 

     /*-------------------------------------------------------------------------------*/  
     showContactsItems: function() {  
         var client_ui = this,
             build_list = function() {
                 client_ui.buildListItems(Gnosus.findAllContacts(), 'contact', function(i){return i.name;}, function(i){return i.show();});
             }
        if (Gnosus.findAllContacts().length > 0) {     
            build_list();
        }
         /*---- roster messages ----*/
         this.items_handlers['session_init_result'] = function (ev, roster) {
             build_list();
             this.unblock();             
         }
         $(document).bind('session_init_result', this.items_handlers['session_init_result'].bind(this));

         /****/
         this.items_handlers['session_init_error'] = function (ev, roster) {
             this.unblock();             
             this.errorDialog('session initialization failed');
         }
         $(document).bind('session_init_error', this.items_handlers['session_init_error'].bind(this));
         
         /**** received roster message ****/
         this.items_handlers['roster_item_add'] = function (ev, contact) {
             var items = this.buildItemListItem(contact.name, 'contact', contact.show());
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
         this.items_handlers['roster_item_update'] = function (ev, contact) {
         }
         $(document).bind('roster_item_update', this.items_handlers['roster_item_update'].bind(this));
         
         /**** roster request response ****/
         this.items_handlers['roster_item_add_result'] = function (ev, jid) {
             this.unblock();
         }
         $(document).bind('roster_item_add_result', this.items_handlers['roster_item_add_result'].bind(this));
         
         /****/
         this.items_handlers['roster_item_add_error'] = function (ev, jid) {
             this.unblock();
             this.errorDialog('failed to add <strong>'+jid+'</strong>');
         }
         $(document).bind('roster_item_add_error', this.items_handlers['roster_item_add_error'].bind(this));
         
         /****/
         this.items_handlers['roster_item_remove_result'] = function (ev, jid) {
             this.unblock();
         }
         $(document).bind('roster_item_remove_result', this.items_handlers['roster_item_remove_result'].bind(this));
         
         /****/
         this.items_handlers['roster_item_remove_error'] = function (ev, jid) {
             this.unblock();
             this.errorDialog('failed to remove <strong>'+jid+'</strong>');
         }
         $(document).bind('roster_item_remove_error', this.items_handlers['roster_item_remove_error'].bind(this));
         
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
     showResourcesItems: function() { 
     }, 

     /*-------------------------------------------------------------------------------*/  
     showSubscriptionsItems: function() { 
         this.buildListItems(Gnosus.findAllSubscriptions(), 'subscription', function(i){return GnosusXmpp.subNodeFromNode(i.node);})
         this.items_handlers['subcription_result'] = function (ev, iq) {
             this.unblock();
         }
         $(document).bind('subcription_result', this.items_handlers['subcription_result'].bind(this));
         this.items_handlers['subscription_error'] = function (ev, service, node) {
             this.unblock();
             this.errorDialog('failed to subscribe to node <strong>'+node+'</strong> on service <strong>'+service+'</service>');
         }
         $(document).bind('subscription_error', this.items_handlers['subscription_error'].bind(this));
     }, 

     /*-------------------------------------------------------------------------------*/  
     showPublicationsItems: function() { 
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

   /*-------------------------------------------------------------------------------  
    * history
    *-------------------------------------------------------------------------------*/    
    history: function(item_type) {
        this.displayUnbind();
        $(this.client_display_content).empty();
        $(this.client_display_toolbar).empty();
        this['history'+this.camelize(item_type)]();
    },               

    /*-------------------------------------------------------------------------------*/    
    historyContacts: function() {
        this.buildMessageContentList('no-input', Gnosus.findAllMessages());
        this.display_handlers['chat'] = function (ev, msg) {
            $(this.client_display_list).prepend(this.buildChatTextMessage(msg));
        }
        $(document).bind('chat', this.display_handlers['chat'].bind(this));
        this.display_handlers['headline'] = function (ev, msgs) {
            var client_ui = this;
            $.each(msgs, function() {
                $(client_ui.client_display_list).prepend(client_ui['build'+client_ui.camelize(this.type)+client_ui.camelize(this.content_type)+'Message'](this));
            });
        }
        $(document).bind('headline', this.display_handlers['headline'].bind(this));
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
                         var contact = Gnosus.findAccountByName($(this.item_dialog+' p').text());
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
                         '<div class="validate-jid">'+  
                            '<label for="jid">jid</label><input type="text" name="jid" class="required"/></br>'+
                         '</div>'+
                     '</div>'; 
        $(this.item_dialog).remove();            
        $(this.client).append(dialog); 
        $(this.item_dialog).dialog({modal:true, resizable:false,
            buttons:{'cancel':this.cancelItemDialog.bind(this), 
                     'send':function() {
                                if (!this.dialogButtonIsDisabled() && this.validateRequiredFields('jid is required')) {
                                    var jid = $(this.item_dialog+' input').val();
                                    this.cancelItemDialog(); 
                                    this.block('adding contact')
                                    GnosusXmpp.addContact(jid, null, []); 
                                }
                            }.bind(this)},
            dialogClass:'add-contact-dialog', width:380}); 
        this.addJidValidation('send');                               
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
     * contacts display
     *-------------------------------------------------------------------------------*/    
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
             contact = Gnosus.findAccountByName(contact_name),
             send_message = '<div class ="client-display-input">'+
                                '<textarea class="init">'+enter_msg+'</textarea>'+
                            '</div>';
         $(this.client_display_content).append(send_message);
         this.buildMessageContentList('input', Gnosus.findMessagesByJidAndType(contact.jid, 'chat'));
         var client_ui = this,
             textarea = $(this.client_display_input+' textarea'),
             orig_textarea_height = textarea.height();
         textarea.autoResize();
         textarea.keyup(function(evt) {
             var input = evt.keyCode;
             if (input == '13') {
                 var contact_name = client_ui.contactOpen(),
                     contact = Gnosus.findAccountByName(contact_name),
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
                 contact = Gnosus.findAccountByName(contact_name);
             if (msg.from.match(new RegExp(contact.jid, 'g'))) {
                 $(this.client_display_list).prepend(this.buildChatTextMessage(msg));
             }
         }
         $(document).bind('chat', this.display_handlers['chat'].bind(this));
     },               

     /*-------------------------------------------------------------------------------*/    
     showContactsCommandsDisplay: function(contact_name) {
          var toolbar = '<div class="add"></div>',
              contact = Gnosus.findAccountByName(contact_name);
         $(this.client_display_content_control).append(toolbar);
         contact.deleteCommands();
         this.block('retrieving command list');
         $.each(contact.resources, function(j,r) {
             GnosusXmpp.getCommandList(r.jid);
         });
         $(this.client_display_content_control+' .add').click(function() {
             this.contactCommandsDialog(contact);
         }.bind(this));
         this.display_handlers['command_list_result'] = function (ev, jid) {
             if (Gnosus.areCommandsAvailable(contact.jid)) {
                 this.unblock();
                 this.buildMessageContentList('no-input', Gnosus.findMessagesByJidAndContentType(contact.jid, 'command'));
             }
         }
         $(document).bind('command_list_result', this.display_handlers['command_list_result'].bind(this));
         this.display_handlers['command_list_error'] = function (ev, jid) {
             this.unblock();
             this.errorDialog('failed to retrieve command list');
         }
         $(document).bind('command_list_error', this.display_handlers['command_list_error'].bind(this));
         this.display_handlers['command_form'] = function (ev, iq) {
             this.unblock();
             this.formDialog(iq);
         }
         $(document).bind('command_form', this.display_handlers['command_form'].bind(this));
         this.display_handlers['command_cancel'] = function (ev, msg) {
             $(this.client_display_list).prepend(this.buildTextCommandMessage(msg));
             this.unblock();
         }
         $(document).bind('command_cancel', this.display_handlers['command_cancel'].bind(this));
         this.display_handlers['command_result'] = function (ev, msg) {
             $(this.client_display_list).prepend(this.buildXCommandMessage(msg));
             this.unblock();
         }
         $(document).bind('command_result', this.display_handlers['command_result'].bind(this));
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
             if (!cats[cat]) {cats[cat]= [];}
             cats[cat].push(cmd);
         });
         $.each(cats, function(cat, cmd) {
             dialog += '<h3><a href="#" class="command-category">'+cat+'</a></h3>'+
                       '<div>';
             $.each(cmd, function() {dialog += '<div class="command">'+this+'</div>';});
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
                 $(client_ui.client_display_list).prepend(client_ui.buildTextCommandMessage(GnosusXmpp.setCommand({to:jid, node:node})));
             });
             client_ui.cancelItemDialog();
             client_ui.block('command request pending');
         });
     },

     /*-------------------------------------------------------------------------------*/    
     formDialog: function(form) {
         var title   = $(form).find('title').text() || 'command form',
             dialog  = '<div id="'+this.toId(this.item_dialog)+'" title="'+title+'">',
             inst    = $(form).find('instructions').text();
             client_ui = this;
        if (inst) {
            dialog += '<div class="instructions">'+inst+'</div>';
        }     
        $(form).find('field').each(function() {
            var meth = 'build'+client_ui.camelize($(this).attr('type'), '-')+'Control';
            if (client_ui[meth]) {dialog += client_ui[meth](this);}
        });
        dialog += '</div>';
        $(this.item_dialog).remove();            
        $(this.client).append(dialog); 
        $(this.item_dialog).dialog({modal:true, resizable:false, 
            buttons:{
                'cancel':function() {
                             $(client_ui.client_display_list).prepend(client_ui.buildTextCommandMessage(GnosusXmpp.setCommandCancel(form)));
                              this.cancelItemDialog();            
                              this.block('canceling');
                          }.bind(this),
                'send':function() {
                           if (!client_ui.dialogButtonIsDisabled()) {
                               var jid  = $(form).attr('from'),
                                   node = $(form).find('command').eq(0).attr('node');
                               $(client_ui.client_display_list).prepend(client_ui.buildXCommandMessage(GnosusXmpp.setCommand(
                                   {to:jid, node:node, action:'submit', payload:GnosusXmpp.buildFormXDataPayload(client_ui.readForm())})));
                               this.cancelItemDialog();            
                               client_ui.block('command request pending');
                           }
                        }.bind(this)},
            dialogClass:'x-form', width:400, height:400
        });
        this.addJidValidation();        
     },

     /*-------------------------------------------------------------------------------*/    
     showContactsResourcesDisplay: function(contact_name) {
     },               

     /*-------------------------------------------------------------------------------*/    
     showContactsPublicationsDisplay: function(contact_name) {
        var contact   = Gnosus.findAccountByName(contact_name),
            client_ui = this;
        this.block('retrieving publications');
        GnosusXmpp.getPubSubServiceDisco(contact.jid, Strophe.getDomainFromJid(contact.jid), 
            function() {
                this.unblock();
                this.buildPubNodeContentList('contact', Gnosus.findPubSubNodesByJid(contact.jid), function(node) {
                    var pub_status = 'not-subscribed';
                    var sub = Gnosus.findSubscriptionsByNodeAndSubscription(node, 'subscribed');
                    if (sub.length > 0) {
                        pub_status = 'subscribed';
                    }
                    return pub_status;
                });
                $(this.client_display_content).find('.publication-node .status').click(function() {  
                    var node = $(this).siblings('.full-node').text();
                    var service = Gnosus.findPubSubServiceByJid(contact.jid);
                    if ($(this).hasClass('not-subscribed')) {
                        GnosusXmpp.setSubscribe(service.jid, node);
                        client_ui.block('subscribing');
                    } else {
                        $.each(Gnosus.findSubscriptionsByNodeAndSubscription(node, 'subscribed'), function() {
                            GnosusXmpp.setUnsubscribe(service.jid, this.node, this.subid)
                        });
                        client_ui.block('unsubscribing');
                    }   
                });    
            }.bind(this),
            function() {
                this.unblock();
            }.bind(this)
        );
        this.display_handlers['subscribe_result'] = function (ev, subscription) {
            this.unblock();
            var item = Gnosus.findServiceItemByJidAndNode(contact.jid, subscription.node)
            $(this.client_display_content).find('.node:contains('+item.name+')').siblings('.status')
                .removeClass('not-subscribed').addClass('subscribed')
        }
        $(document).bind('subscribe_result', this.display_handlers['subscribe_result'].bind(this));
        this.display_handlers['subscribe_error'] = function (ev, service, node) {
            this.unblock();
            this.errorDialog('error subscribing to <strong>'+node+'</strong> on service <strong>'+service+'</strong>');
        }
        $(document).bind('subscribe_error', this.display_handlers['subscribe_error'].bind(this));
        this.display_handlers['unsubscribe_result'] = function (ev, subscription) {
            this.unblock();
            var item = Gnosus.findServiceItemByJidAndNode(contact.jid, subscription.node)
            $(this.client_display_content).find('.node:contains('+item.name+')').siblings('.status')
                .removeClass('subscribed').addClass('not-subscribed')
        }
        $(document).bind('unsubscribe_result', this.display_handlers['unsubscribe_result'].bind(this));
        this.display_handlers['unsubscribe_error'] = function (ev, service, node) {
            this.unblock();
            this.errorDialog('error unsubscribing from <strong>'+node+'</strong> on service <strong>'+service+'</strong>');
        }
        $(document).bind('unsubscribe_error', this.display_handlers['unsubscribe_error'].bind(this));
        this.display_handlers['disco_info_error'] = function (ev, jid, node) {
            this.unblock();
            var msg = 'disco info failed';
            if (node) {msg += ' for node <strong>' + node+'</strong>';}
            this.errorDialog(msg);
        }
        $(document).bind('disco_info_error', this.display_handlers['disco_info_error'].bind(this));
        this.display_handlers['disco_items_error'] = function (ev, jid, node) {
            this.unblock();
            var msg = 'disco items failed';
            if (node) {msg += ' for node <strong>' + node+'</strong>';}
            this.errorDialog(msg);
        }
        $(document).bind('disco_items_error', this.display_handlers['disco_items_error'].bind(this));
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
     * display utils 
     *-------------------------------------------------------------------------------*/  
     buildMessageContentList: function(list_type, content_list) {
        var msgs = ['<ul class="client-display-list '+list_type+'">'];
        var client_ui = this;
        $.each(content_list, function () {
            msgs.push(client_ui['build'+client_ui.camelize(this.type)+client_ui.camelize(this.content_type)+'Message'](this));
        });
        msgs.push('</ul>')
        $(this.client_display_content).append(msgs.join(''));
    },   
      
    /*-------------------------------------------------------------------------------*/ 
     buildPubNodeContentList: function(list_type, content_list, status) {
        var msgs = ['<ul class="client-display-list publication-nodes">'];
        $.each(content_list, function () {
            msgs.push('<li><div class="publication-node">'+
                           '<div class="status '+status(this.node)+'"></div>'+ 
                           '<div class="full-node">'+this.node+'</div>'+          
                           '<div class="node">'+this.name+'</div>'+
                       '</div></li>'
            );
        });
        msgs.push('</ul>')
        $(this.client_display_content).append(msgs.join(''));
    },   
              
    /*-------------------------------------------------------------------------------*/ 
    buildListItems: function(items, item_type, item_name, item_status) {
        var list_items = '<ul>';
        var client_ui = this;
        $.each(items, function () {
            var status = '';
            var iname = item_name(this);
            if (item_status) {status = item_status(this)}
            list_items += client_ui.buildItemListItem(item_name(this), item_type, status);
        });
        list_items += '</ul>';
        $(this.client_items_content).append(list_items);
        this.addItemListEvents($(this.client_items_content+' ul li'));
    },

    /*-------------------------------------------------------------------------------*/ 
    buildItemListItem: function (item, item_type, item_status) { 
        var status = item_status || '',  
            item = '<li>' +
                       '<div class="'+item_type+' item '+status+'">'+ 
                           item+
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
     * validation
     *-------------------------------------------------------------------------------*/ 
    addJidValidation: function() {
        var client_ui = this;
        $(this.item_dialog).find('.validate-jid input').blur(function() {
            var jid = $(this).val();
            if (jid) {
                if (!jid.match(/\@/)) { 
                    $(this).after('<div class="dialog-error">jid is invalid</div>');
                    client_ui.disableDialogButton('send');                   
                }
            } 
        });
        this.addRemoveErrorMessage('.validate-jid input')
    },

    /*-------------------------------------------------------------------------------*/ 
    disableDialogButton: function(button) {
        $('.ui-dialog-buttonpane').find('button:contains('+button+')').addClass('ui-state-disabled');        
    },

    /*-------------------------------------------------------------------------------*/ 
    dialogButtonIsDisabled: function() {
        var is_disabled = false;
        $('.ui-dialog-buttonpane').find('button.ui-state-disabled').each(function(){
            is_disabled = true;
        });
        return is_disabled;        
    },

    /*-------------------------------------------------------------------------------*/ 
    enableDialogButton: function(button) {
        $('.ui-dialog-buttonpane').find('button:contains('+button+')').removeClass('ui-state-disabled');        
    },

    /*-------------------------------------------------------------------------------*/ 
    addRemoveErrorMessage: function(sel) {
        var client_ui = this;
        $(this.item_dialog).find(sel).focus(function() {
            $(this).siblings('.dialog-error').remove();
            if (client_ui.noDialogErrors()) {
                client_ui.enableDialogButton('send');
            }
        });
    },

    /*-------------------------------------------------------------------------------*/ 
    noDialogErrors: function () {
        var no_errors = true;
        $(this.item_dialog).find('.dialog-error').each(function(){
            no_errors = false;
        });
        return no_errors;
    },

    /*-------------------------------------------------------------------------------*/ 
    validateRequiredFields: function(msg) {
        var is_valid = true;
        if (this.noDialogErrors()) {
            $(this.item_dialog).find('input.required').each(function() {
                if (!$(this).val()) {
                    is_valid = false;
                    $(this).after('<div class="dialog-error">'+msg+'</div>')                   
                } 
            });
        }
        return is_valid;
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
        return '<li><div class="text-message">'+
                   this.messageInfo(msg)+
                   '<div class="text">'+msg.text+'</div>'+
               '</div></li>';
    },

    /*-------------------------------------------------------------------------------*/    
    buildHeadlineXMessage: function(msg) {
        return '<li><div class="x-message">'+
                   this.messageInfo(msg)+
                   '<div class="node">'+msg.node+'</div>'+
                   this.buildXDataBody(msg.text)+
               '</div></li>';
    },

    /*-------------------------------------------------------------------------------*/    
    buildHeadlineEntryMessage: function(msg) {
        return '<li><div class="text-message">'+
                   this.messageInfo(msg)+
                   '<div class="node">'+msg.node+'</div>'+
                   '<div class="entry">'+msg.text+'</div>'+
               '</div></li>';
    },

    /*-------------------------------------------------------------------------------
     * x data displays 
     *-------------------------------------------------------------------------------*/ 
    buildTextCommandMessage: function(msg) {
        return '<li><div class="command-request-message">'+
                   this.messageInfo(msg)+
                   '<div class="header">'+msg.node+'</div>'+
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
        var data_type = 'Scalar';
        if ($(data).find('item').length == 0) {
            var l = $(data).find('field').length;
            if ($(data).find('field').length > 1) {
                data_type = 'Hash';
            }           
        } else {
            data_type = 'ArrayHash';
        }
        return this['buildXData'+data_type](data);
    },

    /*-------------------------------------------------------------------------------*/ 
    buildXDataScalar: function(data) {
        return '<div class="scalar">'+this.getXDataValues(data)+'</div>';
    },

    /*-------------------------------------------------------------------------------*/ 
    getXDataValues: function(field) {
        return $.map($(field).find('value'), function(v,i) {
                   return $(v).text();
               }).join(', ');
    },
    
    /*-------------------------------------------------------------------------------*/ 
    buildXDataHash: function(data) {
        var client_ui = this;
        return '<table class="hash">'+ 
                   $.map($(data).find('field'), function(f,i) {
                          return '<tr>'+
                                     '<td class="attr">'+$(f).attr('var')+'</td>'+
                                     '<td class="val">'+client_ui.getXDataValues(f)+'</td>'+
                                 '</tr>';
                   }).join('')+
               '</table>';
    },

    /*-------------------------------------------------------------------------------*/ 
    buildXDataArrayHash: function(data) {
        var client_ui = this,
            attrs     = $.map($(data).find('reported').find('field'), function(a,i) {
                            return $(a).attr('var');
                        }),
            vals      = $.map($(data).find('item'), function(t,i) {
                            var hsh = {};
                            $(t).find('field').each(function() {
                                hsh[$(this).attr('var')] = client_ui.getXDataValues(this);
                            });
                            return hsh;         
                        });           
        return '<table class="array-hash">'+ 
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
    },
    /*-------------------------------------------------------------------------------
     * x data form controls 
     *-------------------------------------------------------------------------------*/ 
    buildFixedControl: function(field) {
        var label   = $(field).find('value').eq(0).text()
        return '<div class="fixed">'+label+'</div>';
    },
 
    /*-------------------------------------------------------------------------------*/ 
    buildBooleanControl: function(field) {
        var name    = $(field).attr('var'),
            label   = $(field).attr('label'),
            control = '<div class="boolean">';
        control += '<input type="checkbox" name="'+name+'"/>'; 
        if (label) {control += '<label>'+label+'</label>';}  
        control += '</br></div>'  
        return control;              
    },
    
    /*-------------------------------------------------------------------------------*/ 
    buildListSingleControl: function(field) {
        var name    = $(field).attr('var'),
            label   = $(field).attr('label'),
            control = '<div class="list-single">';
        if (label) {control += '<label>'+label+'</label>';}    
        control += '<select name="'+name+'">'
        $(field).find('option').each(function() {
           control += '<option value="'+$(this).text()+'">'+$(this).attr('label')+'</option>'; 
        });
        control += '</select></br></div>'; 
        return control;              
    },

    /*-------------------------------------------------------------------------------*/ 
    buildTextSingleControl: function(field) {
        return this.buildInputControl(field, 'text', 'text-single');
    },
    
    /*-------------------------------------------------------------------------------*/ 
    buildTextPrivateControl: function(field) {
        return this.buildInputControl(field, 'password', 'text-private');
    },

    /*-------------------------------------------------------------------------------*/ 
    buildJidSingleControl: function(field) {
        return this.buildInputControl(field, 'text', 'jid-single validate-jid');
    },

    /*-------------------------------------------------------------------------------*/ 
    buildInputControl: function(field, type, klass) {
        var name    = $(field).attr('var'),
            label   = $(field).attr('label'),
            control = '<div class="input-single '+klass+'">';
        if (label) {control += '<label>'+label+'</label>';}    
        control += '<input type="'+type+'" name="'+name+'"/></br></div>'; 
        return control;              
    },
    
    /*-------------------------------------------------------------------------------*/ 
    buildTextMultiControl: function(field) {
        var name    = $(field).attr('var'),
            label   = $(field).attr('label'),
            control = '<div class="text-multi">';
        if (label) {control += '<label>'+label+'</label>';}    
        control += '<textarea name="'+name+'"/></br></div>'; 
        return control;              
    },

    /*-------------------------------------------------------------------------------*/ 
    readForm: function() {
        var fields    = [];
        $(this.item_dialog).find('.jid-single input').each(function() {
            fields.push({type:'jid-single', 'var':$(this).attr('name'), value:$(this).val()})
        });
        $(this.item_dialog).find('.text-single input').each(function() {
            fields.push({type:'text-single', 'var':$(this).attr('name'), value:$(this).val()})
        });
        $(this.item_dialog).find('.text-private input').each(function() {
            fields.push({type:'text-private', 'var':$(this).attr('name'), value:$(this).val()})
        });
        $(this.item_dialog).find('.list-single select').each(function() {
            fields.push({type:'list-single', 'var':$(this).attr('name'), value:$(this).val()})
        });
        $(this.item_dialog).find('.text-multi textarea').each(function() {
            fields.push({type:'text-multi', 'var':$(this).attr('name'), value:$(this).val()})
        });
        $(this.item_dialog).find('.boolean input').each(function() {
            var val = '0';
            if ($(this).attr('checked')){val = '1'}
            fields.push({type:'boolean', 'var':$(this).attr('name'), value:val})
        });
        return fields;
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