/**********************************************************************************
ui displays
**********************************************************************************/
function GnosusUi(num) {
    this.items_handlers = {},
    this.display_handlers = {},
    this.map = null;
    this.cloudmade = new CM.Tiles.CloudMade.Web({key: 'BC9A493B41014CAABB98F0471D759707', styleId:22125});
    this.item_type_choices = {publications:['contacts', true], contacts:['resources', false], 
        resources:['subscriptions', true], subscriptions:['publications', true]};
    this.client                         = '#client-'+num;
    this.client_items                   = this.client+' .client-items';
    this.client_items_content           = this.client+' .client-items-content';
    this.client_items_toolbar           = this.client+' .client-items-toolbar';
    this.client_item_type_selected      = this.client+' .client-item-type-selected';
    this.client_items_add               = this.client+' .client-items-add';
    this.client_items_history           = this.client+' .client-items-history';
    this.client_display                 = this.client+' .client-display';
    this.client_display_content         = this.client+' .client-display-content';
    this.client_display_list            = this.client+' .client-display-list';
    this.client_display_toolbar         = this.client+' .client-display-toolbar';
    this.client_display_toolbar_jid     = this.client+' .client-display-toolbar .jid';
    this.client_display_toolbar_control = this.client+' .client-display-toolbar .control';
    this.client_display_modes           = this.client+' .client-display-modes';
    this.client_display_input           = this.client+' .client-display-input';
    this.client_item_resource_contact   = this.client+' .client-item-resource-contact';
    this.client_contact_resources       = this.client+' .contact-resources';
    this.client_display_map             = this.client+' .client-display-map';
    this.item_dialog                    = '#item-dialog-'+num;
    this.geoloc_map                     = '#geoloc-map-'+num;
    this.client_height                  = 500;
    this.client_width                   = 1000;
    this.toolbar_offset                 = 42;
    this.setDisplaySize();
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
        var client_ui= this;
        $.each(this.items_handlers, function(e, h) {
            $(document).unbind(e);
            delete(client_ui.items_handlers[e]);
        });
    },
    
    /*-------------------------------------------------------------------------------*/    
    displayUnbind: function() {
        var client_ui= this;
        $.each(this.display_handlers, function(e,h) {
            $(document).unbind(e);
            delete(client_ui.display_handlers[e]);
        });
    },

    /*-------------------------------------------------------------------------------*/    
    setDisplaySize: function() {
        var win_height    = $(window).height(),
            win_width     = $(window).width(),
            nav_offset    = 87,
            footer_offset = 41,
            page_width    = 0.9*win_width,
            client_border = 4,
            item_width    = 0.25*page_width,
            display_width = page_width - item_width - client_border - 2;
        this.toolbar_offset = $(this.client_items_toolbar).height() + parseInt($(this.client_items_toolbar).css('paddingTop').substr(0,2)) + 2,
        this.client_height = win_height-client_border-nav_offset-footer_offset;
        this.client_width = page_width; 
        $(this.client).height(this.client_height);
        $(this.client_items_content).height(this.client_height-this.toolbar_offset);
        $('#page-wrapper').width(page_width)
        $('#footer-wrapper').width(page_width);
        $('#navigation').width(page_width);
        $(this.client_items).width(item_width);
        $(this.client_display).width(display_width);
        $(this.client_display).css('left', item_width+2+'px');
    },

    /*-------------------------------------------------------------------------------*/    
    getClientDisplayListHeight: function(type) {
        var height = this.client_height-this.toolbar_offset;
        if (type == 'input') {
            var offset  = $(this.client_display_input+' textarea').height(),
                border  = 4,
                margin  = 2*parseInt($(this.client_display_input+' textarea').css('marginTop').substr(0,2));
                padding = parseInt($(this.client_display_input+' textarea').css('paddingTop').substr(0,2));
            height = height-offset-margin-border-padding;
        }
        return height;
    },

    /*-------------------------------------------------------------------------------*/ 
    bindItemsHandler: function(trigger, handler) {
        this.items_handlers[trigger] = handler;
        $(document).bind(trigger, this.items_handlers[trigger].bind(this));
    },

    /*-------------------------------------------------------------------------------*/ 
    bindDisplayHandler: function(trigger, handler) {
        this.display_handlers[trigger] = handler;
        $(document).bind(trigger, this.display_handlers[trigger].bind(this));
    },

    /*-------------------------------------------------------------------------------*/    
    capitalize: function(str) {return str.charAt(0).toUpperCase()+str.substr(1);},

    /*-------------------------------------------------------------------------------*/    
    camelize: function(str, del) {
        var client_ui = this;
        del = del || '_';
        return $.map(str.split(del), function(s,i) {
                   return client_ui.capitalize(s);
               }).join('');
    },

    /*-------------------------------------------------------------------------------*/    
    singular: function(str) {return str.replace(/s$/,'');},

    /*-------------------------------------------------------------------------------*/    
    toId: function(str) {return str.replace('#','');},

    /*-------------------------------------------------------------------------------*/    
    clientDisplayMode: function () {
        return $(this.client_display_modes+' li.selected').text();
    },

    /*-------------------------------------------------------------------------------*/    
    clientResourceContact: function () {
        return $(this.client_item_resource_contact).text();            
    },

    /*-------------------------------------------------------------------------------*/    
    addClientResourceContact: function (contact_jid) {
        $(this.client_item_resource_contact).remove();
        $(this.client).append(
            '<div class="client-item-resource-contact data">'+
                contact_jid+
            '</div>'
        );
    },

    /*-------------------------------------------------------------------------------*/    
    itemTypeSelected: function () {
        return $(this.client_item_type_selected).text();
    },
    
    /*-------------------------------------------------------------------------------  
     * items
     *-------------------------------------------------------------------------------*/    
     showItems: function (item_type, add_item) {
         this.showItemsToolbar(item_type, add_item);
         $(this.client_items_content).empty();
         this.itemsUnbind();
         this['show'+this.capitalize(item_type)+'Items']();
     },

     /*-------------------------------------------------------------------------------*/    
     showItemsToolbar: function(item_type, add_item) {
         if (add_item == null){add_item = true;}
         $(this.client_items_toolbar).empty();
         var toolbar = '<div class="client-items-history"/>' +        
                       '<div class="client-item-type-selector">'+ 
                           '<div class="client-item-type-selected">' + item_type + '</div>' +
                       '</div>';
        if (add_item) {             
            toolbar += '<div class="client-items-add"/>';  
        }
         $(this.client_items_toolbar).append(toolbar);
         var client_ui = this;
         $(this.client_items_add).click(function() { 
             var item_type = client_ui.itemTypeSelected();
             client_ui['add'+client_ui.capitalize(client_ui.singular(item_type))+'Dialog']();
         });
         $(this.client_items_history).click(function() {            
             var item_type = client_ui.itemTypeSelected();
             client_ui.removeSelectedResource();
             client_ui.history(item_type);
         });
         var type_choices = this.item_type_choices;
         $(this.client_item_type_selected).click(function() {
             var next_item = type_choices[$(this).text()];
             $(this).text(next_item[0]);
             client_ui.showItems(next_item[0], next_item[1]);
             client_ui.history(next_item[0]);
         });
     }, 

     /*-------------------------------------------------------------------------------*/  
     showContactsItems: function() {  
         var contact_name = function(i) {return '<div class="name">'+i.name+'</div>';}
         var client_ui = this,
             build_list = function() {
                 client_ui.buildListItems(Gnosus.findAllContacts(), 'contact', contact_name, function(i){return i.show();});
             }
         if (Gnosus.findAllContacts().length > 0) {     
             build_list();
         }
         /*---- session messages ----*/
         this.bindItemsHandler('session_init_result', function (ev, roster) {
             build_list();
             this.unblock();             
         });
         this.bindItemsHandler('session_init_error', function (ev, roster) {
             this.unblock();             
             this.errorDialog('session initialization failed');
         });
     
         /**** roster request response ****/
         this.bindItemsHandler('roster_item_add_result', function (ev, jid) {
             this.unblock();
         });
         this.bindItemsHandler('roster_item_add_error', function (ev, jid) {
             this.unblock();
             this.errorDialog('failed to add <strong>'+jid+'</strong>');
         });
         this.bindItemsHandler('roster_item_remove_result', function (ev, jid) {
             this.unblock();
         });
         this.bindItemsHandler('roster_item_remove_error', function (ev, jid) {
             this.unblock();
             this.errorDialog('failed to remove <strong>'+jid+'</strong>');
         });
         /**** roster update messages****/
         this.bindItemsHandler('roster_item_add', function (ev, contact) {
             var item = this.buildItemListItems(contact_name(contact), 'contact', contact.show());
             $(this.client_items_content+' ul').append(item);
             this.addItemListEvents($(this.client_items_content+' ul li:last'));             
         });
         this.bindItemsHandler('roster_item_remove', function (ev, contact) {
             client_ui.closeContactIfOpen(contact.name);
             $(this.client_items_content+' ul li').find('.name:contains('+contact.name+')').parent().remove();
         });
         this.bindItemsHandler('roster_item_update', function (ev, contact) {
         });     
         /*---- presence messages ----*/
         this.bindItemsHandler('presence_available', function (ev, acct, resource) {
             $(this.client_items_content+' ul li').find('.item:contains('+acct.name+')')
                .removeClass('online').removeClass('offline').addClass(acct.show());
             if (this.clientDisplayMode() == 'resources') {
                 var item = $(this.client_display_content).find('.contact-resource:contains('+Strophe.getResourceFromJid(resource.jid)+')');
                 if (item.length == 0) {
                     $(this.client_display_list).prepend(this.buildResourceContent(resource));
                     this.addResourceContentEvents(contact_name, acct.jid);
                 }
             }
         });
         this.bindItemsHandler('presence_unavailable', function (ev, acct, resource) {
             this.closeContactResourceIfOpen(resource.jid);
             $(this.client_items_content+' ul li').find('.item:contains('+acct.name+')')
                .removeClass('online').removeClass('offline').addClass(acct.show());
             if (acct.show() == 'offline') {client_ui.closeContactIfOpen(acct.name);}
             if (this.clientDisplayMode() == 'resources') {
                 $(this.client_display_content).find('.contact-resource:contains('+Strophe.getResourceFromJid(resource.jid)+')').parent().remove();
             }
         });
         this.bindItemsHandler('presence_unsubscribed', function (ev, acct) {
             $(this.client_items_content+' ul li').find('.item:contains('+acct.name+')')
                .removeClass('online').removeClass('offline').addClass(acct.show());
             client_ui.closeContactIfOpen(contact.name);
         });
         this.addContactRequestEvent();
     },

     /*-------------------------------------------------------------------------------*/  
     showResourcesItems: function() {          
         this.buildListItems(Gnosus.findAllResourcesByJid(Gnosus.account().jid), 'resource', 
            function(i){return Strophe.getResourceFromJid(i.jid);}, null, null, null, true);    
         this.addContactRequestEvent();
         this.bindItemsHandler('presence_available', function (ev, acct, resource) {
             if (resource && acct.jid == Gnosus.account_jid) {
                 var res = $(this.client_items_content+' ul li').find('.item:contains('+Strophe.getResourceFromJid(resource.jid)+')');
                 if (res.length == 0) {
                     var item = this.buildItemListItems(Strophe.getResourceFromJid(resource.jid), 'resource', null, true);
                     $(this.client_items_content+' ul').append(item);
                     this.addItemListEvents($(this.client_items_content+' ul li:last'));             
                }
            }
         });
         this.bindItemsHandler('presence_unavailable', function (ev, acct, resource) {
             if (acct.jid == Gnosus.account_jid) {
                 var res = Strophe.getResourceFromJid(resource.jid);
                 this.closeResourceIfOpen(resource.jid);
                 $(this.client_items_content+' ul li').find('.item:contains('+Strophe.getResourceFromJid(resource.jid)+')').parent().remove();
             }
         });
     }, 

     /*-------------------------------------------------------------------------------*/  
     showSubscriptionsItems: function() { 
         var sub_item = function(i) {
             return '<div class="jid">'+
                        GnosusXmpp.userPubsubRootToJid(i.node)+
                    '</div>'+
                    '<div class="node">'+
                        GnosusXmpp.subNodeFromNode(i.node)+
                    '</div>';      
         }
         var open_item_name = function(i) {
             var node = $(i).find('.node').text(),
                 jid  = $(i).find('.jid').text();
             return GnosusXmpp.userPubsubNode(jid, node);    
         }
         var delete_item_name = function(i) {
             var litem = $(i).parents('li').eq(0),
                 node  = litem.find('.node').text(),
                 jid   = litem.find('.jid').text();
             return GnosusXmpp.userPubsubNode(jid, node);    
         }
         this.addContactRequestEvent();
         this.buildListItems(Gnosus.findAllSubscriptions(), 'subscription', sub_item, null, open_item_name, delete_item_name);    
         this.bindItemsHandler('subscribe_result', function (ev, sub) {
             var item = this.buildItemListItems(sub_item(sub), 'subscription');
             this.unblock();
             $(this.client_items_content+' ul').append(item);
             this.addItemListEvents($(this.client_items_content+' ul li:last'), open_item_name, delete_item_name);             
         });
         this.bindItemsHandler('subscribe_error', function (ev, service, node) {
             this.unblock();
             this.errorDialog('failed to subscribe to <strong>'+GnosusXmpp.subNodeFromNode(node)+'</strong> on service <strong>'+service+'</strong>');
         });
         this.bindItemsHandler('unsubscribe_result', function (ev, sub) {
             this.unblock();
             var jid  = GnosusXmpp.userPubsubRootToJid(sub.node),
                 node = GnosusXmpp.subNodeFromNode(sub.node);
             this.closeSubscriptionIfOpen(sub.node);
             $(this.client_items_content+' ul li').find('.jid:contains('+jid+')').siblings('.node:contains('+node+')').eq(0).parent().remove();
         });
         this.bindItemsHandler('unsubscribe_error', function (ev, service, node) {
             this.unblock();
             this.errorDialog('failed to unsubscribe to <strong>'+GnosusXmpp.subNodeFromNode(node)+'</strong> on service <strong>'+service+'</strong>');
         });
     }, 

    /*-------------------------------------------------------------------------------*/  
    showPublicationsItems: function() { 
        this.addContactRequestEvent();
        this.buildListItems(Gnosus.findPubNodesByJid(Gnosus.account().jid), 'publication', function(i){return GnosusXmpp.subNodeFromNode(i.node);});    
        this.bindItemsHandler('create_pubsub_node_result', function (ev, pub) {
            var item = this.buildItemListItems(GnosusXmpp.subNodeFromNode(pub.node), 'publication');
            this.unblock();
            $(this.client_items_content+' ul').append(item);
            this.addItemListEvents($(this.client_items_content+' ul li:last'));             
        });
        this.bindItemsHandler('create_pubsub_node_error', function (ev, node) {
            this.unblock();
            this.errorDialog('failed create publication <strong>'+GnosusXmpp.subNodeFromNode(node)+'</strong>');
        });
        this.bindItemsHandler('delete_pubsub_node_result', function (ev, pub) {
            var node = GnosusXmpp.subNodeFromNode(pub.node);
            this.unblock();
            this.closePublicationIfOpen(node);
            $(this.client_items_content+' ul li').find('.item:contains('+GnosusXmpp.subNodeFromNode(node)+')').parent().remove();
        });
        this.bindItemsHandler('delete_pubsub_node_error', function (ev, node) {
            this.unblock();
            this.errorDialog('failed delete publication <strong>'+GnosusXmpp.subNodeFromNode(node)+'</strong>');
        });
    }, 

    /*-------------------------------------------------------------------------------  
    * open items
    *-------------------------------------------------------------------------------*/
    closeContactIfOpen: function(jid) {
        if ($(this.client_items_content+' ul li.open').find('.name').text() == jid) {
            this.unblock();
            this.history('contacts');
        }
    },    

    /*-------------------------------------------------------------------------------*/ 
    closeContactResourceIfOpen: function(jid) {
        if ($(this.client_items_content+' ul li.open').find('.resource').text() == Strophe.getResourceFromJid(jid)) {
            this.removeSelectedResource();
            this.unblock();
            this.history('contacts');
        }
    },    

    /*-------------------------------------------------------------------------------*/ 
    closePublicationIfOpen: function(pub) {
        if ($(this.client_items_content+' ul li.open .item').text() == pub) {
            this.history('publications');
        }
    },

    /*-------------------------------------------------------------------------------*/ 
    closeSubscriptionIfOpen: function(node) {
        var item     =  $(this.client_items_content+' ul li.open'),
            sub_node = GnosusXmpp.userPubsubNode(item.find('.jid').text(), item.find('.node').text());
        if (sub_node == node) {
            this.history('subscriptions');
        }
    },

    /*-------------------------------------------------------------------------------*/ 
    closeResourceIfOpen: function(jid) {
        if ($(this.client_items_content+' ul li.open .item').text() == Strophe.getResourceFromJid(jid)) {
            this.unblock();
            this.history('resources');
        }
    },

   /*-------------------------------------------------------------------------------  
    * history
    *-------------------------------------------------------------------------------*/    
    history: function(item_type) {
        this.displayUnbind();
        $(this.client_display_content).empty();
        $(this.client_display_toolbar).empty();
        $(this.client_items_content+' ul li').removeClass('open');
        this['history'+this.camelize(item_type)]();
    },               

    /*-------------------------------------------------------------------------------*/    
    historyContacts: function() {
        this.buildMessageContentList(Gnosus.findAllContactMessages(), 'no-input');
        this.bindDisplayHandler('chat', function (ev, msg) {
            $(this.client_display_list).prepend(this.buildChatTextMessage(msg));
        });
        this.bindDisplayHandler('headline', function (ev, msgs) {
            var client_ui = this;
            $.each(msgs, function() {
                client_ui.prependMessage(this);                
            });
        });
    },

    /*-------------------------------------------------------------------------------*/    
    historyResources: function() {
        this.buildMessageContentList(Gnosus.findAllAccountMessages(), 'no-input');
        this.bindDisplayHandler('chat', function (ev, msg) {
            $(this.client_display_list).prepend(this.buildChatTextMessage(msg));
        });
    },

    /*-------------------------------------------------------------------------------*/    
    historySubscriptions: function() {
        this.buildMessageContentList(Gnosus.findAllSubscribedMessages(), 'no-input');
        this.bindDisplayHandler('headline', function (ev, msgs) {
            var client_ui = this;
            $.each(msgs, function() {
                client_ui.prependMessage(this);
            });
        });
    },

    /*-------------------------------------------------------------------------------*/    
    historyPublications: function() {
        this.buildMessageContentList(Gnosus.findAllPublishedMessages(), 'no-input');
    },

    /*-------------------------------------------------------------------------------  
     * delete
     *-------------------------------------------------------------------------------*/    
    deleteContactDialog: function(item) {
        var client_ui = this,
            dialog = '<div id="'+this.toId(this.item_dialog)+'" title="delete contact?">'+ 
                        '<p>'+item+'</p>'+
                     '</div>'; 
        this.deleteItemDialog(dialog, function() {
            client_ui.block('deleting contact');
            var contact = Gnosus.findAccountByName(item);
            GnosusXmpp.removeContact(contact.jid);
        });
    },
    
    /*-------------------------------------------------------------------------------*/    
    deleteSubscriptionDialog: function(item) {
        var client_ui = this,
            dialog = '<div id="'+this.toId(this.item_dialog)+'" class="subscription-delete" title="delete subscription?">'+             
                        '<p>'+GnosusXmpp.userPubsubRootToJid(item)+'</p>'+
                        '<p>'+GnosusXmpp.subNodeFromNode(item)+'</p>'+
                     '</div>'; 
        this.deleteItemDialog(dialog, function() {
            client_ui.block('deleting subscription');
            $.each(Gnosus.findSubscriptionsByNodeAndSubscription(item, 'subscribed'), function() {
                GnosusXmpp.setUnsubscribe(this.service, this.node, this.subid);
            });
        });
    },

    /*-------------------------------------------------------------------------------*/    
    deletePublicationDialog: function(item) {
        var client_ui = this,
            dialog = '<div id="'+this.toId(this.item_dialog)+'" title="delete publication?">'+ 
                        '<p>'+GnosusXmpp.subNodeFromNode(item)+'</p>'+
                     '</div>'; 
        this.deleteItemDialog(dialog, function() {
            client_ui.block('deleting publication');
            GnosusXmpp.setDeletePubSubNode(item);
        });
    },

    /*-------------------------------------------------------------------------------*/    
    deleteItemDialog: function(dialog, delete_item) {
        $(this.item_dialog).remove();            
        $(this.client).append(dialog); 
        $(this.item_dialog).dialog({modal:true, resizable:false, width:380,
            buttons:{'cancel':this.cancelItemDialog.bind(this), 
                     'delete':function() {
                         delete_item();
                         this.cancelItemDialog();            
                     }.bind(this)}});
                 
    },

    /*-------------------------------------------------------------------------------  
     * add
     *-------------------------------------------------------------------------------*/    
    addContactDialog: function() {
        var dialog = '<div id="'+this.toId(this.item_dialog)+'" title="add contact">'+ 
                         '<div class="validate-jid">'+  
                            '<p class="client-form" ><label for="jid">jid</label><input type="text" name="jid" class="required"/></p>'+
                         '</div>'+
                     '</div>'; 
        var client_ui = this;             
        this.addItemDialog(dialog, function() {
            if (!client_ui.dialogButtonIsDisabled() && client_ui.validateRequiredFields()) {
                var jid = $(client_ui.item_dialog+' input').val();
                client_ui.cancelItemDialog(); 
                client_ui.block('adding contact')
                GnosusXmpp.addContact(jid, null, []); 
            }
        });            
    },

    /*-------------------------------------------------------------------------------*/    
    addSubscriptionDialog: function() {
        var dialog = '<div id="'+this.toId(this.item_dialog)+'" class="form" title="add subscription">'+ 
                         '<div class="validate-jid">'+  
                            '<p class="client-form" ><label for="jid">jid</label><input type="text" name="jid" class="required"/></p>'+
                         '</div>'+
                         '<p class="client-form" ><label for="node">node</label><input type="text" name="node" class="required"/></p>'+
                     '</div>'; 
        var client_ui = this;             
        this.addItemDialog(dialog, function() {
            if (!client_ui.dialogButtonIsDisabled() && client_ui.validateRequiredFields()) {
                var jid       = $(client_ui.item_dialog+" input[name='jid']").val(),
                    node      = $(client_ui.item_dialog+" input[name='node']").val(),
                    full_node = GnosusXmpp.userPubsubNode(jid, node),
                    service   = Gnosus.findPubSubServiceByJid(jid);
                client_ui.cancelItemDialog(); 
                client_ui.block('adding subscription')
                if (service) {
                    GnosusXmpp.setSubscribe(service.jid, full_node);
                } else {
                    GnosusXmpp.getPubSubServiceDisco(jid, Strophe.getDomainFromJid(jid), 
                        function() {
                            service = Gnosus.findPubSubServiceByJid(jid);
                            if (service) {                            
                                GnosusXmpp.setSubscribe(service.jid, full_node);
                            } else {
                                this.unblock();
                                this.errorDialog('failed to subscribe to <strong>'+node+'</strong> on service <strong>'+service+'</service>');
                            }
                        }.bind(this),
                        function() {
                            this.unblock();
                            this.errorDialog('failed to subscribe to <strong>'+node+'</strong> on service <strong>'+service+'</service>');
                        }.bind(this)
                    );
                }
            }
        });            
    },

    /*-------------------------------------------------------------------------------*/    
    addPublicationDialog: function() {
        var dialog = '<div id="'+this.toId(this.item_dialog)+'" class="form" title="add contact">'+ 
                         '<p class="client-form" ><label for="node">node</label><input type="text" name="node" class="required"/></p>'+
                     '</div>'; 
        var client_ui = this;             
        this.addItemDialog(dialog, function() {
            if (!client_ui.dialogButtonIsDisabled() && client_ui.validateRequiredFields()) {
                var node = $(client_ui.item_dialog+' input').val();
                client_ui.cancelItemDialog(); 
                client_ui.block('adding publication')
                GnosusXmpp.setCreatePubSubNode(node); 
            }
        });            
    },

    /*-------------------------------------------------------------------------------*/    
    addItemDialog: function(dialog, add_item) {
        $(this.item_dialog).remove();            
        $(this.client).append(dialog); 
        $(this.item_dialog).dialog({modal:true, resizable:false,
            buttons:{'cancel':this.cancelItemDialog.bind(this), 
                     'send':add_item.bind(this)},
            width:380}); 
        this.addJidValidation('send');                               
    },
    
    /*-------------------------------------------------------------------------------  
     * dialogs
     *-------------------------------------------------------------------------------*/    
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
                                  GnosusXmpp.addContact(jid, null, []);
                                  GnosusXmpp.acceptSubscriptionRequest($(this.item_dialog+' p').text());
                                  this.cancelItemDialog();            
                              }.bind(this),
                     'reject':function() {
                                  GnosusXmpp.rejectSubscriptionRequest($(this.item_dialog+' p').text());
                                  this.cancelItemDialog();            
                              }.bind(this),}});
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
     * publications display
     *-------------------------------------------------------------------------------*/    
     showPublicationsDisplay: function(node) {
         var client_ui= this;
         $(this.client_display_content).empty();
         this.displayUnbind();
         this.buildEnterMessage(
             function(msg_input) {
                 var msg = $(msg_input).val().replace(/\n$/,'');
                 GnosusXmpp.setPublishEntry(node, msg);
                 client_ui.block('publishing');
             }
         );
         this.buildMessageContentList(Gnosus.findMessagesByNode(GnosusXmpp.userPubsubNode(Gnosus.account().jid, node)), 'input');
         this.bindDisplayHandler('publish_entry_result', function (ev, msg) {
             this.unblock();
             $(this.client_display_list).prepend(this.buildHeadlineEntryMessage(msg));
         });
         this.bindDisplayHandler('publish_entry_error', function (ev, node) {
             this.unblock();
             this.errorDialog('failed to publish to <strong>'+node+'</strong>');
         });
     },               

     /*-------------------------------------------------------------------------------  
      * subscriptions display
      *-------------------------------------------------------------------------------*/    
      showSubscriptionsDisplay: function(node) {
          this.showGeolocToolbar(node);
          $(this.client_display_content).empty();
          this.displayUnbind();
          this.buildMessageContentList(Gnosus.findMessagesByNode(node), 'no-input');
          this.bindDisplayHandler('headline', function (ev, msgs) {
              var client_ui = this,
                  reg_exp = new RegExp(node, 'g');
              $.each(msgs, function() {
                  if (this.node.match(reg_exp)) {
                      client_ui.prependMessage(this);
                  }
                  if (client_ui.map) {
                     client_ui.setGeolocMapCenter(this); 
                  }
              });
          });         
      },               

      /*-------------------------------------------------------------------------------  
       * geoloc
       *-------------------------------------------------------------------------------*/    
      showGeolocToolbar: function(node) {
          $(this.client_display_toolbar).empty();              
          if (node.match(new RegExp('geoloc', 'g'))) {
              $(this.client_display_toolbar).append('<div class="control"></div>');
              $(this.client_display_toolbar_control).append('<div class="geoloc-map">map</div>');
              $(this.client_display_toolbar_control+' .geoloc-map').click(function() {
                  this.showGeolocMapDisplay(node);
              }.bind(this));
          }
      }, 

      /*-------------------------------------------------------------------------------*/
      showGeolocContentDisplay: function(node) {
          this.buildItemContentDisplay('geoloc', node)
      },
      
      /*-------------------------------------------------------------------------------*/
      showGeolocEventsDisplay: function(node) {          
          this.buildMessageContentList(Gnosus.findMessagesByNode(node), 'no-input');
          this.bindDisplayHandler('headline', function (ev, msgs) {
              var client_ui = this,
                  reg_exp = new RegExp(node, 'g');
              $.each(msgs, function() {
                  if (this.node.match(reg_exp)) {
                      client_ui.prependMessage(this);
                  }
              });
          });         
      },

      /*-------------------------------------------------------------------------------*/
      showGeolocMapDisplay: function(node) {   
          $(this.geoloc_map).remove();            
          var map_dialog = '<div id="'+this.toId(this.geoloc_map)+'/>'; 
          $(this.client).append(map_dialog); 
          $(this.geoloc_map).dialog({modal:true, resizable:false,
              buttons:{'close':function() {
                           $(this).dialog("close");        
                           $(this).remove(); 
                           this.map = null;           
                       }},
              width:0.9*this.client_width, height:this.client_height}); 
          var geoloc = Gnosus.findFirstMessageByNode(node),
              center = new CM.LatLng(38.9143, -77.0390);
          this.map = new CM.Map(this.toId(this.geoloc_map), this.cloudmade);
          this.map.setCenter(center, 15);                
          if (geoloc) {
              this.setGeolocMapCenter(geoloc);
          } 
          var topRight = new CM.ControlPosition(CM.TOP_RIGHT, new CM.Size(50, 20));
          this.map.addControl(new CM.LargeMapControl());
          this.map.addControl(new CM.ScaleControl());
      },

    /*-------------------------------------------------------------------------------*/
    setGeolocMapCenter: function(msg) {
        var lat = parseFloat($(msg.text).find('lat').text());
        var lon = parseFloat($(msg.text).find('lon').text());
        center = new CM.LatLng(lat, lon);
        var marker = new CM.Marker(center, {
        	title: "Current Location"
        });
        this.map.setCenter(center, 15);                
        this.map.addOverlay(marker);
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
         this.buildItemContentDisplay('contacts', contact_name)
     },               

     /*-------------------------------------------------------------------------------*/    
     showContactsToolbar: function(select_mode) {
         var client_ui    = this,
             open_item    = $(client_ui.client_items_content+' ul li.open'),
             select_modes = ['chat','commands','resources','publications'];
         select_mode = select_mode || 'chat' 
         if (open_item.find('.online').length == 0) {
             select_modes = ['chat','publications'];
         }    
         this.buildDisplayToolbar(select_modes, select_mode, 'contacts', function() {
             return open_item.find('.name').text();                
         });
     }, 

     /*-------------------------------------------------------------------------------*/    
     showContactsChatDisplay: function(contact_name) {
        var client_ui= this,
            contact = Gnosus.findAccountByName(contact_name);
        this.buildEnterMessage(
            function(msg_input) {
                var msg = $(msg_input).val().replace(/\n$/,'');
                $(client_ui.client_display_list).prepend(client_ui.buildChatTextMessage(GnosusXmpp.chatTextMessage(contact.jid, msg)));
            }
         );
         this.buildMessageContentList(Gnosus.findMessagesByJidAndType(contact.jid, 'chat'), 'input');
         this.bindDisplayHandler('chat', function (ev, msg) {
             var contact = Gnosus.findAccountByName(contact_name);
             if (msg.from.match(new RegExp(contact.jid, 'g'))) {
                 $(this.client_display_list).prepend(this.buildChatTextMessage(msg));
             }
         });
     },               

     /*-------------------------------------------------------------------------------*/    
     showContactsCommandsDisplay: function(contact_name) {
          var toolbar = '<div class="add"></div>',
              contact = Gnosus.findAccountByName(contact_name);
         $(this.client_display_toolbar_control).append(toolbar);
         contact.deleteCommands();
         this.buildMessageContentList(Gnosus.findMessagesByJidAndContentType(contact.jid, 'command'), 'no-input');
         this.block('retrieving command list');
         $.each(contact.resources, function(j,r) {
             GnosusXmpp.getCommandList(r.jid);
         });
         $(this.client_display_toolbar_control+' .add').click(function() {
             this.commandsDialog(contact.jid, contact.resources);
         }.bind(this));
         this.bindDisplayHandler('command_list_result', function (ev, jid) {
             if (Gnosus.areCommandsAvailable(contact.jid)) {
                 this.unblock();
             }
         });
         this.addCommandEvents();
     },               

     /*-------------------------------------------------------------------------------*/    
     showContactsResourcesDisplay: function(contact_name) {
         var contact   = Gnosus.findAccountByName(contact_name),
             client_ui = this;
         this.buildResourceContentList(contact_name, contact.jid, contact.resources);
     },               

     /*-------------------------------------------------------------------------------*/    
     showContactsPublicationsDisplay: function(contact_name) {
        var contact   = Gnosus.findAccountByName(contact_name),
            client_ui = this;
        this.block('retrieving publications');
        GnosusXmpp.getPubSubServiceDisco(contact.jid, Strophe.getDomainFromJid(contact.jid), 
            function() {
                this.unblock();
                this.buildPubNodeContentList(Gnosus.findPubNodesByJid(contact.jid), function(node) {
                    var pub_status = 'not-subscribed';
                    var sub = Gnosus.findSubscriptionsByNodeAndSubscription(node, 'subscribed');
                    if (sub.length > 0) {
                        pub_status = 'subscribed';
                    }
                    return pub_status;
                });
                $(this.client_display_content).find('.publication-node .status').click(function() {  
                    var node = $(this).siblings('.client-data').text();
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
        this.bindDisplayHandler('subscribe_result', function (ev, subscription) {
            this.unblock();
            var item = Gnosus.findServiceItemByJidAndNode(contact.jid, subscription.node)
            $(this.client_display_content).find('.node:contains('+item.name+')').siblings('.status')
                .removeClass('not-subscribed').addClass('subscribed')
        });
        this.bindDisplayHandler('subscribe_error', function (ev, service, node) {
            this.unblock();
            this.errorDialog('error subscribing to <strong>'+GnosusXmpp.subNodeFromNode(node)+'</strong> on service <strong>'+service+'</strong>');
        });
        this.bindDisplayHandler('unsubscribe_result', function (ev, subscription) {
            this.unblock();
            var item = Gnosus.findServiceItemByJidAndNode(contact.jid, subscription.node)
            $(this.client_display_content).find('.node:contains('+item.name+')').siblings('.status')
                .removeClass('subscribed').addClass('not-subscribed')
        });
        this.bindDisplayHandler('unsubscribe_error', function (ev, service, node) {
            this.unblock();
            this.errorDialog('error unsubscribing from <strong>'+GnosusXmpp.subNodeFromNode(node)+'</strong> on service <strong>'+service+'</strong>');
        });
        this.bindDisplayHandler('disco_info_error', function (ev, jid, node) {
            this.unblock();
            var msg = 'disco info failed';
            if (node) {msg += ' for node <strong>' + node+'</strong>';}
            this.errorDialog(msg);
        });
        this.bindDisplayHandler('disco_items_error', function (ev, jid, node) {
            this.unblock();
            var msg = 'disco items failed';
            if (node) {msg += ' for node <strong>' + node+'</strong>';}
            this.errorDialog(msg);
        });
     },               

    /*-------------------------------------------------------------------------------  
     * resource display
     *-------------------------------------------------------------------------------*/    
    showResourcesDisplay: function(resource) {
        this.addClientResourceContact(Gnosus.account().jid)
        this.showResourcesToolbar();
        this.showResourcesContentDisplay(resource);
    }, 

    /*-------------------------------------------------------------------------------*/    
    showResourcesContentDisplay: function(resource) {
        this.buildItemContentDisplay('resources', resource)
    },               

    /*-------------------------------------------------------------------------------*/    
    showResourcesToolbar: function(open_item_name) {
        this.buildDisplayToolbar(['chat','commands'], 'chat', 'resources', open_item_name)
    }, 

    /*-------------------------------------------------------------------------------*/    
    showResourcesChatDisplay: function(resource) {
       var client_ui = this,
           jid       = this.clientResourceContact()+'/'+resource;       
       this.buildEnterMessage(
           function(msg_input) {
               var msg = $(msg_input).val().replace(/\n$/,'');
               $(client_ui.client_display_list).prepend(client_ui.buildChatTextMessage(GnosusXmpp.chatTextMessage(jid, msg)));
           }
        );
        this.buildMessageContentList(Gnosus.findMessagesByJidAndType(jid, 'chat'), 'input');
        this.bindDisplayHandler('chat', function (ev, msg) {
            if (msg.from.match(new RegExp(jid, 'g'))) {
                $(this.client_display_list).prepend(this.buildChatTextMessage(msg));
            }
        });
    },               

    /*-------------------------------------------------------------------------------*/    
    showResourcesCommandsDisplay: function(resource) {
         var toolbar  = '<div class="add"></div>',
             acct_jid = this.clientResourceContact(),
             full_jid = acct_jid+'/'+resource,
             acct     = Gnosus.findAccountByJid(acct_jid);
        $(this.client_display_toolbar_control).append(toolbar);
        acct.deleteCommands();
        this.buildMessageContentList(Gnosus.findMessagesByJidAndContentType(full_jid, 'command'), 'no-input');
        this.block('retrieving command list');
        $.each(acct.resources, function(j,r) {
            GnosusXmpp.getCommandList(r.jid);
        });
        $(this.client_display_toolbar_control+' .add').click(function() {
            var resources = {};
            resources[full_jid] = Gnosus.findResourceByJid(full_jid);
            this.commandsDialog(full_jid, resources);
        }.bind(this));
        this.bindDisplayHandler('command_list_result', function (ev, jid) {
            this.unblock();
        });
        this.addCommandEvents();
    },               

    /*-------------------------------------------------------------------------------
     * command forms 
     *-------------------------------------------------------------------------------*/  
    commandsDialog: function(jid, resources) {
        var commands  = Gnosus.findAllCommandsByJid(jid) ,
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
            dialogClass:'command-dialog', width:400, height:500}); 
        $(this.item_dialog+' .commands').accordion({collapsible: true});                
        $(this.item_dialog+' .command').click(function() {
            var name = $(this).text(),
                cat  = $(this).parent('div').prev('h3').text();
            if (cat != 'ungrouped') {name = cat + '/' + name;}   
            $.each(resources, function(rjid, r) {
                var cmd  = Gnosus.findCommandByJidAndName(rjid, name);
                $(client_ui.client_display_list).prepend(client_ui.buildTextCommandMessage(GnosusXmpp.setCommand({to:rjid, node:cmd.node})));
            });
            client_ui.cancelItemDialog();
            client_ui.block('command request pending');
        });
    },

    /*-------------------------------------------------------------------------------*/    
    formDialog: function(form) {
        var title   = htmlEncode($(form).find('title').text()) || 'command form',
            dialog  = '<div id="'+this.toId(this.item_dialog)+'" title="'+title+'">',
            inst    = htmlEncode($(form).find('instructions').text());
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
                              var jid       = $(form).attr('from'),
                                  cmd       = $(form).find('command').eq(0),
                                  node      = cmd.attr('node'),
                                  sessionid = cmd.attr('sessionid');
                              $(client_ui.client_display_list).prepend(client_ui.buildXCommandMessage(GnosusXmpp.setCommand(
                                  {to:jid, node:node, action:'submit', sessionid:sessionid, payload:GnosusXmpp.buildFormXDataPayload(client_ui.readForm())})));
                              this.cancelItemDialog();            
                              client_ui.block('command request pending');
                          }
                       }.bind(this)},
           dialogClass:'x-form', width:400, height:500
       });
       this.addJidValidation();        
    },

    /*-------------------------------------------------------------------------------*/  
    addCommandEvents: function() {  
        this.bindDisplayHandler('command_list_error', function (ev, jid) {
            this.unblock();
            this.errorDialog('failed to retrieve command list');
        });
        this.bindDisplayHandler('command_form', function (ev, iq) {
            this.unblock();
            this.formDialog(iq);
        });
        this.bindDisplayHandler('command_cancel', function (ev, msg) {
            $(this.client_display_list).prepend(this.buildTextCommandMessage(msg));
            this.unblock();
        });
        this.bindDisplayHandler('command_result', function (ev, msg) {
            var display_message;
            if (msg.type == 'x') {
                display_message = this.buildXCommandMessage(msg);
            } else if (msg.typs = 'text') {
                display_message = this.buildTextCommandMessage(msg);
            }
            $(this.client_display_list).prepend(display_message);
            this.unblock();
        });
        this.bindDisplayHandler('command_error', function (ev, jid, node, msg) {
            this.unblock();
            this.errorDialog('command request failed for jid <strong>'+jid+'</strong> and node <strong>'+node+'</strong><p>'+msg+'</p>');
        });
    },

    /*-------------------------------------------------------------------------------
     * display utils 
     *-------------------------------------------------------------------------------*/  
     prependMessage: function(msg) {
         $(this.client_display_list).prepend(this['build'+this.camelize(msg.type)+this.camelize(msg.content_type)+'Message'](msg));
     },     

     /*-------------------------------------------------------------------------------*/ 
     removeSelectedResource: function() {
          var item = $(this.client_items_content+' ul li.open .item');
          item.removeClass('resource-selected');
          item.find('.resource').remove();
     },
     
     /*-------------------------------------------------------------------------------*/ 
     buildEnterMessage: function(send_msg) {
         var enter_msg = 'enter message',
             send_message = '<div class ="client-display-input">'+
                                '<textarea class="init">'+enter_msg+'</textarea>'+
                            '</div>';
         $(this.client_display_content).append(send_message);
         var textarea = $(this.client_display_input+' textarea'),
             orig_textarea_height = textarea.height();
         textarea.autoResize();
         textarea.keyup(function(evt) {
             var input = evt.keyCode;
             if (input == '13') {
                 send_msg(this);
                 $(this).val('');
                 $(this).height(orig_textarea_height);
                 $(this).css('overflow','hidden');
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
     },

     /*-------------------------------------------------------------------------------*/ 
     buildMessageContentList: function(content_list, list_type) {
        var height    = this.getClientDisplayListHeight(list_type),
            msgs      = ['<ul class="client-display-list" style="height:'+height+'px">'],
            client_ui = this;
        $.each(content_list, function () {
            msgs.push(client_ui['build'+client_ui.camelize(this.type)+client_ui.camelize(this.content_type)+'Message'](this));
        });
        msgs.push('</ul>')
        $(this.client_display_content).append(msgs.join(''));
    },   
      
    /*-------------------------------------------------------------------------------*/ 
     buildPubNodeContentList: function(content_list, status) {
        var height = this.getClientDisplayListHeight('no-input'),
            msgs   = ['<ul class="client-display-list publication-nodes" style="height:'+height+'px">'];
        $.each(content_list, function () {
            msgs.push('<li><div class="publication-node">'+
                           '<div class="status '+status(this.node)+'"></div>'+ 
                           '<div class="client-data">'+this.node+'</div>'+          
                           '<div class="node">'+this.name+'</div>'+
                       '</div></li>'
            );
        });
        msgs.push('</ul>')
        $(this.client_display_content).append(msgs.join(''));
    },   
              
    /*-------------------------------------------------------------------------------*/ 
     buildResourceContentList: function(contact_name, jid, content_list) {
        var height    = this.getClientDisplayListHeight('no-input'),
            msgs      = ['<ul class="client-display-list contact-resources" style="height:'+height+'px">'],
            client_ui = this;
        $.each(content_list, function () {
            msgs.push(client_ui.buildResourceContent(this));
        });
        msgs.push('</ul>')
        $(this.client_display_content).append(msgs.join(''));
        this.addResourceContentEvents(contact_name, jid);
    },   
 
    /*-------------------------------------------------------------------------------*/ 
    buildResourceContent: function(resource) {
        return '<li><div class="contact-resource">'+
                   Strophe.getResourceFromJid(resource.jid) + 
               '</div></li>'
    },   
          
    /*-------------------------------------------------------------------------------*/ 
    addResourceContentEvents: function(contact_name, contact_jid) {
        var client_ui = this;
        $(this.client_contact_resources+' li').hover(
            function() {$(this).addClass('selected');},
            function() {$(this).removeClass('selected');}
        ); 
        $(this.client_contact_resources+' li').click(function() {
            var resource        = $(this).find('.contact-resource').text(),
                contact_jid_div = '<div class="jid">'+contact_jid+'</div>',
                item            = $(client_ui.client_items_content+' ul li.open .item');
            client_ui.addClientResourceContact(contact_jid)
            client_ui.showResourcesToolbar(function() {
                return $(client_ui.client_items_content+' ul li.open').find('.resource').text();                
            });
            client_ui.showResourcesContentDisplay(resource);
            item.addClass('resource-selected');
            item.append('<div class="resource">'+resource+'</div>');
            $(client_ui.client_display_toolbar).prepend(contact_jid_div);
            $(client_ui.client_display_toolbar_jid).click(function() {
                client_ui.removeSelectedResource();
                client_ui.showContactsToolbar('resources');
                client_ui.buildItemContentDisplay('contacts', contact_name)
            });
        }); 
    },
                            
    /*-------------------------------------------------------------------------------*/ 
    buildListItems: function(items, item_type, item_name, item_status, open_item_name, delete_item_name, no_controls) {
        var list_items = '<ul>';
        var client_ui = this;
        $.each(items, function () {
            var status = '';
            var iname = item_name(this);
            if (item_status) {status = item_status(this)}
            list_items += client_ui.buildItemListItems(item_name(this), item_type, status, no_controls);
        });
        list_items += '</ul>';
        $(this.client_items_content).append(list_items);
        this.addItemListEvents($(this.client_items_content+' ul li'), open_item_name, delete_item_name, no_controls);
    },

    /*-------------------------------------------------------------------------------*/ 
    buildItemListItems: function (item, item_type, item_status, no_controls) { 
        item_status = item_status || ''
        var item = '<li>' +
                       '<div class="'+item_type+' item '+item_status+'">'+ 
                           item+
                       '</div>'
                       if (!no_controls) {
                           item += '<div style="display: none" class="controls">'+
                                       '<img src="/images/data-delete.png"/>'+
                                   '</div>';
                       }
                   item += '</li>';
        return item;
    },

    /*-------------------------------------------------------------------------------*/ 
    addItemListEvents: function(select_item, open_item_name, delete_item_name, no_controls) {
        var client_ui = this;
        select_item.hover(
            function() {$(this).addClass('selected').find('.controls').show();},
            function() {$(this).removeClass('selected').find('.controls').hide();}
        ); 
        select_item.find('.item').click(function() {
            var item_type = client_ui.itemTypeSelected();
            client_ui.removeSelectedResource();
            $(this).parents('li').siblings('.open').removeClass('open');
            $(this).parents('li').addClass('open')
            var item_name = '';
            if (open_item_name) {
                item_name = open_item_name(this);
            } else {
                item_name = $(this).text();
            }
            client_ui['show'+client_ui.capitalize(item_type)+'Display'](item_name);
        }); 
        if (!no_controls) {
            select_item.find('img').click(function() {            
                var item_name = '';
                if (delete_item_name) {
                    item_name = delete_item_name(this);
                } else {
                    item_name = $(this).parents('li').eq(0).find('.item').text();
                }
                var item_type = client_ui.itemTypeSelected();
                client_ui['delete'+client_ui.capitalize(client_ui.singular(item_type))+'Dialog'](item_name);
            }); 
        }
    },
        
    /*-------------------------------------------------------------------------------*/   
    addContactRequestEvent: function() {
        this.bindItemsHandler('presence_subscribe', function (ev, jid) {
            this.subscriptionRequestDialog(jid);
        });
    },

    /*-------------------------------------------------------------------------------*/    
    buildDisplayToolbar: function(items, select_item, item_type, open_item_name) {
        $(this.client_display_toolbar).empty();
        var toolbar = '<ul class="client-display-modes">';
        $.each(items, function() {
            if (select_item == this) {
                toolbar += '<li class="selected">'+this+'</li>';
            } else {
                toolbar += '<li>'+this+'</li>';
            }
        })
        toolbar += '</ul><div class="control"></div>';
        $(this.client_display_toolbar).append(toolbar);
        var client_ui = this;
        $(this.client_display_modes+' li').click(function() {
            var item_name = '';
            if (open_item_name) {
                item_name = open_item_name();
            } else {
                item_name = $(client_ui.client_items_content+' ul li.open').children('.item').text();
            }
            $(this).siblings('li.selected').removeClass('selected');
            $(this).addClass('selected');
            client_ui['show'+client_ui.capitalize(item_type)+'ContentDisplay'](item_name);
        });
    }, 
        
    /*-------------------------------------------------------------------------------*/    
    buildItemContentDisplay: function(item_type, item) {
        $(this.client_display_toolbar_control).empty();
        $(this.client_display_content).empty();
        this.displayUnbind();
        var display_type = this.clientDisplayMode();
        this['show'+this.capitalize(item_type)+this.capitalize(display_type)+'Display'](item);
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
    validateRequiredFields: function() {
        var is_valid = true;
        if (this.noDialogErrors()) {
            $(this.item_dialog).find('input.required').each(function() {
                if (!$(this).val()) {
                    is_valid = false;
                    $(this).after('<div class="dialog-error">'+$(this).attr('name')+' required</div>')                   
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
                   '<div class="text">'+htmlEncode(msg.text)+'</div>'+
               '</div></li>';
    },

    /*-------------------------------------------------------------------------------*/    
    buildHeadlineXMessage: function(msg) {
        return '<li><div class="x-message">'+
                   this.messageInfo(msg)+
                   '<div class="node">'+GnosusXmpp.subNodeFromNode(msg.node)+'</div>'+
                   this.buildXDataBody(msg.text)+
               '</div></li>';
    },

    /*-------------------------------------------------------------------------------*/    
    buildHeadlineGeolocMessage: function(msg) {
        return '<li><div class="x-message">'+
                   this.messageInfo(msg)+
                   '<div class="node">'+GnosusXmpp.subNodeFromNode(msg.node)+'</div>'+
                   this.buildGeolocBody(msg.text)+
               '</div></li>';
    },

    /*-------------------------------------------------------------------------------*/    
    buildHeadlineEntryMessage: function(msg) {
        return '<li><div class="text-message">'+
                   this.messageInfo(msg)+
                   '<div class="node">'+GnosusXmpp.subNodeFromNode(msg.node)+'</div>'+
                   '<div class="entry">'+htmlEncode(msg.text)+'</div>'+
               '</div></li>';
    },

    /*-------------------------------------------------------------------------------
     * geoloc 
     *-------------------------------------------------------------------------------*/ 
    buildGeolocBody: function(data) {
        var client_ui = this;
        return '<table class="hash">'+ 
                   $.map($(data).children(), function(c,i) {
                          return '<tr>'+
                                     '<td class="attr">'+htmlEncode($(c).get(0).tagName)+'</td>'+
                                     '<td class="val">'+htmlEncode($(c).text())+'</td>'+
                                 '</tr>';
                   }).join('')+
               '</table>';
    },

    /*-------------------------------------------------------------------------------
     * x data 
     *-------------------------------------------------------------------------------*/ 
    buildTextCommandMessage: function(msg) {
        return '<li><div class="command-request-message">'+
                   this.messageInfo(msg)+
                   '<div class="header">'+msg.node+'</div>'+
                   '<div class="node">'+htmlEncode(msg.text)+'</div>'+
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
        return '<div class="scalar">'+htmlEncode(this.getXDataValues(data))+'</div>';
    },

    /*-------------------------------------------------------------------------------*/ 
    getXDataValues: function(field) {
        return $.map($(field).find('value'), function(v,i) {
                   return htmlEncode($(v).text());
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
     getNameFromField: function(field) {
         return htmlEncode($(field).attr('var'));
     },
     
     /*-------------------------------------------------------------------------------*/ 
     getLabelFromField: function(field) {
         return htmlEncode($(field).attr('label'));
     },

     /*-------------------------------------------------------------------------------*/ 
    buildFixedControl: function(field) {
        var label   = htmlEncode($(field).find('value').eq(0).text());
        return '<div class="fixed">'+label+'</div>';
    },
 
    /*-------------------------------------------------------------------------------*/ 
    buildBooleanControl: function(field) {
        var name    = this.getNameFromField(field),
            label   = this.getLabelFromField(field),
            control = '<div class="boolean">';
        control += '<input type="checkbox" name="'+name+'"/>'; 
        if (label) {control += '<label>'+label+'</label>';}  
        control += '</br></div>'  
        return control;              
    },
    
    /*-------------------------------------------------------------------------------*/ 
    buildListSingleControl: function(field) {
        var name    = this.getNameFromField(field),
            label   = this.getLabelFromField(field),
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
        var name    = this.getNameFromField(field),
            label   = this.getLabelFromField(field),
            control = '<div class="input-single '+klass+'">';
        if (label) {control += '<label>'+label+'</label>';}    
        control += '<input type="'+type+'" name="'+name+'"/></br></div>'; 
        return control;              
    },
    
    /*-------------------------------------------------------------------------------*/ 
    buildTextMultiControl: function(field) {
        var name    = this.getNameFromField(field),
            label   = this.getLabelFromField(field),
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

/*---------------------------------------------------------------------------------------
 * encode/decode html
 *---------------------------------------------------------------------------------------*/ 
function htmlEncode(value){ 
    if (value) {
        return $('<div/>').text(value).html(); 
    }
} 

/*---------------------------------------------------------------------------------------*/ 
function htmlDecode(value){ 
    if (value) {
        return $('<div/>').html(value).text(); 
    }
}

/******************************************************************************************
 plugins
/******************************************************************************************/
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