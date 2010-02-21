/**********************************************************************************
logging
**********************************************************************************/
function rawInput(data) {
    console.log('RECV: ' + data);
}

function rawOutput(data) {
    console.log('SENT: ' + data);
}

// Strophe.log = function (level, msg) {
//     console.log('LOG: ' + msg);
// }

/**********************************************************************************
connection
**********************************************************************************/
function connect(service, jid, password) {
    var conn = new Strophe.Connection(service);
    conn.rawInput = rawInput;
    conn.rawOutput = rawOutput;
	conn.connect(jid, password, onConnect);
	GnosusXmpp.connection = conn;
	Gnosus.account = new Account(service, jid, password)
}

/*-------------------------------------------------------------------------------*/
function onConnect(status) {
    if (status == Strophe.Status.CONNECTING) {
        $(document).trigger('connecting');
	    console.log('Strophe is connecting.');
    } else if (status == Strophe.Status.CONNFAIL) {
	    console.log('Strophe failed to connect.');
    } else if (status == Strophe.Status.DISCONNECTING) {
	    console.log('Strophe is disconnecting.');
    } else if (status == Strophe.Status.DISCONNECTED) {
	    console.log('Strophe is disconnected.');
    } else if (status == Strophe.Status.CONNECTED) {
        $(document).trigger('connected');
	    console.log('Strophe is connected.');
    }
}

/**********************************************************************************
data model interface
**********************************************************************************/
Gnosus = {

    /*-------------------------------------------------------------------------------*/
    account: null,
    contacts: {},
    messages: [],

    /*-------------------------------------------------------------------------------
    contacts
    ---------------------------------------------------------------------------------*/
    addContact: function(item) {
        var groups = item.find('group').map(function (g, i) {g.text();});
        jid = item.attr('jid');
        Gnosus.contacts[jid] = new Contact(item.attr('jid'), item.attr('name'), item.attr('ask'), item.attr('subscription'), groups);
        return Gnosus.contacts[jid];
    },
    findContactByJid: function(jid) {
        return Gnosus.contacts[jid];
    },
    findContactByName: function(name) {
        var result = null;
        for (var jid in Gnosus.contacts) {
            if (Gnosus.contacts[jid].name == name) {
                result = Gnosus.contacts[jid];
                break;
            }
        }
        return result;
    },
    findAllContacts: function() {
        return Gnosus.contacts;
    },
    updateContact: function(item) {
        var contact = Gnosus.contacts[item.attr('jid')];
        if (contact) {
            contact.groups = [];
            contact.setAttributes(item);
        }
        return contact;
    },
    removeContact: function(item) {
        jid = item.attr('jid');
        var contact = Gnosus.contacts[jid];
        if (contact) {
            delete(Gnosus.contacts[jid]);
        }
        return contact;
    },

    /*-------------------------------------------------------------------------------
    contact resources
    ---------------------------------------------------------------------------------*/
    addContactResource: function(presence) {
        var from = $(presence).attr('from');
        var bare_jid = Strophe.getBareJidFromJid(from);
        if (Gnosus.contacts[bare_jid]) {
            resource = new Resource(from, $(presence).find('show').text(), $(presence).find('status').text());
            Gnosus.contacts[bare_jid].addResource(resource);
        }
    },
    removeContactResource: function(presence) {
        var from = $(presence).attr('from');
        var bare_jid = Strophe.getBareJidFromJid(from);
        if (Gnosus.contacts[bare_jid]) {
            Gnosus.contacts[bare_jid].removeResource(from);
        }
    },
    removeContactResources: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        if (Gnosus.contacts[bare_jid]) {
            Gnosus.contacts[bare_jid].removeResources();
        }
    },
    findAllContactResources: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        return Gnosus.contacts[bare_jid] ? Gnosus.contacts[bare_jid].resources : null;
    },
    findContactResource: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        return Gnosus.contacts[bare_jid] ? Gnosus.contacts[bare_jid].resources[jid] : null;
    },
    contactOffline: function() {
        $.each(Gnosus.contacts, function(j,c) {
            c.resources = {};
        });
    },

    /*-------------------------------------------------------------------------------
    account resources
    ---------------------------------------------------------------------------------*/
    addAccountResource: function(presence) {
        var from = $(presence).attr('from');
        resource = new Resource(from);
        Gnosus.account.addResource(resource);
    },
    removeAccountResource: function(presence) {
        var from = $(presence).attr('from');
        Gnosus.account.removeResource(from);
    },
    removeAccountResources: function() {
        Gnosus.account.removeResources();
    },
    findAllAccountResources: function() {
        return Gnosus.account.resources;
    },
    findAccountResource: function(jid) {
        return Gnosus.account.resources[jid];
    },
    
    /*-------------------------------------------------------------------------------
    chat messages
    ---------------------------------------------------------------------------------*/
    addIncomingChatTextMessage: function(msg) {
        var msg_model = new Message($(msg).attr('to'), $(msg).attr('from'), $(msg).find('body').text(), 'chat', 'text');
        Gnosus.messages.unshift(msg_model);  
        return msg_model;      
    },
    addOutgoingChatTextMessage: function(to, body) {
        var msg_model = new Message(to, Gnosus.account.fullJid(), body, 'chat', 'text');
        Gnosus.messages.unshift(msg_model);  
        return msg_model;      
    },
    findAllMessages: function() {
        return Gnosus.messages;
    },
    findMessagesByJidAndType: function(jid, type) {
        var jid_rexp = new RegExp(jid, 'g');
        var result = $.grep(Gnosus.messages, function(m) {
                        return ((m.from.match(jid_rexp) || m.to.match(jid_rexp)) && m.type == type)
                      });   
        return result;         
    },
    findMessagesByJidAndContentType: function(jid, content_type) {
        var jid_rexp = new RegExp(jid, 'g');
        var result = $.grep(Gnosus.messages, function(m) {
                        return ((m.from.match(jid_rexp) || m.to.match(jid_rexp)) && m.content_type == content_type)
                      });   
        return result;         
    },
    
    /*-------------------------------------------------------------------------------
    commands
    ---------------------------------------------------------------------------------*/
    addCommand: function(jid, node, name) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        var resource = null;
        if (bare_jid == Gnosus.account.jid) {
            resource = Gnosus.findAccountResource(jid);
        } else {
            resource = Gnosus.findContactResource(jid);
        }
        if (resource) {
            resource.addCommand(node, name);
        }
    },
    initCommands: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        var resource = null;
        if (bare_jid == Gnosus.account.jid) {
            resource = Gnosus.findAccountResource(jid);
        } else {
            resource = Gnosus.findContactResource(jid);
        }
        if (resource) {
            resource.initCommands();
        }
    },
    areCommandsAvailable: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        var has_commands = false;
        if (bare_jid == jid) {
            var resources = {};
            if (bare_jid == Gnosus.account.jid) {
                resources = Gnosus.findAllAccountResources(jid);
            } else {
                resources = Gnosus.findAllContactResources(jid);
            }
            for (var jid in resources) {
                if (!resources[jid].commands) {
                    has_commands = false;
                    break;
                } else {
                    has_commands = true;
                }
            }
        } else {
            var resource = null;
            if (bare_jid == Gnosus.account.jid) {
                resource = Gnosus.findAccountResource(jid);
            } else {
                resource = Gnosus.findContactResource(jid);
            }
            if (resource) {
                if (resource.commands) {
                    has_commands = true;
                }
            }
        }
        return has_commands;
    },
    findAllCommands: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        var all_commands = {};
        var addCommandHash = function (cmds) {
            $.each(cmds, function() {
                if (!all_commands[this.name]) {
                    all_commands[this.name] = [];
                }
                all_commands[this.name].push($(this));
            });
        };
        if (bare_jid == jid) {
            var resources = [];
            if (bare_jid == Gnosus.account.jid) {
                resources = Gnosus.findAllAccountResources(jid);
            } else {
                resources = Gnosus.findAllContactResources(jid);
            }
            $.each(resources, function(j,r) {
                addCommandHash(r.commands);
            });
        } else {
            var resource = null;
            if (bare_jid == Gnosus.account.jid) {
                resource = Gnosus.findAccountResource(jid);
            } else {
                resource = Gnosus.findContactResource(jid);
            }
            addCommandHash(resource.commands);
        }
        return all_commands;
    },
    addCommandXDataMessage: function(iq) {
        var cmd  = $(iq).find('command').eq(0),
            data = $(iq).find('x').eq(0),
            from = $(iq).attr('from') || Gnosus.account.fullJid();
        var msg_model = new Message($(iq).attr('to'), from, data, 'x', 'command', $(cmd).attr('node'), $(iq).attr('id'));
        Gnosus.messages.unshift(msg_model);  
        return msg_model;      
    },
    addCommandTextMessage: function(to, node, text, id) {
        var msg_model = new Message(to, Gnosus.account.fullJid(), text, 'text', 'command', node, id);
        Gnosus.messages.unshift(msg_model);  
        return msg_model;      
    },
    /*-------------------------------------------------------------------------------
    subscription
    ---------------------------------------------------------------------------------*/
    addSubscription: function(sub) {
        sub_model = new Subscription($(sub).attr('node'), $(sub).attr('jid'), $(sub).attr('subscription'), $(sub).attr('subid'));
        Gnosus.account.subscriptions.push(sub_model);
        return sub_model;
    },
    /*-------------------------------------------------------------------------------
    disco
    ---------------------------------------------------------------------------------*/
    addService: function(jid, service, node, serv) {
        var bare_jid = Strophe.getBareJidFromJid(jid),
            service_model = new Service(service, node, $(serv).attr('name'), $(serv).attr('category'), $(serv).attr('type'));
        if (bare_jid == Gnosus.account.jid) {
            Gnosus.account.services.push(service_model);
        } else {
            var contact = Gnosus.findContactByJid(jid);
            if (contact) {contact.services.push(service_model);}
        }
        return service_model;
    },
    addServiceItem: function(jid, service, parent_node, item) {
        var bare_jid = Strophe.getBareJidFromJid(jid),
            item_model = new ServiceItem(service, parent_node, $(item).attr('jid'), $(item).attr('node'), $(item).attr('name'));
        if (bare_jid == Gnosus.account.jid) {
            Gnosus.account.service_items.push(item_model);
        } else {
            var contact = Gnosus.findContactByJid(jid);
            if (contact) {contact.service_items.push(item_model);}
        }
        return item_model;
    },
    addServiceFeature: function(jid, service, node, feature) {
        var bare_jid = Strophe.getBareJidFromJid(jid),
            feature_model = new ServiceFeature(service, node, $(feature).attr('var'));
        if (bare_jid == Gnosus.account.jid) {
            Gnosus.account.service_features.push(feature_model);
        } else {
            var contact = Gnosus.findContactByJid(jid);
            if (contact) {contact.service_features.push(feature_model);}
        }
        return feature_model;
    },
    findPubSubNodesByJid: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid),
            items = [];
        if (bare_jid == Gnosus.account.jid) {
            items = $.grep(Gnosus.account.service_items, function(s,i) {
                        return s.node.match(new RegExp(GnosusXmpp.user_pubsub_root(jid), 'g'));               
                    });
        } else {
            var contact = Gnosus.findContactByJid(jid);
            if (contact) {
                items = $.grep(contact.service_items, function(s,i) {
                           if (s.node) {
                               return s.node.match(new RegExp(GnosusXmpp.user_pubsub_root(jid), 'g')); 
                            } else {
                                return false;
                            }              
                        });
            }
        }
        return items;
    },
}

/**********************************************************************************
models
**********************************************************************************/
function Account(service, jid, password) {
    this.service = service;
    this.jid = Strophe.getBareJidFromJid(jid);
    this.resource = Strophe.getResourceFromJid(jid) || 'gnos.us';
    this.password = password;
    this.resources = {};
    this.services = [];
    this.service_items = [];
    this.service_features = [];
    this.subscriptions = [];
    this.publications = [];
}

Account.prototype = {
    fullJid: function() {
        return this.jid+'/'+this.resource;
    },
    addResource: function(resource) {
        this.resources[resource.jid] = resource;
    },
    deleteCommands: function() {
        $.each(this.resources, function(j,r) {
            r.deleteCommands();
        });
    }    
}

/*-------------------------------------------------------------------------------*/
function Message(to, from, text, type, content_type, node, id, item_id) {
    this.to = to;
    this.from = from;
    this.type = type;
    this.text = text;
    this.content_type = content_type;
    this.node = node;
    this.item_id = item_id;
    this.created_at = new Date();
    this.id = id;
}

Message.prototype = {
    createdAtAsString: function() {
        var minutes = this.created_at.getMinutes();
        minutes = minutes < 10 ? '0'+minutes : minutes
        return this.created_at.getFullYear()+'/'+(this.created_at.getMonth()+1)+'/'+this.created_at.getDate()+' '+
               this.created_at.getHours()+':'+minutes;
    }
}

/*-------------------------------------------------------------------------------*/
function Contact(jid, name, ask, subscription, groups) {
    this.jid = jid;
    this.name = name || jid;
    this.ask = ask || '';
    this.subscription = subscription || 'none';
    this.groups = groups;
    this.resources = {};
    this.services = [];
    this.service_items = [];
    this.service_features = [];
    this.publications = [];
}

Contact.prototype = {
    show: function() {
        var stat = 'offline';
        for (var jid in this.resources) {
            stat = this.resources[jid].show;
            if (stat == 'online') {
                break;
            }
        }
        return stat;
    },
    addResource: function(resource) {
        this.resources[resource.jid] = resource;
    },
    setAttributes: function(item) {
        this.jid = item.attr('jid');
        this.name =  item.attr('name') || this.jid;
        this.ask = item.attr('ask') || '';
        this.subscription = item.attr('subscription') || 'none';
        this.groups = item.find('group').map(function (g, i) {g.text();});
    },
    removeResource: function(jid) {
        delete(this.resources[jid]);
    },
    removeResources: function() {
        this.resources = {};
    },
    deleteCommands: function() {
        $.each(this.resources, function(j,r) {
            r.deleteCommands();
        });
    },
}

/*-------------------------------------------------------------------------------*/
function Resource(jid, show, status) {
    this.jid = jid;
    this.show = show || 'online';
    this.status = status || 'unknown';
    this.client_name = '';
    this.client_version = '';
    this.client_os = '';
    this.commands = null;
}

Resource.prototype = {
    setVersionAttributes: function(version) {
        this.client_name = $(version).find('name').text() || 'none';
        this.client_version =  $(version).find('version').text() || 'none';
        this.client_os =  $(version).find('os').text() || 'none';
    },
    deleteCommands: function() {
        this.commands = null;
    },
    initCommands: function() {
        if (!this.commands) {
            this.commands = [];
        }
    },
    addCommand: function(node, name) {
        this.commands.push(new Command(this.jid, node, name));
    }
}

/*-------------------------------------------------------------------------------*/
function Command(jid, node, name) {
    this.node = node;
    this.jid = jid;
    this.name = name || node;
}

Command.prototype = {
}

/*-------------------------------------------------------------------------------*/
function Subscription(node, service, subscription, subid) {
  this.node = node;
  this.service = service;
  this.subscription = subscription;
  this.subid = subid
}

Subscription.prototype = {    
}

/*-------------------------------------------------------------------------------*/
function Service(jid, node, name, category, type) {
    this.jid = jid;
    this.name = name;
    this.category = category;
    this.type = type;
    this.node = node;
}

Service.prototype = {
    
}

/*-------------------------------------------------------------------------------*/
function ServiceItem(service, parent_node, jid, node, name) {
    this.parent_node = parent_node;
    this.service = service;
    this.node = node;
    this.jid = jid;
    this.name = name;
}

ServiceItem.prototype = {
    
}

/*-------------------------------------------------------------------------------*/
function ServiceFeature(service, node, feature_var) {
    this.node = node;
    this.service = service;
    this.feature_var = feature_var;
}

ServiceFeature.prototype = {
    
}

/**********************************************************************************
Gnosus XMPP Interface interface
**********************************************************************************/
GnosusXmpp = {
    
    /*---------------------------------------------------------------------------*/
    connection: null,
    
    /*-------------------------------------------------------------------------------
    pubsub nodes
    ---------------------------------------------------------------------------------*/
    pubsub_root: function(jid) {
        return '/home/'+Strophe.getDomainFromJid(jid);
    },

    /*-------------------------------------------------------------------------------*/
    user_pubsub_root: function(jid) {
        return GnosusXmpp.pubsub_root(jid)+'/'+Strophe.getNodeFromJid(jid);
    },

    /*-------------------------------------------------------------------------------
    roster
    ---------------------------------------------------------------------------------*/
    addContact: function (jid, name, groups) {
        var iq = $iq({type: 'set'}).c('query', {xmlns: Strophe.NS.ROSTER}).c('item', {name: name || "", jid: jid});
        if (groups && groups.length > 0) {
            $.each(groups, function () {
                iq.c("group").t(this).up();
            });
        }
        GnosusXmpp.connection.sendIQ(iq, function(iq) {
            var type = $(iq).attr('type');
            if (type == 'result') {
                GnosusXmpp.sendSubscriptionRequest(jid);
                $(document).trigger('roster_item_add_result', iq);
            } else {
                $(document).trigger('roster_item_add_error', iq);
            }
        });
    },
    
    /*-------------------------------------------------------------------------------*/
    removeContact: function (jid) {
        var iq = $iq({type: 'set'}).c('query', {xmlns: Strophe.NS.ROSTER}).c('item', {jid: jid, subscription: 'remove'});
        GnosusXmpp.connection.sendIQ(iq, function(iq) {
            var type = $(iq).attr('type');
            if (type == 'result') {
                GnosusXmpp.connection.send($pres({to: jid, type: "unsubscribe"}));
                $(document).trigger('roster_item_remove_result', iq);
            } else {
                $(document).trigger('roster_item_remove_error', iq);
            }
        });
    },
  
    /*-------------------------------------------------------------------------------*/
    acceptSubscriptionRequest: function (jid) {
        GnosusXmpp.connection.send($pres({to: jid, type: "subscribed"}));
    },
 
    /*-------------------------------------------------------------------------------*/
    rejectSubscriptionRequest: function (jid) {
        GnosusXmpp.connection.send($pres({to: jid, type: "unsubscribed"}));
    },
 
    /*-------------------------------------------------------------------------------*/
    sendSubscriptionRequest: function (jid) {
        GnosusXmpp.connection.send($pres({to: jid, type: 'subscribe'}));
    },
 
    /*-------------------------------------------------------------------------------*/
    initialPresence: function () {
        var presence = $pres({priority: 1});
        GnosusXmpp.connection.send(presence);
    },
    
    /*-------------------------------------------------------------------------------
    chat
    ---------------------------------------------------------------------------------*/
    chatTextMessage: function (to, body) {
        var msg = $msg({to:to, type:'chat'}).c('body').t(body);
        GnosusXmpp.connection.send(msg);
        return Gnosus.addOutgoingChatTextMessage(to, body);
    }, 
    
    /*-------------------------------------------------------------------------------
    commands
    ---------------------------------------------------------------------------------*/
    sendGetCommandList: function (to) {
        var cmd_iq = $iq({to:to, type:'get'}).c('query', {xmlns:Strophe.NS.DISCO_ITEMS, node:'http://jabber.org/protocol/commands'});
        GnosusXmpp.connection.sendIQ(cmd_iq, function(iq) {
            var type = $(iq).attr('type');
            var jid = $(iq).attr('from');
            if (type == 'result') {
                Gnosus.initCommands(jid);
                $(iq).find('item').each(function () {
                    Gnosus.addCommand(jid, $(this).attr('node'), $(this).attr('name'));
                });
                $(document).trigger('command_list_result', jid);
            } else {
                $(document).trigger('command_list_error', jid);
            }
        });
    },  
    
    /*-------------------------------------------------------------------------------*/
    sendCommand: function (args) {
        var cmd_attr = {xmlns:Strophe.NS.COMMANDS, node:args['node']},
            msg = null;
        if (args['action']) {
            cmd_attr['action'] = args['action'];
        } else {
            cmd_attr['action'] = 'execute';
        }
        var cmd_iq = $iq({to:args['to'], type: 'set'}).c('command', cmd_attr);
        if (args['payload']) {
            cmd_iq.cnode(args['payload']);
            msg = Gnosus.addCommandXDataMessage(cmd_iq.nodeTree);
        } else {
            msg = Gnosus.addCommandTextMessage(args['to'], args['node'], 'command request');
        }
        GnosusXmpp.connection.sendIQ(cmd_iq, function(iq) {
            var type = $(iq).attr('type'),
                jid  = $(iq).attr('from');
            if (type == 'result') {
                var x   = $(iq).find('x').eq(0),
                    x_type = 'result';
                if (x) {x_type = $(x).attr('type')}
                if (x_type == 'form') {
                    $(document).trigger('command_form', iq);
                } else {
                    $(document).trigger('command_result', Gnosus.addCommandXDataMessage(iq));
                }
            } else {
                $(document).trigger('command_error', jid);
            }
        });
        return msg;
    },
     
    /*-------------------------------------------------------------------------------*/
    buildFormXDataPayload: function(form_data) {
        var xdata = $build("x", {xmlns:Strophe.NS.XDATA, type:'submit'});
        $.each(form_data, function() {
            xdata.c('field', {type:this['type'], 'var':this['var']}).c('value').t(this['value']).up().up();
        });
        return xdata.nodeTree;
    },

    /*-------------------------------------------------------------------------------*/
    sendCommandCancel: function (req) {
        var to         = $(req).attr('from'),
            command    = $(req).find('command').eq(0),
            msg        = Gnosus.addCommandTextMessage(to, $(command).attr('node'), 'cancel', $(req).attr('id')), 
            cmd_attr   = {xmlns: Strophe.NS.COMMANDS, node:$(command).attr('node'), action:'cancel'};          
        if ($(command).attr('sessionid')) {
            cmd_attr['sessionid'] = $(command).attr('sessionid');
        }  
        var cmd_iq = $iq({to:to, type: 'set'}).c('command', cmd_attr);         
        GnosusXmpp.connection.sendIQ(cmd_iq, function(iq) {
            var type = $(iq).attr('type'),
                jid  = $(iq).attr('from');
            if (type == 'result') {
                $(document).trigger('command_cancel', Gnosus.addCommandTextMessage(to, $(command).attr('node'), 'canceled'), $(req).attr('id'));
            } else {
                $(document).trigger('command_error', jid);
            }
        });
        return msg;
    },  
    
    /*-------------------------------------------------------------------------------
    pubsub
    ---------------------------------------------------------------------------------*/
    sendGetSubscriptions: function(to) {
        var iq = $iq({to:to, type: 'get'}).c('pubsub', {xmlns:Strophe.NS.PUBSUB}).c('subscriptions');
        GnosusXmpp.connection.sendIQ(iq, function(iq) {
            var type = $(iq).attr('type');
            if (type == 'result') {
                $(iq).find('subscription').each(function() {
                    $(document).trigger('subscriptions_result', Gnosus.addSubscription(this));
                });
            } else {
                $(document).trigger('subscriptions_error',  $(iq).attr('from'));
            }
        });
    },

    /*-------------------------------------------------------------------------------*/
    sendCreatePubSubNode: function(for_jid, to, node) {
        var iq = $iq({to:to, type: 'set'})
            .c('pubsub', {xmlns:Strophe.NS.PUBSUB})
            .c('create', {node:node}).up().c('configure');
        GnosusXmpp.connection.sendIQ(iq, function(iq) {
            var type = $(iq).attr('type');
            if (type == 'result') {
                $(iq).find('subscription').each(function() {
                    $(document).trigger('create_pubsub_node_result', Gnosus.addSubscription(this));
                });
            } else {
                $(document).trigger('create_pubsub_node_error',  $(iq).attr('from'));
            }
        });
    },

    /*-------------------------------------------------------------------------------
    disco
    ---------------------------------------------------------------------------------*/
    sendGetDiscoInfo: function(for_jid, to, node, result, error) {
        var qattr = {xmlns:Strophe.NS.DISCO_INFO},
            iq = $iq({to:to, type: 'get'});
        if (node) {qattr['node'] = node;} 
        iq.c('query', qattr);  
        GnosusXmpp.connection.sendIQ(iq, function(iq) {
            var type = $(iq).attr('type');
            if (type == 'result') {
                var services = [];
                    features = []
                $(iq).find('identity').each(function() {
                    services.push(Gnosus.addService(for_jid, to, node, this));
                });
                $(iq).find('feature').each(function() {
                    features.push(Gnosus.addServiceFeature(for_jid, to, node, this));
                });
                if (result) {
                    result($(iq).attr('from'), node, services, features);
                } else {
                    $(document).trigger('disco_info_result', $(iq).attr('from'), node, services, features);
                }
            } else {
                if (error) {
                    error($(iq).attr('from'), node);
                } else {
                    $(document).trigger('disco_info_error', $(iq).attr('from'), node);
                }
            }
        });
    },

    /*-------------------------------------------------------------------------------*/
    sendGetDiscoItems: function(for_jid, to, node, result, error) {
        var qattr = {xmlns:Strophe.NS.DISCO_ITEMS},
            iq = $iq({to:to, type: 'get'});
        if (node) {qattr['node'] = node;}    
        iq.c('query', qattr);  
        GnosusXmpp.connection.sendIQ(iq, function(iq) {
            var type = $(iq).attr('type');
            if (type == 'result') {
                var items = [];
                $(iq).find('item').each(function() {
                    items.push(Gnosus.addServiceItem(for_jid, to, node, this));
                });
                if (result) {
                    result($(iq).attr('from'), node, items);
                } else {
                    $(document).trigger('disco_items_result', $(iq).attr('from'), node, items);
                }
            } else {
                if (error) {
                    error($(iq).attr('from'), node);
                } else {
                    $(document).trigger('disco_items_error', $(iq).attr('from'), node);
                }
            }
        });
    },
    
    /*-------------------------------------------------------------------------------*/
    sendPubSubServiceDisco: function(for_jid, service, done, not_found) {
        GnosusXmpp.sendGetDiscoItems(for_jid, service, null, function(serv, parent_node, items) {
            $.each(items, function() {
                GnosusXmpp.sendGetDiscoInfo(for_jid, this.jid, this.node, function(serv, parent_node, services, features) {
                    $.each(services, function() {
                        if (this.category == 'pubsub' && this.type =='service') {
                            GnosusXmpp.sendGetDiscoItems(for_jid, this.jid, GnosusXmpp.user_pubsub_root(for_jid), done, not_found);
                        }
                    });
                })
            });
        });
    },
}

/**********************************************************************************
roster
**********************************************************************************/
Strophe.addConnectionPlugin('roster', {

    /*-------------------------------------------------------------------------------*/
    init: function (connection) {
        this.connection = connection;
        Strophe.addNamespace('ROSTER', 'jabber:iq:roster');
    },
 
    /*-------------------------------------------------------------------------------*/
    statusChanged: function (status) {
        if (status === Strophe.Status.CONNECTED) {
            this.connection.addHandler(this.onRosterSet.bind(this), Strophe.NS.ROSTER, 'iq', 'set');
            this.connection.addHandler(this.onPresence.bind(this), null, 'presence');
            var roster_iq = $iq({type: 'get'}).c('query', {xmlns: Strophe.NS.ROSTER});
            this.connection.sendIQ(roster_iq, function(iq) {
                var type = $(iq).attr('type');
                if (type == 'result') {
                    $(iq).find('item').each(function () {
                        Gnosus.addContact($(this));
                    });
                    $(document).trigger('roster_init_result');
                } else {
                    $(document).trigger('roster_init_error');
                }
            });
            GnosusXmpp.initialPresence();
        } else if (status === Strophe.Status.DISCONNECTED) {
            Gnosus.contactOffline();
            $(document).trigger('roster_offline');
        }
    },
 
    /*-------------------------------------------------------------------------------*/
    onRosterSet: function (iq) {
        var that = this;
        $(iq).find('item').each(function () {
            var jid = $(this).attr('jid'),
                subscription = $(this).attr('subscription') || '';        
            if (subscription === 'remove') {
                $(document).trigger('roster_item_remove', Gnosus.removeContact($(this)));
            } else if (subscription === 'none') {
                that.updateContact($(this))
            } else if (subscription === 'from') {
                that.updateContact($(this))
            } else {
                $(document).trigger('roster_item_update', Gnosus.updateContact($(this)));
            }
        });
        this.connection.send($iq({type: 'result', id: $(iq).attr('id')}));
        return true;
    },
 
    /*-------------------------------------------------------------------------------*/
    updateContact: function(item) {
        var jid = item.attr('jid');
        if (!Gnosus.findContactByJid(jid)) {                
            $(document).trigger('roster_item_add', Gnosus.addContact(item));
        } else {
            $(document).trigger('roster_item_update', Gnosus.updateContact(item));
        }
    },

    /*-------------------------------------------------------------------------------*/
    onPresence: function (presence) {
        var from = $(presence).attr('from'),
            jid = Strophe.getBareJidFromJid(from),
            ptype = $(presence).attr('type') || 'available',
            contact = Gnosus.findContactByJid(jid);
        if (contact && ptype != 'error') {
            if (ptype === 'unavailable') {
                Gnosus.removeContactResource(presence);
                $(document).trigger("presence_unavailable", contact);
            } else if (ptype === 'subscribe') {
                GnosusXmpp.acceptSubscriptionRequest(jid);
            } else if (ptype === 'unsubscribed') {
                Gnosus.removeContactResources(jid);
                $(document).trigger("presence_unsubscribed", contact);
            } else {
                Gnosus.addContactResource(presence);
                $(document).trigger("presence", contact);
            }        
        } else if (ptype === 'subscribe') {
            $(document).trigger("presence_subscribe", jid);
        } else if (jid == Gnosus.account.jid) {
            Gnosus.addAccountResource(presence);
        }
        return true;
    }
});
 
/**********************************************************************************
messages
**********************************************************************************/
Strophe.addConnectionPlugin('messages', {

    /*-------------------------------------------------------------------------------*/
    init: function (connection) {
        this.connection = connection;
    },
 
    /*-------------------------------------------------------------------------------*/
    statusChanged: function (status) {
        if (status === Strophe.Status.CONNECTED) {
        	this.connection.addHandler(this.onMessage.bind(this), null, 'message'); 
        }
    },
 
    /*-------------------------------------------------------------------------------*/
    onMessage: function (msg) {
        var type = $(msg).attr('type');
        var body = $(msg).find('body').text();
        if (type == "chat" && body) {
    	    $(document).trigger("chat", Gnosus.addIncomingChatTextMessage(msg));            
	    }
        return true;
    }

});

/**********************************************************************************
disco
**********************************************************************************/
Strophe.addConnectionPlugin('disco', {

    /*-------------------------------------------------------------------------------*/
    init: function (connection) {
        this.connection = connection;
    },
 
    /*-------------------------------------------------------------------------------*/
    statusChanged: function (status) {
        if (status === Strophe.Status.CONNECTED) {
        	this.connection.addHandler(this.onDisoItemsGet.bind(this), Strophe.NS.DISCO_ITEMS, 'iq', 'get'); 
        	this.connection.addHandler(this.onDisoInfoGet.bind(this), Strophe.NS.DISCO_INFO, 'iq', 'get'); 
        }
    },
 
    /*-------------------------------------------------------------------------------*/
    onDisoItemsGet: function (iq) {
        var qattr = {xmlns:Strophe.NS.DISCO_ITEMS};
            resp = $iq({type: 'error', to:$(iq).attr('from'), id:$(iq).attr('id')})
                .c('query', qattr).up()
                .c('error', {type: 'cancel'})
                .c('service-unavailable', {xmlns: Strophe.NS.STANZAS});
        if ($(iq).find('query').attr('node')) {qattr['node'] = $(iq).find('query').attr('node');}
        this.connection.send(resp);
        return true;
    },

    /*-------------------------------------------------------------------------------*/
    onDisoInfoGet: function (iq) {
        var qattr = {xmlns:Strophe.NS.DISCO_INFO},
            resp = $iq({type: 'error', to:$(iq).attr('from'), id:$(iq).attr('id')})
                .c('query', qattr).up()
                .c('error', {type: 'cancel'})
                .c('service-unavailable', {xmlns: Strophe.NS.STANZAS});
        if ($(iq).find('query').attr('node')) {qattr['node'] = $(iq).find('query').attr('node');}
        this.connection.send(resp);
        return true;
    }

});

/**********************************************************************************
commands
**********************************************************************************/
Strophe.addConnectionPlugin('commands', {

    /*-------------------------------------------------------------------------------*/
    init: function (connection) {
        this.connection = connection;
        Strophe.addNamespace('COMMANDS', 'http://jabber.org/protocol/commands');
        Strophe.addNamespace('XDATA', 'jabber:x:data');
    },
 
    /*-------------------------------------------------------------------------------*/
    statusChanged: function (status) {
        if (status === Strophe.Status.CONNECTED) {
        	this.connection.addHandler(this.onCommandSet.bind(this), Strophe.NS.COMMANDS, 'iq', 'set'); 
        }
    },
 
    /*-------------------------------------------------------------------------------*/
    onCommandSet: function (iq) {
        var resp = $iq({type: 'error', to:$(iq).attr('from'), id:$(iq).attr('id')})
            .c('command', {xmlns:Strophe.NS.COMMANDS, node:$(iq).find('command').attr('node'), action:'execute'}).up()
            .c('error', {type: 'modify'})
            .c('bad-request', {xmlns: Strophe.NS.STANZAS}).up()
            .c('bad-action', {xmlns: Strophe.NS.COMMANDS});
        this.connection.send(resp);
        return true;
    },

});

/**********************************************************************************
pubsub
**********************************************************************************/
Strophe.addConnectionPlugin('pubsub', {

    /*-------------------------------------------------------------------------------*/
    init: function (connection) {
        this.connection = connection;
    },
 
    /*-------------------------------------------------------------------------------*/
    statusChanged: function (status) {
        if (status === Strophe.Status.CONNECTED) {
            Strophe.addNamespace('PUBSUB', 'http://jabber.org/protocol/pubsub');
        }
    },
});
