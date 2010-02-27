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
	Gnosus.accounts[Strophe.getBareJidFromJid(jid)] = new Account(service, jid, password);
	Gnosus.account_jid = Strophe.getBareJidFromJid(jid);
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
    account_jid: null,
    accounts: {},
    messages: [],

    /*-------------------------------------------------------------------------------
    contacts
    ---------------------------------------------------------------------------------*/
    addContact: function(item) {
        var groups = item.find('group').map(function (g, i) {g.text();}),
            jid = item.attr('jid');
        Gnosus.accounts[jid] = new Contact(item.attr('jid'), item.attr('name'), item.attr('ask'), item.attr('subscription'), groups);
        return Gnosus.accounts[jid];
    },
    findAllContacts: function() {
        var contacts = [];
        for (var c in Gnosus.accounts) {
            if (Gnosus.account().jid != Gnosus.accounts[c].jid) {contacts.push(Gnosus.accounts[c]);}
        }
        return contacts;
    },
    updateContact: function(item) {
        var contact = Gnosus.accounts[item.attr('jid')];
        if (contact) {
            contact.groups = [];
            contact.setAttributes(item);
        }
        return contact;
    },
    removeContact: function(item) {
        jid = item.attr('jid');
        var contact = Gnosus.accounts[jid];
        if (contact) {
            delete(Gnosus.accounts[jid]);
        }
        return contact;
    },

    /*-------------------------------------------------------------------------------
    accounts
    ---------------------------------------------------------------------------------*/
    account: function() {
        return Gnosus.findAccountByName(Gnosus.account_jid);
    },
    findAccountByJid: function(jid) {
        return Gnosus.accounts[jid];
    },
    findAccountByName: function(name) {
        var result = null;
        for (var jid in Gnosus.accounts) {
            if (Gnosus.accounts[jid].name == name) {
                result = Gnosus.accounts[jid];
                break;
            }
        }
        return result;
    },

    /*-------------------------------------------------------------------------------
    resources
    ---------------------------------------------------------------------------------*/
    addResource: function(presence) {
        var from = $(presence).attr('from');
        var bare_jid = Strophe.getBareJidFromJid(from);
        if (Gnosus.accounts[bare_jid]) {
            var resource = new Resource(from, $(presence).find('show').text(), $(presence).find('status').text());
            Gnosus.accounts[bare_jid].addResource(resource);
        }
    },
    removeResource: function(presence) {
        var from = $(presence).attr('from');
        var bare_jid = Strophe.getBareJidFromJid(from);
        if (Gnosus.accounts[bare_jid]) {
            Gnosus.accounts[bare_jid].removeResource(from);
        }
    },
    removeResources: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        if (Gnosus.accounts[bare_jid]) {
            Gnosus.accounts[bare_jid].removeResources();
        }
    },
    findAllResources: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        return Gnosus.accounts[bare_jid] ? Gnosus.accounts[bare_jid].resources : null;
    },
    findResource: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        return Gnosus.accounts[bare_jid] ? Gnosus.accounts[bare_jid].resources[jid] : null;
    },
    goOffline: function() {
        $.each(Gnosus.accounts, function(j,c) {
            c.resources = {};
        });
    },

    /*-------------------------------------------------------------------------------
    messages
    ---------------------------------------------------------------------------------*/
    addIncomingChatTextMessage: function(msg) {
        var msg_model = new Message($(msg).attr('to'), $(msg).attr('from'), $(msg).find('body').text(), 'chat', 'text');
        Gnosus.messages.unshift(msg_model);  
        return msg_model;      
    },
    addOutgoingChatTextMessage: function(to, body) {
        var msg_model = new Message(to, Gnosus.account().fullJid(), body, 'chat', 'text');
        Gnosus.messages.unshift(msg_model);  
        return msg_model;      
    },
    addHeadlineMessages: function(msg) {
        var items = $(msg).find('items').eq(0),
            item  = $(msg).find('item'),
            node  = $(items).attr('node'),
            from  = GnosusXmpp.userPubsubRootToJid(node),
            msgs  = [];
            $(item).each(function() {
                var msg_model = null,
                    id        = $(this).attr('id'),
                    xdata     = $(this).find('x').eq(0),
                    entry     = $(this).find('entry');
                if (xdata.attr('xmlns') == Strophe.NS.XDATA) {
                    msg_model = new Message(Gnosus.account().jid, from, xdata, 'headline', 'x', node, null, id);
                } else if (entry.attr('xmlns') == Strophe.NS.ENTRY) {
                    msg_model = new Message(Gnosus.account().jid, from, entry.find('title').text(), 'headline', 'entry', node, null, id);
                }
                if (msg_model) { 
                    Gnosus.messages.unshift(msg_model);
                    msgs.unshift(msg_model); 
                }
            })    
        return msgs;
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
    findMessagesByNode: function(node) {
        return $.grep(Gnosus.messages, function(m) {return (m.node == node)});         
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
        var resource = Gnosus.findResource(jid);
        if (resource) {resource.addCommand(node, name);}
    },
    initCommands: function(jid) {
        var resource = Gnosus.findResource(jid);
        if (resource) {resource.initCommands();}
    },
    areCommandsAvailable: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        var has_commands = false;
        if (bare_jid == jid) {
            var resources = Gnosus.findAllResources(jid);
            for (var rjid in resources) {
                if (!resources[rjid].commands) {
                    has_commands = false;
                    break;
                } else {
                    has_commands = true;
                }
            }
        } else {
            var resource = Gnosus.findResource(jid);
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
            $.each(Gnosus.findAllResources(jid), function() {
                addCommandHash(this.commands);
            });
        } else {
            var resource = Gnosus.findResource(jid);
            if (resource) {addCommandHash(resource.commands);}
        }
        return all_commands;
    },
    addCommandXDataMessage: function(iq) {
        var cmd  = $(iq).find('command').eq(0),
            data = $(iq).find('x').eq(0),
            from = $(iq).attr('from') || Gnosus.account().fullJid();
        var msg_model = new Message($(iq).attr('to'), from, data, 'x', 'command', $(cmd).attr('node'), $(iq).attr('id'));
        Gnosus.messages.unshift(msg_model);  
        return msg_model;      
    },
    addCommandTextMessage: function(to, node, text, id) {
        var msg_model = new Message(to, Gnosus.account().fullJid(), text, 'text', 'command', node, id);
        Gnosus.messages.unshift(msg_model);  
        return msg_model;      
    },
    /*-------------------------------------------------------------------------------
    subscription
    ---------------------------------------------------------------------------------*/
    addSubscription: function(sub, serv) {
        return Gnosus.addSubscribed(sub, serv, $(sub).attr('node'));
    },
    addSubscribed: function(sub, serv, node) {
        var sub_model = new Subscription(node, serv, $(sub).attr('subscription'), $(sub).attr('subid'));
        Gnosus.account().subscriptions.push(sub_model);
        return sub_model;
    },
    findSubscriptionsByNodeAndSubscription: function(node, sub) {
        return $.grep(Gnosus.account().subscriptions, function(s) {
            return s.node == node && s.subscription == sub;
        });
        return Gnosus.account().subscriptions[node];
    },
    findSubscriptionById: function(id) {
        return $.grep(Gnosus.account().subscriptions, function(s) {
            return s.node == node;
        });
        return Gnosus.account().subscriptions[node];
    },
    removeSubscription: function(subid) {
        return Gnosus.account().removeSubscription(subid);
    },
    removeSubscriptionsByService: function(serv) {
        var subs = $.grep(Gnosus.account().subscriptions, function(a) {
                       return a.service == serv; 
                   });
        $.each(subs, function() {
            Gnosus.account().removeSubscription(this.subid);
        });       
        return subs;
    },
    /*-------------------------------------------------------------------------------
    disco
    ---------------------------------------------------------------------------------*/
    addService: function(jid, service, node, serv) {
        var service_model = new Service(service, node, $(serv).attr('name'), $(serv).attr('category'), $(serv).attr('type')),
            contact       = Gnosus.findAccountByJid(jid);
        if (contact) {contact.services.push(service_model);}
        return service_model;
    },
    addServiceItem: function(jid, service, parent_node, item) {
        var item_model = new ServiceItem(service, parent_node, $(item).attr('jid'), $(item).attr('node'), $(item).attr('name')),
            contact    = Gnosus.findAccountByJid(jid);
        if (contact) {contact.service_items.push(item_model);}
        return item_model;
    },
    addServiceFeature: function(jid, service, node, feature) {
        var feature_model = new ServiceFeature(service, node, $(feature).attr('var')),
            contact       = Gnosus.findAccountByJid(jid);
        if (contact) {contact.service_features.push(feature_model);}
        return feature_model;
    },
    findPubSubNodesByJid: function(jid) {
        var items   = [],
            contact = Gnosus.findAccountByJid(jid);
        if (contact) {
            items = $.grep(contact.service_items, function(s) {
                       if (s.node) {
                           return s.node.match(new RegExp(GnosusXmpp.userPubsubRoot(jid), 'g')); 
                        } else {
                            return false;
                        }              
                    });
        }
        return items;
    },
    findServiceItemByJidAndNode: function(jid, node) {
        var item    = null,
            contact = Gnosus.findAccountByJid(jid);
        if (contact) {
            for (var s in contact.service_items) {
                if (contact.service_items[s].node == node) {
                    item = contact.service_items[s];
                    break;
                }              
            }
        }
        return item;
    },
    findPubSubServiceByJid: function(jid) {
        var service   = null,
            contact = Gnosus.findAccountByJid(jid);
        if (contact) {
            for (var s in contact.services) {
                if (contact.services[s].category == 'pubsub' && contact.services[s].type =='service') {
                    service = contact.services[s];
                    break;
                }
            }
        }
        return service;
    },
    initServiceDisco: function(jid) {
        var acct = Gnosus.findAccountByJid(jid);
        if (acct) {
            acct.services = [];
            acct.service_items = [];
            acct.service_features = [];
        }
    },
}

/**********************************************************************************
models
**********************************************************************************/
function Account(service, jid, password) {
    this.service = service;
    this.jid = Strophe.getBareJidFromJid(jid);
    this.name = this.jid;
    this.resource = Strophe.getResourceFromJid(jid) || 'gnos.us';
    this.password = password;
    this.resources = {};
    this.services = [];
    this.service_items = [];
    this.service_features = [];
    this.subscriptions = [];
    this.publications = {};
}

Account.prototype = {
    show: function() {
        return 'online';
    },
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
    },
    removeSubscription: function(subid) {
        var delete_indx = null,
            sub         = null;
        for(var i in Gnosus.account().subscriptions) {
            if (Gnosus.account().subscriptions[i].subid == subid) {
                delete_indx = i;
                break;
            }
        }
        if (delete_indx) {sub = this.subscriptions.splice(delete_indx, 1);}
        return sub;
    },
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
function Subscription(node, serv, subscription, subid) {
  this.node = node;
  this.service = serv;
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
    this.name = name || node;    
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
    pubsubRoot: function(jid) {
        return '/home/'+Strophe.getDomainFromJid(jid);
    },

    /*-------------------------------------------------------------------------------*/
    userPubsubRoot: function(jid) {
        return GnosusXmpp.pubsubRoot(jid)+'/'+Strophe.getNodeFromJid(jid);
    },

    /*-------------------------------------------------------------------------------*/
    userPubsubRootToJid: function(node) {
        var node_comps = node.split('/');
        return node_comps[3]+'@'+node_comps[2];
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
        GnosusXmpp.connection.sendIQ(iq, 
            function(iq) {
                GnosusXmpp.subscriptionRequest(jid);
                $(document).trigger('roster_item_add_result', iq);
            },
            function(iq) {
                $(document).trigger('roster_item_add_error', iq);
            }
        );
    },
    
    /*-------------------------------------------------------------------------------*/
    removeContact: function (jid) {
        var iq = $iq({type: 'set'}).c('query', {xmlns: Strophe.NS.ROSTER}).c('item', {jid: jid, subscription: 'remove'});
        GnosusXmpp.connection.sendIQ(iq, 
            function(iq) {
                GnosusXmpp.connection.send($pres({to: jid, type: "unsubscribe"}));
                $(document).trigger('roster_item_remove_result', iq);
            },
            function(iq) {
                $(document).trigger('roster_item_remove_error', iq);
            }
        );
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
    subscriptionRequest: function (jid) {
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
    getCommandList: function (to) {
        var cmd_iq = $iq({to:to, type:'get'}).c('query', {xmlns:Strophe.NS.DISCO_ITEMS, node:'http://jabber.org/protocol/commands'});
        GnosusXmpp.connection.sendIQ(cmd_iq, 
            function(iq) {
                var jid = $(iq).attr('from');
                Gnosus.initCommands(jid);
                $(iq).find('item').each(function () {
                    Gnosus.addCommand(jid, $(this).attr('node'), $(this).attr('name'));
                });
                $(document).trigger('command_list_result', jid);
            },
            function(iq) {
                $(document).trigger('command_list_error', jid);
            }
        );
    },  
    
    /*-------------------------------------------------------------------------------*/
    resultCommandList: function (iq) {
        var resp = $iq({type: 'result', to:$(iq).attr('from'), id:$(iq).attr('id')})
            .c('query', {xmlns:Strophe.NS.DISCO_ITEMS, node:Strophe.NS.COMMANDS});
        this.connection.send(resp);        
    },

    /*-------------------------------------------------------------------------------*/
    errorCommandBadRequest: function(iq) {
        var resp = $iq({type: 'error', to:$(iq).attr('from'), id:$(iq).attr('id')})
            .c('command', {xmlns:Strophe.NS.COMMANDS, node:$(iq).find('command').attr('node'), action:'execute'}).up()
            .c('error', {type: 'modify'})
            .c('bad-request', {xmlns: Strophe.NS.STANZAS}).up()
            .c('bad-action', {xmlns: Strophe.NS.COMMANDS});
        this.connection.send(resp);
    },
    
    /*-------------------------------------------------------------------------------*/
    setCommand: function (args) {
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
        GnosusXmpp.connection.sendIQ(cmd_iq, 
            function(iq) {
                var jid  = $(iq).attr('from'),
                    x   = $(iq).find('x').eq(0),
                    x_type = 'result';
                if (x) {x_type = $(x).attr('type')}
                if (x_type == 'form') {
                    $(document).trigger('command_form', iq);
                } else {
                    $(document).trigger('command_result', Gnosus.addCommandXDataMessage(iq));
                }
            },
            function(iq) {
                $(document).trigger('command_error', jid);
            }
        );
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
    setCommandCancel: function (req) {
        var to         = $(req).attr('from'),
            command    = $(req).find('command').eq(0),
            msg        = Gnosus.addCommandTextMessage(to, $(command).attr('node'), 'cancel', $(req).attr('id')), 
            cmd_attr   = {xmlns: Strophe.NS.COMMANDS, node:$(command).attr('node'), action:'cancel'};          
        if ($(command).attr('sessionid')) {
            cmd_attr['sessionid'] = $(command).attr('sessionid');
        }  
        var cmd_iq = $iq({to:to, type: 'set'}).c('command', cmd_attr);         
        GnosusXmpp.connection.sendIQ(cmd_iq, 
            function(iq) {
                $(document).trigger('command_cancel', Gnosus.addCommandTextMessage(to, $(command).attr('node'), 'canceled'), $(req).attr('id'));
            },
            function(iq) {
                $(document).trigger('command_error', jid);
            }
        );
        return msg;
    },  
    
    /*-------------------------------------------------------------------------------
    pubsub
    ---------------------------------------------------------------------------------*/
    getSubscriptions: function(to, result, error) {
        var iq = $iq({to:to, type: 'get'}).c('pubsub', {xmlns:Strophe.NS.PUBSUB}).c('subscriptions');
        GnosusXmpp.connection.sendIQ(iq, 
            function(iq) {
                var subscriptions =[];
                $(iq).find('subscription').each(function() {subscriptions.push(Gnosus.addSubscription(this, to));});
                if (result) {
                    result(subscriptions)
                } else {
                    $(document).trigger('subscriptions_result', suscriptions);
                }
            },
            function(iq) {
                if (error) {
                    error($(iq).attr('from'))
                } else {
                    $(document).trigger('subscriptions_error',  $(iq).attr('from'));
                }
            }
        );
    },

    /*-------------------------------------------------------------------------------*/
    setSubscribe: function(service, node) {
        var iq = $iq({to:service, type: 'set'}).c('pubsub', {xmlns:Strophe.NS.PUBSUB})
            .c('subscribe', {node:node, jid:Gnosus.account().jid});
        GnosusXmpp.connection.sendIQ(iq, 
            function(iq) {
                var subscription = $(iq).find('subscription').eq(0);
                $(document).trigger('subscribe_result', Gnosus.addSubscribed(subscription, service, node));
            },
            function(iq) {
                $(document).trigger('subscribe_error',  [service, node]);
            }
        );
    },

    /*-------------------------------------------------------------------------------*/
    setUnsubscribe: function(service, node, subid) {
        var iq  = $iq({to:service, type: 'set'}).c('pubsub', {xmlns:Strophe.NS.PUBSUB})
            .c('unsubscribe', {node:node, jid:Gnosus.account().jid, subid:subid});
        GnosusXmpp.connection.sendIQ(iq, 
            function(iq) {
                $(document).trigger('unsubscribe_result', Gnosus.removeSubscription(subid));
            },
            function(iq) {
                $(document).trigger('unsubscribe_error', [service, node]);
            }
        );
    },

    /*-------------------------------------------------------------------------------*/
    createPubSubNode: function(for_jid, to, node) {
        var iq = $iq({to:to, type: 'set'})
            .c('pubsub', {xmlns:Strophe.NS.PUBSUB})
            .c('create', {node:node}).up().c('configure');
        GnosusXmpp.connection.sendIQ(iq, 
            function(iq) {
                $(iq).find('subscription').each(function() {
                    $(document).trigger('create_pubsub_node_result', Gnosus.addSubscription(this));
                });
            },
            function(iq) {
                $(document).trigger('create_pubsub_node_error',  $(iq).attr('from'));
            }
        );
    },

    /*-------------------------------------------------------------------------------
    disco
    ---------------------------------------------------------------------------------*/
    getDiscoInfo: function(for_jid, to, node, result, error) {
        var qattr = {xmlns:Strophe.NS.DISCO_INFO},
            iq = $iq({to:to, type: 'get'});
        if (node) {qattr['node'] = node;} 
        iq.c('query', qattr);  
        GnosusXmpp.connection.sendIQ(iq, 
            function(iq) {
                var services = [],
                    features = [];
                $(iq).find('identity').each(function() {
                    services.push(Gnosus.addService(for_jid, to, node, this));
                });
                $(iq).find('feature').each(function() {
                    features.push(Gnosus.addServiceFeature(for_jid, to, node, this));
                });
                if (result) {
                    result($(iq).attr('from'), node, services, features);
                } else {
                    $(document).trigger('disco_info_result', [$(iq).attr('from'), node, services, features]);
                }
            },
            function(iq) {
                if (error) {
                    error($(iq).attr('from'), node);
                } else {
                    $(document).trigger('disco_info_error', [$(iq).attr('from'), node]);
                }
            }
        );
    },

    /*-------------------------------------------------------------------------------*/
    getDiscoItems: function(for_jid, to, node, result, error) {
        var qattr = {xmlns:Strophe.NS.DISCO_ITEMS},
            iq = $iq({to:to, type: 'get'});
        if (node) {qattr['node'] = node;}    
        iq.c('query', qattr);  
        GnosusXmpp.connection.sendIQ(iq, 
            function(iq) {
                var items = [];
                $(iq).find('item').each(function() {
                    items.push(Gnosus.addServiceItem(for_jid, to, node, this));
                });
                if (result) {
                    result($(iq).attr('from'), node, items);
                } else {
                    $(document).trigger('disco_items_result', [$(iq).attr('from'), node, items]);
                }
            },
            function(iq) {
                if (error) {
                    error($(iq).attr('from'), node);
                } else {
                    $(document).trigger('disco_items_error', [$(iq).attr('from'), node]);
                }
            }
        );
    },
    
    /*-------------------------------------------------------------------------------*/
    getPubSubServiceDisco: function(for_jid, service, done, node_not_found) {
        Gnosus.initServiceDisco(for_jid);
        GnosusXmpp.getDiscoItems(for_jid, service, null, function(serv, parent_node, items) {
            $.each(items, function() {
                GnosusXmpp.getDiscoInfo(for_jid, this.jid, this.node, function(serv, parent_node, services, features) {
                    $.each(services, function() {
                        if (this.category == 'pubsub' && this.type =='service') {
                            Gnosus.removeSubscriptionsByService(this.jid);
                            GnosusXmpp.getDiscoItems(for_jid, this.jid, GnosusXmpp.userPubsubRoot(for_jid), function() {
                                GnosusXmpp.getSubscriptions(Gnosus.findPubSubServiceByJid(for_jid).jid, done)
                            }, node_not_found);
                        }
                    });
                });
            });
        });
    },
    
    /*-------------------------------------------------------------------------------*/
    errorDiscoItems: function(iq, err) {
        var qattr = {xmlns:Strophe.NS.DISCO_ITEMS};
        if ($(iq).find('query').attr('node')) {qattr['node'] = $(iq).find('query').attr('node');}
        var resp = $iq({type: 'error', to:$(iq).attr('from'), id:$(iq).attr('id')})
            .c('query', qattr).up().c('error', {type: 'cancel'}).c(err, {xmlns: Strophe.NS.STANZAS});
        this.connection.send(resp);            
    },

    /*-------------------------------------------------------------------------------*/
    errorDiscoInfo: function(iq, err) {
        var qattr = {xmlns:Strophe.NS.DISCO_ITEMS};
        if ($(iq).find('query').attr('node')) {qattr['node'] = $(iq).find('query').attr('node');}
        var resp = $iq({type: 'error', to:$(iq).attr('from'), id:$(iq).attr('id')})
            .c('query', qattr).up().c('error', {type: 'cancel'}).c(err, {xmlns: Strophe.NS.STANZAS});
        this.connection.send(resp);            
    }
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
            this.connection.sendIQ(roster_iq, 
                function(iq) {
                    $(iq).find('item').each(function () {
                        Gnosus.addContact($(this));
                    });
                    $(document).trigger('roster_init_result');
                },
                function(iq) {
                    $(document).trigger('roster_init_error');
                }
            );
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
        if (!Gnosus.findAccountByJid(jid)) {                
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
            contact = Gnosus.findAccountByJid(jid);
        if (contact && ptype != 'error') {
            if (ptype === 'unavailable') {
                Gnosus.removeResource(presence);
                $(document).trigger("presence_unavailable", contact);
            } else if (ptype === 'subscribe') {
                GnosusXmpp.acceptSubscriptionRequest(jid);
            } else if (ptype === 'unsubscribed') {
                Gnosus.removeResources(jid);
                $(document).trigger("presence_unsubscribed", contact);
            } else {
                Gnosus.addResource(presence);
                $(document).trigger("presence", contact);
            }        
        } else if (ptype === 'subscribe') {
            $(document).trigger("presence_subscribe", jid);
        } else if (jid == Gnosus.account().jid) {
            Gnosus.addResource(presence);
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
    	    $(document).trigger('chat', Gnosus.addIncomingChatTextMessage(msg));            
	    } else if (type =='headline') {
	        $(document).trigger('headline', [Gnosus.addHeadlineMessages(msg)])
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
        var node  = $(iq).find('query').attr('node');
        if (node == Strophe.NS.COMMANDS) {
            GnosusXmpp.resultCommandList(iq);
        } else {
            GnosusXmpp.errorDiscoItems(iq, 'service-unavailable');
        }
        return true;
    },

    /*-------------------------------------------------------------------------------*/
    onDisoInfoGet: function (iq) {
        GnosusXmpp.errorDiscoInfo(iq, 'service-unavailable');
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
        GnosusXmpp.errorCommandBadRequest(iq);
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
            Strophe.addNamespace('ENTRY', 'http://www.w3.org/2005/Atom');
        }
    },
});
