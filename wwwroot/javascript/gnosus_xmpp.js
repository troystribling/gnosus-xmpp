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
	Gnosus.accounts[Strophe.getBareJidFromJid(jid)] = new Account(service, jid, password);
	Gnosus.account_jid = Strophe.getBareJidFromJid(jid);
	Gnosus.account_resource = Strophe.getResourceFromJid(jid);
    var conn = new Strophe.Connection(service);
    conn.rawInput = rawInput;
    conn.rawOutput = rawOutput;
	GnosusXmpp.connection = conn;
	conn.connect(jid, password, onConnect);
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
    account_resource: null,
    accounts: {},
    messages: [],

    /*-------------------------------------------------------------------------------
    contacts
    ---------------------------------------------------------------------------------*/
    addContact: function(item) {
        var groups = $.map(item.find('group'), function (g, i) {return $(g).text();}),
            jid = item.attr('jid');
        if (!this.accounts[jid]) {   
            this.accounts[jid] = new Contact(item.attr('jid'), item.attr('name'), item.attr('ask'), item.attr('subscription'), groups);
        }
        return this.accounts[jid];
    },
    findAllContacts: function() {
        var contacts = [];
        for (var c in this.accounts) {
            if (this.account_jid != this.accounts[c].jid) {contacts.push(this.accounts[c]);}
        }
        return contacts;
    },
    updateContact: function(item) {
        var jid = item.attr('jid'),
            contact = this.accounts[jid];
        if (contact && jid != this.account_jid) {
            contact.groups = [];
            contact.setAttributes(item);
        }
        return contact;
    },
    removeContact: function(item) {
        var jid = item.attr('jid'),
            contact = this.accounts[jid];
        if (contact && jid != this.account_jid) {
            delete(this.accounts[jid]);
        }
        return contact;
    },

    /*-------------------------------------------------------------------------------
    accounts
    ---------------------------------------------------------------------------------*/
    account: function() {
        return this.findAccountByName(this.account_jid);
    },
    accountFullJid: function() {
        return this.account_jid+'/'+this.account_resource;
    },
    findAccountByJid: function(jid) {
        return this.accounts[jid];
    },
    findAccountByName: function(name) {
        var result = null;
        for (var jid in this.accounts) {
            if (this.accounts[jid].name == name) {
                result = this.accounts[jid];
                break;
            }
        }
        return result;
    },

    /*-------------------------------------------------------------------------------
    resources
    ---------------------------------------------------------------------------------*/
    addResource: function(presence) {
        var from     = $(presence).attr('from'),
            resource = null,
            bare_jid = Strophe.getBareJidFromJid(from);
        if (this.accounts[bare_jid] && bare_jid != from) {
            resource = this.findResourceByJid(from);
            if (resource) {
                resource.updateWithPresence(presence);
            } else {
                resource = new Resource(from, $(presence).find('show').text(), $(presence).find('status').text());
                this.accounts[bare_jid].addResource(resource);
            }
        }
        return resource;
    },
    updateWithClientVersion: function(version) {
        var from     = $(version).attr('from'),
            resource = null,
            bare_jid = Strophe.getBareJidFromJid(from);
        if (this.accounts[bare_jid]) {
            resource = this.findResourceByJid(from);
            if (resource) {
                resource.updateWithClientVersion(version);
            }
        }
        return resource;
    },
    removeResource: function(presence) {
        var from     = $(presence).attr('from'),
            resource = null,
            bare_jid = Strophe.getBareJidFromJid(from);
        if (this.accounts[bare_jid]) {
            resource = this.accounts[bare_jid].removeResource(from);
        }
        return resource;
    },
    removeResources: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        if (this.accounts[bare_jid]) {
            this.accounts[bare_jid].removeResources();
        }
    },
    findAllResourcesByJid: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        return this.accounts[bare_jid] ? this.accounts[bare_jid].resources : null;
    },
    findResourceByJid: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        return this.accounts[bare_jid] ? this.accounts[bare_jid].resources[jid] : null;
    },
    goOffline: function() {
        $.each(this.accounts, function(j,c) {
            c.resources = {};
        });
    },

    /*-------------------------------------------------------------------------------
    messages
    ---------------------------------------------------------------------------------*/
    addIncomingChatTextMessage: function(msg) {
        var msg_model = new Message($(msg).attr('to'), $(msg).attr('from'), $(msg).find('body').text(), 'chat', 'text');
        this.messages.unshift(msg_model);  
        return msg_model;      
    },
    addOutgoingChatTextMessage: function(to, body) {
        var msg_model = new Message(to, this.account().fullJid(), body, 'chat', 'text');
        this.messages.unshift(msg_model);  
        return msg_model;      
    },
    addOutgoingEntryMessage: function(node, entry) {
        var msg_model = new Message(this.findPubSubServiceByJid(this.account_jid).jid, this.account().fullJid(), entry, 'headline', 'entry', node);
        this.messages.unshift(msg_model);  
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
                    msg_model = new Message(Gnosus.account_jid, from, xdata, 'headline', 'x', node, null, id);
                } else if (entry.attr('xmlns') == Strophe.NS.ENTRY) {
                    msg_model = new Message(Gnosus.account_jid, from, entry.find('title').text(), 'headline', 'entry', node, null, id);
                }
                if (msg_model) { 
                    Gnosus.messages.unshift(msg_model);
                    msgs.unshift(msg_model); 
                }
            })    
        return msgs;
    },
    addCommandXDataMessage: function(iq) {
        var cmd  = $(iq).find('command').eq(0),
            data = $(iq).find('x').eq(0),
            from = $(iq).attr('from') || this.account().fullJid();
        var msg_model = new Message($(iq).attr('to'), from, data, 'x', 'command', $(cmd).attr('node'), $(iq).attr('id'));
        this.messages.unshift(msg_model);  
        return msg_model;      
    },
    addCommandTextMessage: function(to, node, text, id) {
        var msg_model = new Message(to, this.account().fullJid(), text, 'text', 'command', node, id);
        this.messages.unshift(msg_model);  
        return msg_model;      
    },
    findAllContactMessages: function() {
        var acct = this.account();
        if (acct) {
            var jid_rexp = new RegExp(acct.jid, 'g');
            return $.grep(this.messages, function(m) {
                       return ((!m.from.match(jid_rexp) || !m.to.match(jid_rexp)))
                   });   
        } else {
            return [];
        }
    },
    findAllAccountMessages: function() {
        var jid_rexp = new RegExp(this.account_jid, 'g');
        return $.grep(this.messages, function(m) {
                   return ((m.from.match(jid_rexp) && m.to.match(jid_rexp)))
               });   
    },
    findAllSubscribedMessages: function() {
        var jid_rexp = new RegExp(this.account_jid, 'g');
        return $.grep(this.messages, function(m) {
                   return ((!m.from.match(jid_rexp) || m.to.match(jid_rexp)) && m.type == 'headline')
               });   
    },
    findAllPublishedMessages: function() {
        var jid_rexp = new RegExp(this.account_jid, 'g');
        return $.grep(this.messages, function(m) {
                   return (m.from.match(jid_rexp) && m.type == 'headline')
               });   
    },
    findMessagesByJidAndType: function(jid, type) {
        var jid_rexp = new RegExp(jid, 'g');
        return $.grep(this.messages, function(m) {
                   return ((m.from.match(jid_rexp) || m.to.match(jid_rexp)) && m.type == type)
               });   
    },
    findMessagesByNode: function(node) {
        return $.grep(this.messages, function(m) {return (m.node == node);});         
    },
    findMessagesByJidAndContentType: function(jid, content_type) {
        var jid_rexp = new RegExp(jid, 'g');
        return $.grep(this.messages, function(m) {
                   return ((m.from.match(jid_rexp) || m.to.match(jid_rexp)) && m.content_type == content_type)
               });   
    },
    /*-------------------------------------------------------------------------------
    commands
    ---------------------------------------------------------------------------------*/
    addCommand: function(jid, node, name) {
        var resource = this.findResourceByJid(jid);
        if (resource) {resource.addCommand(node, name);}
    },
    initCommands: function(jid) {
        var resource = this.findResourceByJid(jid);
        if (resource) {resource.initCommands();}
    },
    areCommandsAvailable: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        var has_commands = false;
        if (bare_jid == jid) {
            var resources = this.findAllResourcesByJid(jid);
            for (var rjid in resources) {
                if (!resources[rjid].commands) {
                    has_commands = false;
                    break;
                } else {
                    has_commands = true;
                }
            }
        } else {
            var resource = this.findResourceByJid(jid);
            if (resource) {
                if (resource.commands) {
                    has_commands = true;
                }
            }
        }
        return has_commands;
    },
    findAllCommandsByJid: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        var all_commands = {};
        var addCommandHash = function (cmds) {
            if (cmds) {
                $.each(cmds, function() {
                    if (!all_commands[this.name]) {
                        all_commands[this.name] = [];
                    }
                    all_commands[this.name].push($(this));
                });
            }
        };
        if (bare_jid == jid) {
            $.each(this.findAllResourcesByJid(jid), function() {
                addCommandHash(this.commands);
            });
        } else {
            var resource = this.findResourceByJid(jid);
            if (resource) {addCommandHash(resource.commands);}
        }
        return all_commands;
    },
    findCommandByJidAndName: function(jid, name) {
        var resource = this.findResourceByJid(jid),
            cmd      = null;
        for (var i in resource.commands) {
            var test_cmd = resource.commands[i];
            if (test_cmd.name == name) {
                cmd = test_cmd;
                break;                
            }
        }
        return cmd;
    },
    /*-------------------------------------------------------------------------------
    subscription
    ---------------------------------------------------------------------------------*/
    addSubscription: function(sub, serv) {
        return this.addSubscribed(sub, serv, $(sub).attr('node'));
    },
    addSubscribed: function(sub, serv, node) {
        var sub_model = new Subscription(node, serv, $(sub).attr('subscription'), $(sub).attr('subid'));
        this.account().subscriptions.push(sub_model);
        return sub_model;
    },
    addPubSubNode: function(jid, service, parent_node, node) {
        var item_model = new ServiceItem(service, parent_node, service, node),
            acct       = this.findAccountByJid(jid);
        if (acct) {acct.service_items.push(item_model);}
        return item_model;
    },
    removeSubscription: function(subid) {
        return this.account().removeSubscription(subid);
    },
    removeSubscriptionsByService: function(serv) {
        var subs = $.grep(this.account().subscriptions, function(a) {
                       return a.service == serv; 
                   });
        $.each(subs, function() {
            Gnosus.account().removeSubscription(this.subid);
        });       
        return subs;
    },
    removePubSubNode: function(jid, node) {
        var acct = this.findAccountByJid(jid),
            item = null
        for(var i in acct.service_items) {
            if (acct.service_items[i].node == node) {
                delete_indx = i;
                break;
            }
        }
        if (delete_indx !=null) {item = acct.service_items.splice(delete_indx, 1);}
        return item;
    },
    findAllSubscriptions: function() {
        return this.account().subscriptions;
    },
    findSubscriptionsByNodeAndSubscription: function(node, sub) {
        return $.grep(this.account().subscriptions, function(s) {
            return s.node == node && s.subscription == sub;
        });
    },
    findSubscriptionBySubid: function(subid) {
        var sub = null;
        for (var s in this.account().subscriptions) {
            var chk = this.account().subscriptions[s]
            if (chk.subid == subid) {
                sub = chk;
                break;
            }
        }
        return sub;
    },
    /*-------------------------------------------------------------------------------
    disco
    ---------------------------------------------------------------------------------*/
    addService: function(jid, service, node, serv) {
        var service_model = new Service(service, node, $(serv).attr('name'), $(serv).attr('category'), $(serv).attr('type')),
            acct          = this.findAccountByJid(jid);
        if (acct) {acct.services.push(service_model);}
        return service_model;
    },
    addServiceItem: function(jid, service, parent_node, item) {
        var item_model = new ServiceItem(service, parent_node, $(item).attr('jid'), $(item).attr('node'), $(item).attr('name')),
            acct       = this.findAccountByJid(jid);
        if (acct) {acct.service_items.push(item_model);}
        return item_model;
    },
    addServiceFeature: function(jid, service, node, feature) {
        var feature_model = new ServiceFeature(service, node, $(feature).attr('var')),
            acct          = this.findAccountByJid(jid);
        if (acct) {acct.service_features.push(feature_model);}
        return feature_model;
    },
    findPubNodesByJid: function(jid) {
        var items = [],
            acct  = this.findAccountByJid(jid);
        if (acct) {
            var parent_node = GnosusXmpp.userPubsubRoot(jid);
            items = $.grep(acct.service_items, function(s) {
                       if (s.node) {
                           return s.parent_node == parent_node; 
                        } else {
                            return false;
                        }              
                    });
        }
        return items;
    },
    findServiceItemByJidAndNode: function(jid, node) {
        var item  = null,
            acct  = this.findAccountByJid(jid);
        if (acct) {
            for (var s in acct.service_items) {
                if (acct.service_items[s].node == node) {
                    item = acct.service_items[s];
                    break;
                }              
            }
        }
        return item;
    },
    findPubSubServiceByJid: function(jid) {
        var service = null,
            acct    = this.findAccountByJid(jid);
        if (acct) {
            for (var s in acct.services) {
                if (acct.services[s].category == 'pubsub' && acct.services[s].type =='service') {
                    service = acct.services[s];
                    break;
                }
            }
        }
        return service;
    },
    initServiceDisco: function(jid) {
        var acct = this.findAccountByJid(jid);
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
        if (Strophe.getResourceFromJid(resource.jid) != Gnosus.account_resource) {
            this.resources[resource.jid] = resource;
        }
    },
    removeResource: function(jid) {
        var resource = this.resources[jid];
        delete(this.resources[jid]);
        return resource;
    },
    deleteCommands: function() {
        $.each(this.resources, function(j,r) {
            r.deleteCommands();
        });
    },
    removeSubscription: function(subid) {
        var delete_indx = null,
            sub         = null,
            subs        = Gnosus.account().subscriptions;
        for(var i in subs) {
            if (subs[i].subid == subid) {
                delete_indx = i;
                break;
            }
        }
        if (delete_indx !=null) {sub = subs.splice(delete_indx, 1);}
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
        this.groups = $.map(item.find('group'), function (g, i) {return $(g).text();});
    },
    removeResource: function(jid) {
        var resource = this.resources[jid];
        delete(this.resources[jid]);
        return resource;
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
    deleteCommands: function() {
        this.commands = null;
    },
    updateWithClientVersion: function(version) {
        this.client_name = $(version).find('name').text() || 'none';
        this.client_version =  $(version).find('version').text() || 'none';
        this.client_os =  $(version).find('os').text() || 'none';
    },
    updateWithPresence: function(presence) {
        this.jid    = $(presence).attr('from');
        this.show   = $(presence).find('show').text();
        this.status =  $(presence).find('status').text()
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
    client_version: '0.0.0',
    client_name: 'gnos.us',
    
    /*-------------------------------------------------------------------------------*/
    client_identity: {category: 'client',
                      client: this.client_name,
                      type: 'web'},
    client_features: ['http://jabber.org/protocol/disco#info', 
                      'http://jabber.org/protocol/disco#items',
                      'jabber:iq:version',
                      'jabber:x:data',
                      'http://jabber.org/protocol/commands',
                      'http://jabber.org/protocol/pubsub',
                      'http://jabber.org/protocol/pubsub#publish',
                      'http://jabber.org/protocol/pubsub#subscribe',
                      'http://jabber.org/protocol/pubsub#create-nodes',
                      'http://jabber.org/protocol/pubsub#delete-nodes'],
    
    /*-------------------------------------------------------------------------------
    pubsub nodes
    ---------------------------------------------------------------------------------*/
    pubsubRoot: function(jid) {
        return '/home/'+Strophe.getDomainFromJid(jid);
    },

    /*-------------------------------------------------------------------------------*/
    userPubsubRoot: function(jid) {
        return this.pubsubRoot(jid)+'/'+Strophe.getNodeFromJid(jid);
    },

    /*-------------------------------------------------------------------------------*/
    userPubsubNode: function(jid, node) {
        return this.userPubsubRoot(jid)+'/'+node;
    },

    /*-------------------------------------------------------------------------------*/
    userPubsubRootToJid: function(node) {
        var node_comps = node.split('/');
        return node_comps[3]+'@'+node_comps[2];
    },

    /*-------------------------------------------------------------------------------*/
    subNodeFromNode: function(node) {
        return node.split('/').pop();
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
        this.connection.sendIQ(iq, 
            function(iq) {
                GnosusXmpp.subscriptionRequest(jid);
                $(document).trigger('roster_item_add_result', jid);
            },
            function(iq) {
                $(document).trigger('roster_item_add_error', jid);
            }
        );
    },
    
    /*-------------------------------------------------------------------------------*/
    removeContact: function (jid) {
        var iq = $iq({type: 'set'}).c('query', {xmlns: Strophe.NS.ROSTER}).c('item', {jid: jid, subscription: 'remove'});
        this.connection.sendIQ(iq, 
            function(iq) {
                GnosusXmpp.connection.send($pres({to: jid, type: "unsubscribe"}));
                $(document).trigger('roster_item_remove_result', jid);
            },
            function(iq) {
                $(document).trigger('roster_item_remove_error', jid);
            }
        );
    },
  
    /*-------------------------------------------------------------------------------*/
    acceptSubscriptionRequest: function (jid) {
        this.connection.send($pres({to: jid, type: "subscribed"}));
    },
 
    /*-------------------------------------------------------------------------------*/
    rejectSubscriptionRequest: function (jid) {
        this.connection.send($pres({to: jid, type: "unsubscribed"}));
    },
 
    /*-------------------------------------------------------------------------------*/
    subscriptionRequest: function (jid) {
        this.connection.send($pres({to: jid, type: 'subscribe'}));
    },
 
    /*-------------------------------------------------------------------------------*/
    initialPresence: function () {
        var presence = $pres({priority: 1});
        this.connection.send(presence);
    },
    
    /*-------------------------------------------------------------------------------
    chat
    ---------------------------------------------------------------------------------*/
    chatTextMessage: function (to, body) {
        var msg = $msg({to:to, type:'chat'}).c('body').t(body);
        this.connection.send(msg);
        return Gnosus.addOutgoingChatTextMessage(to, body);
    }, 
    
    /*-------------------------------------------------------------------------------
    commands
    ---------------------------------------------------------------------------------*/
    getCommandList: function (to) {
        var cmd_iq = $iq({to:to, type:'get'}).c('query', {xmlns:Strophe.NS.DISCO_ITEMS, node:'http://jabber.org/protocol/commands'});
        this.connection.sendIQ(cmd_iq, 
            function(iq) {
                var jid = $(iq).attr('from');
                Gnosus.initCommands(jid);
                $(iq).find('item').each(function () {
                    Gnosus.addCommand(jid, $(this).attr('node'), $(this).attr('name'));
                });
                $(document).trigger('command_list_result', jid);
            },
            function(iq) {
                var jid = $(iq).attr('from');
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
        if (args['sessionid']) {
            cmd_attr['sessionid'] = args['sessionid'];
        }
        var cmd_iq = $iq({to:args['to'], type: 'set'}).c('command', cmd_attr);
        if (args['payload']) {
            cmd_iq.cnode(args['payload']);
            msg = Gnosus.addCommandXDataMessage(cmd_iq.nodeTree);
        } else {
            msg = Gnosus.addCommandTextMessage(args['to'], args['node'], 'command request');
        }
        this.connection.sendIQ(cmd_iq, 
            function(iq) {
                var jid    = $(iq).attr('from'),
                    x      = $(iq).find('x'),
                    x_type = 'result';
                if (x.length > 0) {
                    x_type = $(x.eq(0)).attr('type')
                    if (x_type == 'form') {
                        $(document).trigger('command_form', iq);
                    } else {
                        $(document).trigger('command_result', Gnosus.addCommandXDataMessage(iq));
                    }
                } else {
                    $(document).trigger('command_result', Gnosus.addCommandTextMessage(args['to'], args['node'], 
                        $(iq).find('command').eq(0).attr('status')));
                }
            },
            function(iq) {
                var from = $(iq).attr('from'),
                    cmd  = $(iq).find('command').eq(0),
                    node = cmd.attr('node'),
                    msg  = cmd.find('text').text();
                $(document).trigger('command_error', [from, node, msg]);
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
        this.connection.sendIQ(cmd_iq, 
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
        this.connection.sendIQ(iq, 
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
            .c('subscribe', {node:node, jid:Gnosus.account_jid});
        this.connection.sendIQ(iq, 
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
            .c('unsubscribe', {node:node, jid:Gnosus.account_jid, subid:subid});
        this.connection.sendIQ(iq, 
            function(iq) {
                $(document).trigger('unsubscribe_result', Gnosus.removeSubscription(subid));
            },
            function(iq) {
                $(document).trigger('unsubscribe_error', [service, node]);
            }
        );
    },

    /*-------------------------------------------------------------------------------*/
    setCreatePubSubNode: function(node) {
        var jid       = Gnosus.account_jid,
            full_node = GnosusXmpp.userPubsubNode(jid, node),
            serv      = Gnosus.findPubSubServiceByJid(jid),
            iq        = $iq({to:serv.jid, type: 'set'})
                            .c('pubsub', {xmlns:Strophe.NS.PUBSUB})
                            .c('create', {node:full_node}).up().c('configure');
        this.connection.sendIQ(iq,
            function(iq) {
                $(document).trigger('create_pubsub_node_result',
                    Gnosus.addPubSubNode(jid, serv, GnosusXmpp.userPubsubRoot(Gnosus.account_jid), full_node));
            },
            function(iq) {
                $(document).trigger('create_pubsub_node_error', full_node);
            }
        );
    },

    /*-------------------------------------------------------------------------------*/
    setCreateUserPubsubRoot: function() {
        var jid       = Gnosus.account_jid,
            full_node = GnosusXmpp.userPubsubRoot(jid),
            serv      = Gnosus.findPubSubServiceByJid(jid),
            iq        = $iq({to:serv.jid, type: 'set'})
                            .c('pubsub', {xmlns:Strophe.NS.PUBSUB})
                            .c('create', {node:full_node}).up().c('configure');
        this.connection.sendIQ(iq, 
            function(iq) {
                Gnosus.addPubSubNode(jid, serv, GnosusXmpp.pubsubRoot(jid), full_node)
                GnosusXmpp.initialPresence();
                $(document).trigger('session_init_result');
            },
            function(iq) {
                $(document).trigger('session_init_error');
            }
        );
    },

    /*-------------------------------------------------------------------------------*/
    setDeletePubSubNode: function(node) {
        var jid       = Gnosus.account_jid;
            full_node = GnosusXmpp.userPubsubNode(jid, node),
            serv      = Gnosus.findPubSubServiceByJid(jid),
            iq        = $iq({to:serv.jid, type: 'set'})
                            .c('pubsub', {xmlns:Strophe.NS.PUBSUB_OWNER})
                            .c('delete', {node:full_node});
        this.connection.sendIQ(iq, 
            function(iq) {
                $(document).trigger('delete_pubsub_node_result', Gnosus.removePubSubNode(jid, full_node));
            },
            function(iq) {
                $(document).trigger('delete_pubsub_node_error',  full_node);
            }
        );
    },

    /*-------------------------------------------------------------------------------*/
    setPublishEntry: function (node, entry) {
        var serv      = Gnosus.findPubSubServiceByJid(Gnosus.account_jid),
            full_node = GnosusXmpp.userPubsubNode(Gnosus.account_jid, node),
            iq        = $iq({to:serv.jid, type: 'set'})
                            .c('pubsub', {xmlns:Strophe.NS.PUBSUB})
                            .c('publish', {node:full_node}).c('item').c('entry', {xmlns:Strophe.NS.ENTRY})
                            .c('title').t(entry);
        this.connection.sendIQ(iq, 
            function() {
                $(document).trigger('publish_entry_result', Gnosus.addOutgoingEntryMessage(full_node, entry));
            },
            function(iq) {
                $(document).trigger('publish_entry_error',  node);
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
        this.connection.sendIQ(iq, 
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
        this.connection.sendIQ(iq, 
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
    resultClientDiscoInfo: function(iq) {
        var resp = $iq({to:$(iq).attr('from'), type:'result', id:$(iq).attr('id')}).c('query', {xmlns:Strophe.NS.DISCO_INFO})
            .c('identity', {category:this.client_identity['category'], type:this.client_identity['type'], name:this.client_identity['name']}).up();
        $.each(this.client_features, function() {
            resp.c('feature', {'var':this}).up();
        });  
        this.connection.send(resp);
    },
    
    /*-------------------------------------------------------------------------------*/
    resultClientDiscoItems: function(iq) {
        var resp = $iq({to:$(iq).attr('from'), type:'result', id:$(iq).attr('id')}).c('query', {xmlns:Strophe.NS.DISCO_ITEMS});
        this.connection.send(resp);
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
    },
    
    /*-------------------------------------------------------------------------------
    client version
    ---------------------------------------------------------------------------------*/
    getClientVersion: function (to) {
        var cv_iq = $iq({to:to, type:'get'}).c('query', {xmlns:Strophe.NS.CLIENT_VERSION});
        this.connection.sendIQ(cv_iq, 
            function(iq) {
                $(document).trigger('client_version_result', Gnosus.updateWithClientVersion(iq));                
            },
            function(iq) {
                $(document).trigger('client_version_error', to);                
            }
        );
    },      
    resultClientVersion: function (iq) {
        var resp = $iq({to:$(iq).attr('from'), type:'result', id:$(iq).attr('id')}).c('query', {xmlns:Strophe.NS.CLIENT_VERSION})
            .c('name').t(GnosusXmpp.client_name).up().c('version').t(GnosusXmpp.client_version);
        this.connection.send(resp);
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
            this.connection.sendIQ(roster_iq, 
                function(iq) {
                    $(iq).find('item').each(function () {Gnosus.addContact($(this));});
                    GnosusXmpp.getPubSubServiceDisco(Gnosus.account_jid, Strophe.getDomainFromJid(Gnosus.account_jid), 
                        function() {
                            GnosusXmpp.initialPresence();
                            $(document).trigger('session_init_result');
                        }.bind(this),
                        function() {
                            GnosusXmpp.setCreateUserPubsubRoot();
                        }.bind(this)
                    )
                },
                function(iq) {
                    $(document).trigger('session_init_error');
                }
            );
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
            acct = Gnosus.findAccountByJid(jid);
        if (acct && ptype != 'error') {
            if (ptype === 'unavailable') {
                $(document).trigger("presence_unavailable", [acct, Gnosus.removeResource(presence)]);
            } else if (ptype === 'subscribe') {
                GnosusXmpp.acceptSubscriptionRequest(jid);
            } else if (ptype === 'unsubscribed') {
                Gnosus.removeResources(jid);
                $(document).trigger("presence_unsubscribed", acct);
            } else {
                if (from != Gnosus.accountFullJid()){GnosusXmpp.getClientVersion(from);}
                $(document).trigger("presence_available", [acct, Gnosus.addResource(presence)]);
            }        
        } else if (ptype === 'subscribe') {
            $(document).trigger("presence_subscribe", jid);
        } else if (jid == Gnosus.account_jid) {
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
        } else if (node == '' || node == null) {
            GnosusXmpp.resultClientDiscoItems(iq);
        } else {
            GnosusXmpp.errorDiscoItems(iq, 'service-unavailable');
        }
        return true;
    },

    /*-------------------------------------------------------------------------------*/
    onDisoInfoGet: function (iq) {
        var node  = $(iq).find('query').attr('node');
        if (node == '' || node == null) {
            GnosusXmpp.resultClientDiscoInfo(iq);
        } else {
            GnosusXmpp.errorDiscoInfo(iq, 'service-unavailable');
        }
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
            Strophe.addNamespace('PUBSUB_OWNER', 'http://jabber.org/protocol/pubsub#owner');
            Strophe.addNamespace('ENTRY', 'http://www.w3.org/2005/Atom');
        }
    },
});

/**********************************************************************************
client version
**********************************************************************************/
Strophe.addConnectionPlugin('client_version', {

    /*-------------------------------------------------------------------------------*/
    init: function (connection) {
        this.connection = connection;
        Strophe.addNamespace('CLIENT_VERSION', 'jabber:iq:version');
    },
 
    /*-------------------------------------------------------------------------------*/
    statusChanged: function (status) {
        if (status === Strophe.Status.CONNECTED) {
        	this.connection.addHandler(this.onClientVersionGet.bind(this), Strophe.NS.CLIENT_VERSION, 'iq', 'get'); 
        }
    },
 
    /*-------------------------------------------------------------------------------*/
    onClientVersionGet: function (iq) {
        GnosusXmpp.resultClientVersion(iq)
        return true;
    },

});

