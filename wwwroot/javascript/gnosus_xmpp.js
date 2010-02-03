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
        for (var contact in Gnosus.contacts) {
            if (Gnosus.contacts[contact].name == name) {
                result = Gnosus.contacts[contact];
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
        for (var contact in Gnosus.contacts) {
            Gnosus.contacts[contact].resources = {};
        }            
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
    
    /*-------------------------------------------------------------------------------
    commands
    ---------------------------------------------------------------------------------*/
    addCommand: function(jid, node, name) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        var resource = null;
        if (bare_jid == Gnosus.account.jid) {
            resource = Gnosus.findAccountResource(jid)
        } else {
            resource = Gnosus.findContactResource(jid)
        }
        if (resource) {
            resource.addCommand(node, name);
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
                resources = Gnosus.findAllContactResources(jid)
            }
            for (var resource in resources) {
                if (!resources[resource].commands) {
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
                resource = Gnosus.findContactResource(jid)
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
            for (var cmd in cmds) {
                all_commands[cmd.node] = cmd;
            }
        };
        if (bare_jid == jid) {
            var resources = [];
            if (bare_jid == Gnosus.account.jid) {
                resources = Gnosus.findAllAccountResources(jid);
            } else {
                resources = Gnosus.findAllContactResources(jid)
            }
            for (var resource in resources) {
                addCommandHash(resource.commands);
            }
        } else {
            var resource = null;
            if (bare_jid == Gnosus.account.jid) {
                resource = Gnosus.findAccountResource(jid);
            } else {
                resource = Gnosus.findContactResource(jid)
            }
            addCommandHash(resource.commands);
        }
        return all_commands;
    }
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
}

Account.prototype = {
    fullJid: function() {
        return this.jid+'/'+this.resource;
    },
    addResource: function(resource) {
        this.resources[resource.jid] = resource;
    },
    deleteCommands: function() {
        for(var jid in this.resources) {
            this.resources[jid].deleteCommands();
        }
    }    
}

/*-------------------------------------------------------------------------------*/
function Message(to, from, text, type, content_type, node, item_id) {
    this.to = to;
    this.from = from;
    this.type = type;
    this.text = text;
    this.content_type = content_type;
    this.node = node;
    this.item_id = item_id;
    this.created_at = new Date(); 
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
}

Contact.prototype = {
    show: function() {
        var stat = 'offline';
        for (var k in this.resources) {
            stat = this.resources[k].show;
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
        for(var jid in this.resources) {
            this.resources[jid].deleteCommands();
        }
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
    addCommand: function(node, name) {
        if (!this.commands) {
            this.commands = [];
        }
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

/**********************************************************************************
Gnosus XMPP Interface interface
**********************************************************************************/
GnosusXmpp = {
    
    /*---------------------------------------------------------------------------*/
    connection: null,
    
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
                $(document).trigger('roster_item_add_response', iq);
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
                $(document).trigger('roster_item_remove_response', iq);
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
        var msg = $msg({to: to, type: 'chat'}).c('body').t(body);
        GnosusXmpp.connection.send(msg);
        return Gnosus.addOutgoingChatTextMessage(to, body);
    }, 
    
    /*-------------------------------------------------------------------------------
    commands
    ---------------------------------------------------------------------------------*/
    getCommandList: function (jid) {
        var command_iq = $iq({to:jid, type: 'get'}).c('query', {xmlns: Strophe.NS.DISCOITEMS, node:'http://jabber.org/protocol/commands'});
        GnosusXmpp.connection.sendIQ(command_iq, function(iq) {
            var type = $(iq).attr('type');
            var jid = $(iq).attr('from');
            if (type == 'result') {
                $(iq).find('item').each(function () {
                    var node = $(this).attr('node');
                    var cmd_name = $(this).attr('name');
                    Gnosus.addCommand(jid, node, cmd_name);
                });
                $(document).trigger('command_list_response', jid);
            } else {
                $(document).trigger('command_list_error', jid);
            }
        });
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
        Strophe.addNamespace('DISCOITEMS', 'http://jabber.org/protocol/disco#items');
        Strophe.addNamespace('DISCOINFO', 'http://jabber.org/protocol/disco#info');
        Strophe.addNamespace('XMPPSTANZAS', 'urn:ietf:params:xml:ns:xmpp-stanzas');
    },
 
    /*-------------------------------------------------------------------------------*/
    statusChanged: function (status) {
        if (status === Strophe.Status.CONNECTED) {
        	this.connection.addHandler(this.onDisoItemsGet.bind(this), Strophe.NS.DISCOITEMS, 'iq', 'get'); 
        	this.connection.addHandler(this.onDisoInfoGet.bind(this), Strophe.NS.DISCOINFO, 'iq', 'get'); 
        }
    },
 
    /*-------------------------------------------------------------------------------*/
    onDisoItemsGet: function (iq) {
        var resp = $iq({type: 'error', to:$(iq).attr('from'), id:$(iq).attr('id')})
            .c('query', {xmlns:Strophe.NS.DISCOITEMS, node:$(iq).find('query').attr('node')}).up()
            .c('error', {type: 'cancel'})
            .c('service-unavailable', {xmlns: Strophe.NS.XMPPSTANZAS});
        this.connection.send(resp);
        return true;
    },

    /*-------------------------------------------------------------------------------*/
    onDisoInfoGet: function (iq) {
        var resp = $iq({type: 'error', to:$(iq).attr('from'), id:$(iq).attr('id')})
            .c('query', {xmlns:Strophe.NS.DISCOINFO, node:$(iq).find('query').attr('node')}).up()
            .c('error', {type: 'cancel'})
            .c('service-unavailable', {xmlns: Strophe.NS.XMPPSTANZAS});
        this.connection.send(resp);
        return true;
    }

});
