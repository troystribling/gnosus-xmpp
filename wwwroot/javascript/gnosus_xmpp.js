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
	Gnosus.connection = conn;
	Gnosus.account = new Account(service, jid, password)
}

/*-------------------------------------------------------------------------------*/
function onConnect(status) {
    if (status == Strophe.Status.CONNECTING) {
	    console.log('Strophe is connecting.');
    } else if (status == Strophe.Status.CONNFAIL) {
	    console.log('Strophe failed to connect.');
    } else if (status == Strophe.Status.DISCONNECTING) {
	    console.log('Strophe is disconnecting.');
    } else if (status == Strophe.Status.DISCONNECTED) {
	    console.log('Strophe is disconnected.');
    } else if (status == Strophe.Status.CONNECTED) {
	    console.log('Strophe is connected.');
    }
}

/**********************************************************************************
client interface
**********************************************************************************/
Gnosus = {

    /*-------------------------------------------------------------------------------*/
    connection: null,
    account: null,
    contacts: {},
    messages: {},

    /*-------------------------------------------------------------------------------*/
    add_contact: function(item) {
        var groups = item.find('group').map(function (g, i) {g.text();});
        jid = item.attr('jid');
        Gnosus.contacts[jid] = new Contact(item.attr('jid'), item.attr('name'), item.attr('ask'), item.attr('subscription'), groups);
    },
    find_contact: function(jid) {
        return Gnosus.contacts[jid];
    },
    find_all_contacts: function() {
        return Gnosus.contacts;
    },
    update_contact: function(item) {
        var contact = Gnosus.contacts[item.attr('jid')];
        if (contact) {
            contact.groups = [];
            contact.set_attributes(item);
        }
    },
    remove_contact: function(item) {
        jid = item.attr('jid');
        if (Gnosus.contacts[jid]) {
            delete(Gnosus.contacts[jid]);
        }
    },

    /*-------------------------------------------------------------------------------*/
    add_contact_resource: function(presence) {
        var from = $(presence).attr('from');
        var bare_jid = Strophe.getBareJidFromJid(from);
        if (Gnosus.contacts[bare_jid]) {
            resource = new Resource(from, $(presence).find('show').text(), $(presence).find('status').text());
            Gnosus.contacts[bare_jid].add_resource(resource);
        }
    },
    remove_contact_resource: function(presence) {
        var from = $(presence).attr('from');
        var bare_jid = Strophe.getBareJidFromJid(from);
        if (Gnosus.contacts[bare_jid]) {
            Gnosus.contacts[bare_jid].remove_resource(from);
        }
    },
    remove_contact_resources: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        if (Gnosus.contacts[bare_jid]) {
            Gnosus.contacts[bare_jid].remove_resources();
        }
    },
    find_all_contact_resources: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        return Gnosus.contacts[bare_jid] ? Gnosus.contacts[bare_jid].resources : null;
    },
    find_contact_resource: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        return Gnosus.contacts[bare_jid] ? Gnosus.contacts[bare_jid].resources[jid] : null;
    },
    contact_offline: function() {
        for (var contact in Gnosus.contacts) {
            Gnosus.contacts[contact].resources = {};
        }            
    },

    /*-------------------------------------------------------------------------------*/
    add_account_resource: function(presence) {
        var from = $(presence).attr('from');
        resource = new Resource(from);
        Gnosus.account.add_resource(resource);
    },
    remove_account_resource: function(presence) {
        var from = $(presence).attr('from');
        Gnosus.account.remove_resource(from);
    },
    remove_account_resources: function() {
        Gnosus.account.remove_resources();
    },
    find_all_account_resources: function() {
        return Gnosus.account.resources;
    },
    find_account_resource: function(jid) {
        return Gnosus.account.resources[jid];
    },
    
    /*-------------------------------------------------------------------------------*/
    add_chat_message: function(msg) {
        var to = $(msg).attr('to');
        if (!Gnosus.message[to]) {
            Gnosus.message[to] = [];
        }
        Gnosus.messages[to].push(new Message(to, $(msg).attr('from'), 'chat', $(msg).find('body').text(), 'text'));        
    },
    find_all_messages: function() {
        var all_message = [];
        for (var j in  Gnosus.messages) {
            all_messages.push(Gnosus.messages[j])
        }
        return all_messages;
    }    
}

/**********************************************************************************
models
**********************************************************************************/
function Account(service, jid, password) {
    this.service = service;
    this.jid = Strophe.getBareJidFromJid(jid);
    this.password = password;
    this.resources = {};
}

Account.prototype = {
    add_resource: function(resource) {
        this.resources[resource.jid] = resource;
    }
}

/*-------------------------------------------------------------------------------*/
function Message(to, from, type, text, content_type, node, item_id) {
    this.to = to;
    this.from = from;
    this.text = text;
    this.content_type = content_type;
    this.node = node;
    this.item_id = item_id;
    this.created_at = new Date(); 
}

Message.prototype = {    
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
        var show = "offline";
        for (var k in this.resources) {
            show = this.resources[k];
            if (show == 'online') {
                break;
            }
        }
        return show;
    },
    add_resource: function(resource) {
        this.resources[resource.jid] = resource;
    },
    remove_resource: function(jid) {
        delete(this.resources[jid]);
    },
    remove_resources: function() {
        this.resources = {};
    }
}

/*-------------------------------------------------------------------------------*/
function Resource(jid, show, status) {
    this.jid = jid;
    this.show = show || 'onine';
    this.status = status || 'unknown';
    this.client_name = '';
    this.client_version = '';
    this.client_os = '';
}

Resource.prototype = {
    set_version_attributes: function(version) {
        this.client_name = $(version).find('name').text() || 'none';
        this.client_version =  $(version).find('version').text() || 'none';
        this.client_os =  $(version).find('os').text() || 'none';
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
            Gnosus.connection.addHandler(this.onRosterSet.bind(this), Strophe.NS.ROSTER, 'iq', 'set');
            Gnosus.connection.addHandler(this.onPresence.bind(this), null, 'presence');
            var roster_iq = $iq({type: 'get'}).c('query', {xmlns: Strophe.NS.ROSTER}); 
            var that = this;
            Gnosus.connection.sendIQ(roster_iq, function(iq) {
                $(iq).find('item').each(function () {
                    Gnosus.add_contact($(this));
                });
                $(document).trigger('roster_item', that);
            });
            this.initial_presence();
        } else if (status === Strophe.Status.DISCONNECTED) {
            Gnosus.contact_offline();
            $(document).trigger('roster_item', this);
        }
    },
 
    /*-------------------------------------------------------------------------------*/
    onRosterSet: function (iq) {
        var item = $(iq).find('item');
        var jid = item.attr('jid');
        var subscription = item.attr('subscription') || '';        
        if (subscription === 'remove') {
            Gnosus.remove_contact(item);
        } else if (subscription === 'none') {
            Gnosus.add_contact(item);
        } else {
            Gnosus.update_contact(item);
        }
        this.connection.send($iq({type: 'result', id: $(iq).attr('id')}));
        $(document).trigger("roster_item", this);
        return true;
    },
 
    /*-------------------------------------------------------------------------------*/
    onPresence: function (presence) {
        var from = $(presence).attr('from');
        var jid = Strophe.getBareJidFromJid(from);
        var ptype = $(presence).attr('type') || 'available'; 
        if (Gnosus.find_contact(jid) && ptype != 'error') {
            if (ptype === 'unavailable') {
                Gnosus.remove_contact_resource(presence);
            } else {
                Gnosus.add_contact_resource(presence);
            }        
            $(document).trigger("roster_item", this);
        } else if (jid == Gnosus.account.jid) {
            Gnosus.add_account_resource(presence);
        }
        return true;
    },
 
    /*-------------------------------------------------------------------------------*/
    addContact: function (jid, name, groups) {
        var iq = $iq({type: 'set'}).c('query', {xmlns: Strophe.NS.ROSTER}).c('item', {name: name || "", jid: jid});
        if (groups && groups.length > 0) {
            $.each(groups, function () {
                iq.c("group").t(this).up();
            });
        }
        Gnosus.connection.sendIQ(iq);
    },
    
    /*-------------------------------------------------------------------------------*/
    deleteContact: function (jid) {
        var iq = $iq({type: 'set'}).c('query', {xmlns: Strophe.NS.ROSTER}).c('item', {jid: jid, subscription: 'remove'});
        Gnosus.connection.sendIQ(iq);
    },
  
    /*-------------------------------------------------------------------------------*/
    modifyContact: function (jid, name, groups) {
        Gnosus.addContact(jid, name, groups);
    },
 
    /*-------------------------------------------------------------------------------*/
    initial_presence: function () {
        var presence = $pres({priority: 1});
        Gnosus.connection.send(presence);
    },
    
    /*-------------------------------------------------------------------------------*/
    subscribe: function (jid, name, groups) {
        this.addContact(jid, name, groups);
        var presence = $pres({to: jid, type: 'subscribe'});
        Gnosus.connection.send(presence);
    },
    
    /*-------------------------------------------------------------------------------*/
    unsubscribe: function (jid) {
        var presence = $pres({to: jid, type: "unsubscribe"});
        Gnosus.connection.send(presence);
        this.deleteContact(jid);
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
        	Gnosus.connection.addHandler(this.onMessage.bind(this), null, 'message'); 
        }
    },
 
    /*-------------------------------------------------------------------------------*/
    onMessage: function (msg) {
        var type = $(msg).attr('type');
        if (type == "chat" && $(msg).attr('body')) {
            Gnosus.add_chat_message(msg);
    	    $(document).trigger("chat_message", msg);            
	    }
    },
 
    /*-------------------------------------------------------------------------------*/
    sendMessage: function (to, body) {
    }
 
});
