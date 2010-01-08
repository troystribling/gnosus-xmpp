/**********************************************************************************
logging
**********************************************************************************/
function rawInput(data) {
    console.log('RECV: ' + data);
}

function rawOutput(data) {
    console.log('SENT: ' + data);
}

Strophe.log = function (level, msg) {
    console.log('LOG: ' + msg);
}

/**********************************************************************************
client
**********************************************************************************/
function new_client(client_id) {
    var client = $(client_id);
    $(function() {
        client.resizable({handles:'s', minHeight: 250, autoHide: true});
    });
	client.splitter({type: "v", outline: true, minLeft: 200, sizeLeft: 200, minRight: 500, cookie: "vsplitter"});
}

/**********************************************************************************
connection
**********************************************************************************/
function connect(service, jid, password) {
    new_client('#client-1')
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

    /*-------------------------------------------------------------------------------*/
    add_contact: function(item) {
        var contact = new Contact(item.attr('jid'));
        contact.set_attributes(item);
        Gnosus.contacts[contact.jid] = contact;
    },
    find_contact: function(jid) {
        return Gnosus.contacts[contact.jid];
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
    add_roster_item: function(presence) {
        var from = $(presence).attr('from');
        var bare_jid = Strophe.getBareJidFromJid(from);
        if (Gnosus.contacts[bare_jid]) {
            roster_item = new RosterItem(from);
            roster_item.set_presence_attributes(presence);
            Gnosus.contacts[bare_jid].add_roster_item(roster_item);
        }
    },
    remove_roster_item: function(presence) {
        var from = $(presence).attr('from');
        var bare_jid = Strophe.getBareJidFromJid(from);
        if (Gnosus.contacts[bare_jid]) {
            Gnosus.contacts[bare_jid].remove_roster_item(jid);
        }
    },
    remove_roster_items: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        if (Gnosus.contacts[bare_jid]) {
            Gnosus.contacts[bare_jid].remove_roster_items();
        }
    },
    find_all_roster_items: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        return Gnosus.contacts[bare_jid] ? Gnosus.contacts[bare_jid].resources : null;
    },
    find_roster_item: function(jid) {
        var bare_jid = Strophe.getBareJidFromJid(jid);
        return Gnosus.contacts[bare_jid] ? Gnosus.contacts[bare_jid].resources[jid] : null;
    },
    go_off_line: function() {
        for (var contact in Gnosus.contacts) {
            Gnosus.contacts[contact].resources = {};
        }            
    }
}

/**********************************************************************************
models
**********************************************************************************/
function Account(service, jid, password) {
    this.service = service;
    this.jid = jid;
    this.password = password;
}

/*-------------------------------------------------------------------------------*/
function Contact(jid) {
    this.jid = jid;
    this.ask = '';
    this.subscription = '';
    this.groups = [];
    this.resources = {}
}

Contact.prototype = {
    online: function() {
        var result = false;
        for (var k in this.resources) {
            result = true;
            break;
        }
        return result;
    },
    set_attributes: function(item) {
        this.subscription = item.attr('subscription') || 'none';
        this.ask = item.attr('ask') || '';
        item.find('group').each(function () {
            this.groups.push($(this).text());
        });
    },
    add_roster_item: function(roster_item) {
        this.resources[jid] = roster_item;
    },
    remove_roster_item: function(jid) {
        delete(this.resources[jid]);
    }
    remove_roster_items: function() {
        this.resources = {};
    }
}

/*-------------------------------------------------------------------------------*/
function RosterItem(jid) {
    this.jid = jid;
    this.show = '';
    this.status = '';
    this.client_name = '';
    this.client_version = '';
    this.client_os = '';
}

RosterItem.prototype = {
    set_presence_attributes: function(presence) {
        this.client_name = $(presence).find('show').text() || 'online';
        this.status =  $(presence).find('status').text();
    },
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
            Gnosus.connection.addHandler(this.rosterChanged.bind(this), Strophe.NS.ROSTER, 'iq', 'set');
            Gnosus.connection.addHandler(this.presenceChanged.bind(this), null, 'presence');
            var roster_iq = $iq({type: 'get'}).c('query', {xmlns: Strophe.NS.ROSTER}); 
            var that = this;
            Gnosus.connection.sendIQ(roster_iq, function(iq) {
                $(iq).find('item').each(function () {
                    Gnosus.add_contact($(this));
                });
                // $(document).trigger('roster_changed', that);
            });
        } else if (status === Strophe.Status.DISCONNECTED) {
            Gnosus.go_off_line();
            // $(document).trigger('roster_changed', this);
        }
    },
 
    /*-------------------------------------------------------------------------------*/
    rosterChanged: function (iq) {
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
        // $(document).trigger("roster_changed", this);
        return true;
    },
 
    /*-------------------------------------------------------------------------------*/
    presenceChanged: function (presence) {
        var from = $(presence).attr('from');
        var jid = Strophe.getBareJidFromJid(from);
        var ptype = $(presence).attr('type') || 'available'; 
        if (Gnosus.find_contact(jid) && ptype != 'error') {
            if (ptype === 'unavailable') {
                Gnosus.remove_roster_item(presence);
            } else {
                Gnosus.add_roster_item(presence);
            }        
            // $(document).trigger("roster_changed", this);
        }
    },
 
    /*-------------------------------------------------------------------------------*/
    addContact: function (jid, name, groups) {
        var iq = $iq({type: "set"}).c("query", {xmlns: Strophe.NS.ROSTER}).c("item", {name: name || "", jid: jid});
        if (groups && groups.length > 0) {
            $.each(groups, function () {
                iq.c("group").t(this).up();
            });
        }
        Gnosus.connection.sendIQ(iq);
    },
    
    /*-------------------------------------------------------------------------------*/
    deleteContact: function (jid) {
        var iq = $iq({type: "set"}).c("query", {xmlns: Strophe.NS.ROSTER}).c("item", {jid: jid, subscription: "remove"});
        Gnosus.connection.sendIQ(iq);
    },
  
    /*-------------------------------------------------------------------------------*/
    modifyContact: function (jid, name, groups) {
        Gnosus.addContact(jid, name, groups);
    },
 
    /*-------------------------------------------------------------------------------*/
    subscribe: function (jid, name, groups) {
        this.addContact(jid, name, groups);
        var presence = $pres({to: jid, "type": "subscribe"});
        Gnosus.connection.send(presence);
    },
    
    /*-------------------------------------------------------------------------------*/
    unsubscribe: function (jid) {
        var presence = $pres({to: jid, "type": "unsubscribe"});
        Gnosus.connection.send(presence);
        this.deleteContact(jid);
    }
});
