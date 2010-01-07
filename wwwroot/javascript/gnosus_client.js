/**********************************************************************************
logging
**********************************************************************************/
function rawInput(data) {
    console.log("RECV: " + data);
}

function rawOutput(data) {
    console.log("SENT: " + data);
}

Strophe.log = function (level, msg) {
    console.log("LOG: " + msg);
}

/**********************************************************************************
client
**********************************************************************************/
Gnosus = {
    connection: null,
    account: null,
    contacts: [],
    roster: [],
    add_contact: function(jid) {
        Gnosus.contacts.push(new Contact(jid));
    },
    remove_contact: function(jid) {
        Gnosus.contacts = $.grep(Gnosus.contacts, function(c)  {
                              return !(c.jid == jid);
                          });
    },
    add_roster_item: function(jid) {
        Gnosus.roster.push(new RosterItem(jid));
    },
    remove_roster_item: function(jid) {
        Gnosus.roster = $.grep(Gnosus.roster, function(r)  {
                            return !(r.jid.match(jid));
                        });
    },
    find_all_roster_items: function(jid) {
        $.grep(Gnosus.roster, function(r)  {
            return r.jid.match(jid);
        });
    }
}

/*-------------------------------------------------------------------------------*/
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
    new_client("#client-1")
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
	    console.log("Strophe is connecting.");
    } else if (status == Strophe.Status.CONNFAIL) {
	    console.log("Strophe failed to connect.");
    } else if (status == Strophe.Status.DISCONNECTING) {
	    console.log("Strophe is disconnecting.");
    } else if (status == Strophe.Status.DISCONNECTED) {
	    console.log("Strophe is disconnected.");
    } else if (status == Strophe.Status.CONNECTED) {
	    console.log("Strophe is connected.");
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
    this.ask = "";
    this.subscription = "none";
    this.groups = [];
}

Contact.prototype = {
    online: function() {
        return Gnosus.find_all_roster_items(this.jid).length > 0
    },
}

/*-------------------------------------------------------------------------------*/
function RosterItem(jid) {
    this.jid = "";
    this.client_name = "";
    this.client_version = "";
    this.client_os = "";
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
            this.connection.addHandler(this.rosterChanged.bind(this), Strophe.NS.ROSTER, "iq", "set");
            this.connection.addHandler(this.presenceChanged.bind(this), null, "presence");
            var roster_iq = $iq({type: "get"}).c('query', {xmlns: Strophe.NS.ROSTER}); 
            var that = this;
            this.connection.sendIQ(roster_iq, function (iq) {
                $(iq).find("item").each(function () {
                    var contact = new Contact();
                    contact.name = $(this).attr('name') || "";
                    contact.subscription = $(this).attr('subscription') ||
                        "none";
                    contact.ask = $(this).attr('ask') || "";
                    $(this).find("group").each(function () {
                        contact.groups.push($(this).text());
                    });
                    that.contacts[$(this).attr('jid')] = contact;
                });
 
                // let user code know something happened
                $(document).trigger('roster_changed', that);
            });
        } else if (status === Strophe.Status.DISCONNECTED) {
            // set all users offline
            for (var contact in this.contacts) {
                this.contacts[contact].resources = {};
            }
            
            // notify user code
            $(document).trigger('roster_changed', this);
        }
    },
 
    /*-------------------------------------------------------------------------------*/
    rosterChanged: function (iq) {
        var item = $(iq).find('item');
        var jid = item.attr('jid');
        var subscription = item.attr('subscription') || "";
        
        if (subscription === "remove") {
            // removing contact from roster
            delete this.contacts[jid];
        } else if (subscription === "none") {
            // adding contact to roster
            var contact = new Contact();
            contact.name = item.attr('name') || "";
            item.find("group").each(function () {
                contact.groups.push(this.text());
            });
            this.contacts[jid] = contact;
        } else {
            // modifying contact on roster
            var contact = this.contacts[jid];
            contact.name = item.attr('name') || contact.name;
            contact.subscription = subscription || contact.subscription;
            contact.ask = item.attr('ask') || contact.ask;
            contact.groups = [];
            item.find("group").each(function () {
                contact.groups.push(this.text());
            });
        }
        
        // acknowledge receipt
        this.connection.send($iq({type: "result", id: $(iq).attr('id')}));
        
        // notify user code of roster changes
        $(document).trigger("roster_changed", this);
        
        return true;
    },
 
    /*-------------------------------------------------------------------------------*/
    presenceChanged: function (presence) {
        var from = $(presence).attr("from");
        var jid = Strophe.getBareJidFromJid(from);
        var resource = Strophe.getResourceFromJid(from);
        var ptype = $(presence).attr("type") || "available";
 
        if (!this.contacts[jid] || ptype === "error") {
            // ignore presence updates from things not on the roster
            // as well as error presence
            return true;
        }
        
        if (ptype === "unavailable") {
            // remove resource, contact went offline
            delete this.contacts[jid].resources[resource];
        } else {
            // contact came online or changed status
            this.contacts[jid].resources[resource] = {
                show: $(presence).find("show").text() || "online",
                status: $(presence).find("status").text()
            };
        }
        
        // notify user code of roster changes
        $(document).trigger("roster_changed", this);
    },
 
    /*-------------------------------------------------------------------------------*/
    addContact: function (jid, name, groups) {
        var iq = $iq({type: "set"})
            .c("query", {xmlns: Strophe.NS.ROSTER})
            .c("item", {name: name || "", jid: jid});
        if (groups && groups.length > 0) {
            $.each(groups, function () {
                iq.c("group").t(this).up();
            });
        }
        this.connection.sendIQ(iq);
    },
    
    /*-------------------------------------------------------------------------------*/
    deleteContact: function (jid) {
        var iq = $iq({type: "set"})
            .c("query", {xmlns: Strophe.NS.ROSTER})
            .c("item", {jid: jid, subscription: "remove"});
        this.connection.sendIQ(iq);
    },
 
 
    /*-------------------------------------------------------------------------------*/
    modifyContact: function (jid, name, groups) {
        this.addContact(jid, name, groups);
    },
 
    /*-------------------------------------------------------------------------------*/
    subscribe: function (jid, name, groups) {
        this.addContact(jid, name, groups);
        
        var presence = $pres({to: jid, "type": "subscribe"});
        this.connection.send(presence);
    },
    
    /*-------------------------------------------------------------------------------*/
    unsubscribe: function (jid) {
        var presence = $pres({to: jid, "type": "unsubscribe"});
        this.connection.send(presence);
        
        this.deleteContact(jid);
    }
});
