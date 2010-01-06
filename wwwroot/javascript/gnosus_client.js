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
};

/**********************************************************************************
client
**********************************************************************************/
Gnosus = {
    connection: null,
    account: null
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
function Contact() {
    this.jid = "";
    this.ask = "";
    this.subscription = "none";
    this.groups = [];
}

/*-------------------------------------------------------------------------------*/
function RosterItem() {
    this.jid = "";
    this.status = "none";
    this.client_name = "";
    this.client_version = "";
    this.client_os = "";
}

/**********************************************************************************
roster
**********************************************************************************/
