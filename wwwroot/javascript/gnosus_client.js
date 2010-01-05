/*--------------------------------------------------------------------------------
logging
---------------------------------------------------------------------------------*/
function rawInput(data) {
    console.log('RECV: ' + data);
}

function rawOutput(data) {
    console.log('SENT: ' + data);
}

Strophe.log = function (level, msg) {
    console.log('LOG: ' + msg);
};

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

/*-------------------------------------------------------------------------------*/
function new_client(client_id) {
    var client = $(client_id);
    $(function() {
        client.resizable({handles:'s', minHeight: 250, autoHide: true});
    });
	client.splitter({type: "v", outline: true, minLeft: 200, sizeLeft: 200, minRight: 500,
		             cookie: "vsplitter"});
}

/*-------------------------------------------------------------------------------*/
function connect(service, jid, pass) {
    new_client("#client-1")
    var connection = new Strophe.Connection(service);
    connection.rawInput = rawInput;
    connection.rawOutput = rawOutput;
	connection.connect(jid, pass, onConnect);
}