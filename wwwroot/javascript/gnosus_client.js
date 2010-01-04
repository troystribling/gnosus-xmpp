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
function connect(service, jid, pass) {
    $(function() {
        $("#client-1").resizable({maxWidth: 1000, minWidth: 750, autoHide: true});
    });
    var connection = new Strophe.Connection(service);
    connection.rawInput = rawInput;
    connection.rawOutput = rawOutput;
	connection.connect(jid, pass, onConnect);
}