const child_process = require("child_process");
const webpack = require("webpack");
const webpack_dev_server = require("webpack-dev-server");
const webpack_config = require("/home/mike/src/mlabs/cardano-transaction-lib/webpack.config.e2e.js");

exports._exec = function(cmd) {
    return function (onError, onSuccess) {
	var cancel = child_process.exec(cmd, (error, stdout, stderr) => {
	    if (error) {
		onError(error);
	    } else {
		onSuccess(stdout);
	    }
	});

	
	return function (cancelError, onCancelerError, onCancelerSuccess) {
	    cancel();
	    onCancelerSuccess();
	};
    };
}

exports._runServer = function(port) {
    return function (onError, onSuccess) {
	const compiler = webpack(webpack_config);
	const devServerOptions = { port: port, open: true };

	console.log("Entry points: " + devServerOptions.entry);
	
	const server = new webpack_dev_server(devServerOptions, compiler);

	var cancel = server.start((error, stdout, stderr) => {
	    console.log(stdout);
	    console.log(stderr);	    
	    if(error) {
		onError(error);
	    } else {
		onSuccess();
	    }
	});

	return function (cancelError, onCancelerError, onCancelerSuccess) {
	    cancel();
	    onCancelerSuccess();
	}
    };
}
