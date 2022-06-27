exports._publishTestFeedback = value => () => window.ctlTestFeedback = value;

exports._retrieveTestFeedback = nothing => mkJust => () => {
    console.log("test");
    if(typeof(window.ctlTestFeedback) === 'undefined') {
	console.log("1");	
	return nothing;
    }
    else {
	console.log("2");	
	return mkJust(ctlTestFeedback);
    }
}
