
// jsonTurnNumbersToStrings :: String -> String
exports.jsonTurnNumbersToStrings = (str) => {
    const s = String(str);

    var arr = [];
    var prev_in_number = false;
    var in_number = false;
    var in_string = false;
    var escaped = -1;

    for (var i = 0, n = s.length; i < n; ++i) {
        const c = s[i];
        // set the escape flag
        if (in_string && escaped!=i){
            if (c == "\\"){
                escaped = i+1;
            }
        }
        // set in_string flag
        if (c == '"' && escaped!=i) {
            in_string = !in_string;
        }
        // set in_number flag and quote numbers
        if(!in_string){
            if (c >= '0' && c <= '9' || c == '-') {
                in_number=true;
            }
            // assuming a number can only end with:
            if (c == ',' || c == '}' || c == ']' || /\s/.test(c)){
                if(in_number){
                    arr.push("\"");
                }
                in_number=false;
            }
            if(in_number){
                if(!prev_in_number){
                    arr.push("\"");
                }
            }
            prev_in_number = in_number;
        }
        // push char if in string or is not a whitespace
        if(in_string || !/\s/.test(c)){
            arr.push(c);
        }
    }
    // simple number case - close with quote
    if (!in_string && in_number){
        arr.push("\"");
    }
    return arr.join('');
};
