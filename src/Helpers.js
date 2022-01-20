
// jsonTurnNumbersToStrings :: String -> String
exports.jsonTurnNumbersToStrings = (str) => {
    const s = String(str);

    var arr = [];
    var prev_in_number = false;
    var in_number = false;
    var in_string = false;

    for (var i = 0, n = s.length; i < n; ++i) {
        const c = s[i];
        if (c == '"') {
            if( i ==0 ||  (i>0 && s[i-1] != "\\" )){
                in_string = !in_string;
            }
        }
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
        if(in_string || !/\s/.test(c)){
            arr.push(c);
        }
    }
    if (!in_string && in_number){
        arr.push("\"");
    }
    return arr.join('');
};
