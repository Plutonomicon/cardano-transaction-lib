// parseJsonExtractingIntegers
//   :: (forall a b. a -> b -> Tuple a b) -> String -> Tuple (String /\ Array (Int /\ String))
const parseJsonExtractingIntegers = str => {
    const s = String(str);

    const index = [];
    let counter = 0;
    let numberAcc = [];

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
                if (!in_number) {
                    arr.push((counter++).toString());
                }
                in_number=true;
            }
            // assuming a number can only end with:
            if (c == ',' || c == '}' || c == ']' || /\s/.test(c)){
                if (in_number) {
                    index.push(numberAcc.join(''));
                    numberAcc = [];
                }
                in_number=false;
            }
            prev_in_number = in_number;
        }
        // push char if in string or is not a whitespace
        if (in_number) {
            numberAcc.push(c);
        } else if (in_string || !/\s/.test(c)) {
            arr.push(c);
        }
    }

    if (in_number) {
        index.push(numberAcc.join(''));
    }

    return [arr.join(''), index];
};

exports.parseJsonExtractingIntegers = tuple => str => {
    const [ output, index ] = parseJsonExtractingIntegers(str);
    return tuple(output)(index);
};
