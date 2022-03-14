// parseJsonExtractingIntegers
//   :: String -> {patchedPayload :: String, numberIndex :: Array String}
exports.parseJsonExtractingIntegers = str => {
    const [patchedPayload, numberIndex] = parseJsonExtractingIntegers(str);
    return {patchedPayload, numberIndex};
};

exports.stringifyAeson_ = numberIndex => originalObject => {
    const fatal = msg => {
        throw new Error("Error in stringifyObject: " + msg);
    };

    let res = '';

    // Recursively iterate over object fields.
    // TODO: use a trampoline?
    const go = object => {
        if (object === null || typeof object == 'string' || typeof object == 'boolean') {
            res += JSON.stringify(object);
        } else if (object instanceof Array) {
            res += '[';
            object.forEach((elem, ix) => {
                go(elem);
                if (ix != object.length - 1) {
                    res += ',';
                }
            });
            res += ']';
        } else if (typeof object == 'object') {
            res += '{';
            let keys = [];
            for (let key in object) {
                if (object.hasOwnProperty(key)) {
                    keys.push(key);
                }
            };
            keys.sort(); // for stability of Eq instance
            keys.forEach((key, ix) => {
                res += JSON.stringify(key);
                res += ':';
                go(object[key]);
                if (ix != keys.length - 1) {
                    res += ',';
                }
            });
            res += '}';
        } else if (typeof object == 'number') {
            if (object in numberIndex) {
                res += numberIndex[object];
            } else {
                fatal("No such index in numberIndex!");
            }
        } else {
            fatal("Wrong type of object: " + typeof object);
        }
    };

    go(originalObject);
    return res;
};

// NOTE: For a general overview of this function's purpose,
//       consult module docstring in Aeson.purs
const parseJsonExtractingIntegers = str => {
    const s = String(str);

    const index = [];
    let counter = 0;
    let numberAcc = [];

    const arr = [];
    let in_number = false;
    let in_string = false;
    let escaped = -1;

    for (let i = 0, n = s.length; i < n; ++i) {
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
                    // push a number index in place of a number
                    arr.push((counter++).toString());
                }
                in_number=true;
            }
            // assuming a number can only end with:
            if (c == ',' || c == '}' || c == ']' || /\s/.test(c)){
                if (in_number) {
                    // push the accumulated number string into the
                    // number index
                    index.push(numberAcc.join(''));
                    numberAcc = [];
                }
                in_number=false;
            }
        }
        // push char if in string or is not a whitespace
        if (in_number) {
            numberAcc.push(c);
        } else if (in_string || !/\s/.test(c)) {
            arr.push(c);
        }
    }

    // single number case
    if (in_number) {
        index.push(numberAcc.join(''));
    }

    return [arr.join(''), index];
};
