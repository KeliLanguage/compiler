function KELI_PRELUDE$show(o) {
    // if o is an array
    if (Array.isArray(o)) {
        return "[" + o.map(KELI_PRELUDE$show) + "]"
    }

    if (typeof o === "object") {
        let result = "";
        // if is a tag
        if (o.hasOwnProperty("__tag")) {
            result += `${o.__union}.${o.__tag}`;
            // if is a carryful tag
            if (o.hasOwnProperty("__carry")) {
                result += `(${KELI_PRELUDE$show(o.__carry)})`;
            }
        } else { // if is a object
            result += "$.";
            const keys = Object.keys(o);
            for (let i = 0; i < keys.length; i++) {
                result += `${keys[i]}(${KELI_PRELUDE$show(o[keys[i]])})`
                if (i < keys.length - 1) {
                    result += " ";
                }
            }
        }
        return result;
    } else if (typeof o === "string") {
        return `"${o}"`;
    } else {
        return o;
    }

}