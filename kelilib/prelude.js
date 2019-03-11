"use strict";
// For displaying a particular data
function KELI$show(o) {
    // if o is an array
    if (Array.isArray(o)) {
        return "[" + o.map(KELI$show) + "]"
    }

    if (typeof o === "object") {
        let result = "";
        // if is a tag
        if (o.hasOwnProperty("__tag")) {
            result += `${o.__union}.${o.__tag}`;
            // if is a carryful tag
            if (o.hasOwnProperty("__carry")) {
                result += `(${KELI$show(o.__carry)})`;
            }
        } else { // if is a object
            result += "$.";
            const keys = Object.keys(o);
            for (let i = 0; i < keys.length; i++) {
                result += `${keys[i]}(${KELI$show(o[keys[i]])})`
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

// For comparing if 2 values are equals
// Modified from https://github.com/epoberezkin/fast-deep-equal/blob/master/index.js
const isArray = Array.isArray;
const keyList = Object.keys;
const hasProp = Object.prototype.hasOwnProperty;

function KELI$equal(a, b) {
  if (a === b) return true;

  if (a && b && typeof a == 'object' && typeof b == 'object') {
    let arrA = isArray(a)
      , arrB = isArray(b)
      , i
      , length
      , key;

    if (arrA && arrB) {
      length = a.length;
      if (length != b.length) return false;
      for (i = length; i-- !== 0;)
        if (!KELI$equal(a[i], b[i])) return false;
      return true;
    }

    if (arrA != arrB) return false;

    const dateA = a instanceof Date
      , dateB = b instanceof Date;
    if (dateA != dateB) return false;
    if (dateA && dateB) return a.getTime() == b.getTime();

    const regexpA = a instanceof RegExp
      , regexpB = b instanceof RegExp;
    if (regexpA != regexpB) return false;
    if (regexpA && regexpB) return a.toString() == b.toString();

    const keys = keyList(a);
    length = keys.length;

    if (length !== keyList(b).length)
      return false;

    for (i = length; i-- !== 0;)
      if (!hasProp.call(b, keys[i])) return false;

    for (i = length; i-- !== 0;) {
      key = keys[i];
      if (!KELI$equal(a[key], b[key])) return false;
    }

    return true;
  }

  return a!==a && b!==b;
};