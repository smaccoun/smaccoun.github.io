// Generated by purs version 0.11.7
"use strict";
var $foreign = require("./foreign");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Either = require("../Data.Either");
var Data_Function = require("../Data.Function");
var Data_Maybe = require("../Data.Maybe");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Data_String_Regex_Flags = require("../Data.String.Regex.Flags");
var Prelude = require("../Prelude");
var showRegex = new Data_Show.Show($foreign["showRegex'"]);
var search = $foreign._search(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var renderFlags = function (v) {
    return (function () {
        if (v.value0.global) {
            return "g";
        };
        return "";
    })() + ((function () {
        if (v.value0.ignoreCase) {
            return "i";
        };
        return "";
    })() + ((function () {
        if (v.value0.multiline) {
            return "m";
        };
        return "";
    })() + ((function () {
        if (v.value0.sticky) {
            return "y";
        };
        return "";
    })() + (function () {
        if (v.value0.unicode) {
            return "u";
        };
        return "";
    })())));
};
var regex = function (s) {
    return function (f) {
        return $foreign["regex'"](Data_Either.Left.create)(Data_Either.Right.create)(s)(renderFlags(f));
    };
};
var parseFlags = function (s) {
    return new Data_String_Regex_Flags.RegexFlags({
        global: Data_String.contains("g")(s),
        ignoreCase: Data_String.contains("i")(s),
        multiline: Data_String.contains("m")(s),
        sticky: Data_String.contains("y")(s),
        unicode: Data_String.contains("u")(s)
    });
};
var match = $foreign._match(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var flags = function ($8) {
    return Data_String_Regex_Flags.RegexFlags.create($foreign["flags'"]($8));
};
module.exports = {
    regex: regex,
    flags: flags,
    renderFlags: renderFlags,
    parseFlags: parseFlags,
    match: match,
    search: search,
    showRegex: showRegex,
    source: $foreign.source,
    test: $foreign.test,
    replace: $foreign.replace,
    "replace'": $foreign["replace'"],
    split: $foreign.split
};
