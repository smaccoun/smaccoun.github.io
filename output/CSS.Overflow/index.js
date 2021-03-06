// Generated by purs version 0.11.7
"use strict";
var CSS_Property = require("../CSS.Property");
var CSS_String = require("../CSS.String");
var CSS_Stylesheet = require("../CSS.Stylesheet");
var Control_Apply = require("../Control.Apply");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Generic = require("../Data.Generic");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Unit = require("../Data.Unit");
var Prelude = require("../Prelude");
var Overflow = function (x) {
    return x;
};
var visible = Overflow(CSS_String.fromString(CSS_Property.isStringValue)("visible"));
var valOverflow = new CSS_Property.Val(function (v) {
    return v;
});
var scroll = Overflow(CSS_String.fromString(CSS_Property.isStringValue)("scroll"));
var overflowY = CSS_Stylesheet.key(valOverflow)(CSS_String.fromString(CSS_Property.isStringKey)("overflow-y"));
var overflowX = CSS_Stylesheet.key(valOverflow)(CSS_String.fromString(CSS_Property.isStringKey)("overflow-x"));
var overflowInherit = Overflow(CSS_String.fromString(CSS_Property.isStringValue)("inherit"));
var overflowAuto = Overflow(CSS_String.fromString(CSS_Property.isStringValue)("auto"));
var overflow = CSS_Stylesheet.key(valOverflow)(CSS_String.fromString(CSS_Property.isStringKey)("overflow"));
var hidden = Overflow(CSS_String.fromString(CSS_Property.isStringValue)("hidden"));
var genericOverflow = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Overflow.Overflow" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Overflow))(Data_Generic.fromSpine(CSS_Property.genericValue)(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Overflow.Overflow", [ {
        sigConstructor: "CSS.Overflow.Overflow",
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(CSS_Property.genericValue)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("CSS.Overflow.Overflow", [ function ($dollarq) {
        return Data_Generic.toSpine(CSS_Property.genericValue)(v);
    } ]);
});
var eqOverflow = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(CSS_Property.eqValue)(x)(y);
    };
});
var ordOverflow = new Data_Ord.Ord(function () {
    return eqOverflow;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(CSS_Property.ordValue)(x)(y);
    };
});
module.exports = {
    Overflow: Overflow,
    overflow: overflow,
    overflowX: overflowX,
    overflowY: overflowY,
    overflowAuto: overflowAuto,
    hidden: hidden,
    scroll: scroll,
    visible: visible,
    overflowInherit: overflowInherit,
    eqOverflow: eqOverflow,
    ordOverflow: ordOverflow,
    genericOverflow: genericOverflow,
    valOverflow: valOverflow
};
