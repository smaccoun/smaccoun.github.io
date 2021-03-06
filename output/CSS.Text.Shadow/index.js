// Generated by purs version 0.11.7
"use strict";
var CSS_Color = require("../CSS.Color");
var CSS_Common = require("../CSS.Common");
var CSS_Property = require("../CSS.Property");
var CSS_Size = require("../CSS.Size");
var CSS_String = require("../CSS.String");
var CSS_Stylesheet = require("../CSS.Stylesheet");
var Data_Tuple_Nested = require("../Data.Tuple.Nested");
var TextShadow = (function () {
    function TextShadow(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    TextShadow.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new TextShadow(value0, value1, value2, value3);
                };
            };
        };
    };
    return TextShadow;
})();
var None = (function () {
    function None() {

    };
    None.value = new None();
    return None;
})();
var Initial = (function () {
    function Initial() {

    };
    Initial.value = new Initial();
    return Initial;
})();
var Inherit = (function () {
    function Inherit() {

    };
    Inherit.value = new Inherit();
    return Inherit;
})();
var valTextShadow = new CSS_Property.Val(function (v) {
    if (v instanceof TextShadow) {
        return CSS_Property.value(CSS_Property.valTuple(CSS_Size.valSize)(CSS_Property.valTuple(CSS_Size.valSize)(CSS_Property.valTuple(CSS_Size.valSize)(CSS_Property.valTuple(CSS_Property.valColor)(CSS_Property.valUnit)))))(Data_Tuple_Nested.tuple4(v.value0)(v.value1)(v.value2)(v.value3));
    };
    if (v instanceof None) {
        return CSS_String.fromString(CSS_Property.isStringValue)("none");
    };
    if (v instanceof Initial) {
        return CSS_String.fromString(CSS_Property.isStringValue)("initial");
    };
    if (v instanceof Inherit) {
        return CSS_String.fromString(CSS_Property.isStringValue)("inherit");
    };
    throw new Error("Failed pattern match at CSS.Text.Shadow line 17, column 1 - line 17, column 45: " + [ v.constructor.name ]);
});
var textShadow = function (h) {
    return function (v) {
        return function (b) {
            return function (c) {
                return CSS_Stylesheet.key(valTextShadow)(CSS_String.fromString(CSS_Property.isStringKey)("text-shadow"))(new TextShadow(h, v, b, c));
            };
        };
    };
};
var noneTextShadow = new CSS_Common.None(None.value);
var initialTextShadow = new CSS_Common.Initial(Initial.value);
var inheritTextShadow = new CSS_Common.Inherit(Inherit.value);
module.exports = {
    TextShadow: TextShadow,
    None: None,
    Initial: Initial,
    Inherit: Inherit,
    textShadow: textShadow,
    valTextShadow: valTextShadow,
    noneTextShadow: noneTextShadow,
    initialTextShadow: initialTextShadow,
    inheritTextShadow: inheritTextShadow
};
