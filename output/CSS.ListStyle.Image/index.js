// Generated by purs version 0.11.7
"use strict";
var CSS_Common = require("../CSS.Common");
var CSS_Property = require("../CSS.Property");
var CSS_String = require("../CSS.String");
var CSS_Stylesheet = require("../CSS.Stylesheet");
var Control_Apply = require("../Control.Apply");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Generic = require("../Data.Generic");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_Unit = require("../Data.Unit");
var ListStyleImage = (function () {
    function ListStyleImage(value0) {
        this.value0 = value0;
    };
    ListStyleImage.create = function (value0) {
        return new ListStyleImage(value0);
    };
    return ListStyleImage;
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
var Unset = (function () {
    function Unset() {

    };
    Unset.value = new Unset();
    return Unset;
})();
var None = (function () {
    function None() {

    };
    None.value = new None();
    return None;
})();
var valListStyleImage = new CSS_Property.Val(function (v) {
    if (v instanceof Initial) {
        return CSS_String.fromString(CSS_Property.isStringValue)("initial");
    };
    if (v instanceof Inherit) {
        return CSS_String.fromString(CSS_Property.isStringValue)("inherit");
    };
    if (v instanceof Unset) {
        return CSS_String.fromString(CSS_Property.isStringValue)("unset");
    };
    if (v instanceof None) {
        return CSS_String.fromString(CSS_Property.isStringValue)("none");
    };
    if (v instanceof ListStyleImage) {
        return CSS_String.fromString(CSS_Property.isStringValue)("url('" + (v.value0 + "')"));
    };
    throw new Error("Failed pattern match at CSS.ListStyle.Image line 28, column 1 - line 28, column 49: " + [ v.constructor.name ]);
});
var urlListStyleImage = new CSS_Common.URL(function (s) {
    return new ListStyleImage(s);
});
var unsetListStyleImage = new CSS_Common.Unset(Unset.value);
var noneListImageImage = new CSS_Common.None(None.value);
var listStyleImage = CSS_Stylesheet.key(valListStyleImage)(CSS_String.fromString(CSS_Property.isStringKey)("list-style-image"));
var initialListStyleImage = new CSS_Common.Initial(Initial.value);
var inheritListStyleImage = new CSS_Common.Inherit(Inherit.value);
var genericListStyleImage = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.ListStyle.Image.ListStyleImage" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(ListStyleImage.create))(Data_Generic.fromSpine(Data_Generic.genericString)(v["value1"][0](Data_Unit.unit)));
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.ListStyle.Image.Initial" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Initial.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.ListStyle.Image.Inherit" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Inherit.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.ListStyle.Image.Unset" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Unset.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.ListStyle.Image.None" && v.value1.length === 0)) {
        return new Data_Maybe.Just(None.value);
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.ListStyle.Image.ListStyleImage", [ {
        sigConstructor: "CSS.ListStyle.Image.ListStyleImage",
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
        } ]
    }, {
        sigConstructor: "CSS.ListStyle.Image.Initial",
        sigValues: [  ]
    }, {
        sigConstructor: "CSS.ListStyle.Image.Inherit",
        sigValues: [  ]
    }, {
        sigConstructor: "CSS.ListStyle.Image.Unset",
        sigValues: [  ]
    }, {
        sigConstructor: "CSS.ListStyle.Image.None",
        sigValues: [  ]
    } ]);
}, function (v) {
    if (v instanceof ListStyleImage) {
        return new Data_Generic.SProd("CSS.ListStyle.Image.ListStyleImage", [ function ($dollarq) {
            return Data_Generic.toSpine(Data_Generic.genericString)(v.value0);
        } ]);
    };
    if (v instanceof Initial) {
        return new Data_Generic.SProd("CSS.ListStyle.Image.Initial", [  ]);
    };
    if (v instanceof Inherit) {
        return new Data_Generic.SProd("CSS.ListStyle.Image.Inherit", [  ]);
    };
    if (v instanceof Unset) {
        return new Data_Generic.SProd("CSS.ListStyle.Image.Unset", [  ]);
    };
    if (v instanceof None) {
        return new Data_Generic.SProd("CSS.ListStyle.Image.None", [  ]);
    };
    throw new Error("Failed pattern match at CSS.ListStyle.Image line 23, column 8 - line 23, column 64: " + [ v.constructor.name ]);
});
var showListStyleImage = new Data_Show.Show(Data_Generic.gShow(genericListStyleImage));
var eqListStyleImage = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof ListStyleImage && y instanceof ListStyleImage) {
            return x.value0 === y.value0;
        };
        if (x instanceof Initial && y instanceof Initial) {
            return true;
        };
        if (x instanceof Inherit && y instanceof Inherit) {
            return true;
        };
        if (x instanceof Unset && y instanceof Unset) {
            return true;
        };
        if (x instanceof None && y instanceof None) {
            return true;
        };
        return false;
    };
});
var ordListStyleImage = new Data_Ord.Ord(function () {
    return eqListStyleImage;
}, function (x) {
    return function (y) {
        if (x instanceof ListStyleImage && y instanceof ListStyleImage) {
            return Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
        };
        if (x instanceof ListStyleImage) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof ListStyleImage) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Initial && y instanceof Initial) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Initial) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Initial) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Inherit && y instanceof Inherit) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Inherit) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Inherit) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Unset && y instanceof Unset) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Unset) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Unset) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof None && y instanceof None) {
            return Data_Ordering.EQ.value;
        };
        throw new Error("Failed pattern match at CSS.ListStyle.Image line 22, column 8 - line 22, column 56: " + [ x.constructor.name, y.constructor.name ]);
    };
});
module.exports = {
    ListStyleImage: ListStyleImage,
    Initial: Initial,
    Inherit: Inherit,
    Unset: Unset,
    None: None,
    listStyleImage: listStyleImage,
    eqListStyleImage: eqListStyleImage,
    ordListStyleImage: ordListStyleImage,
    genericListStyleImage: genericListStyleImage,
    showListStyleImage: showListStyleImage,
    valListStyleImage: valListStyleImage,
    initialListStyleImage: initialListStyleImage,
    inheritListStyleImage: inheritListStyleImage,
    unsetListStyleImage: unsetListStyleImage,
    noneListImageImage: noneListImageImage,
    urlListStyleImage: urlListStyleImage
};
