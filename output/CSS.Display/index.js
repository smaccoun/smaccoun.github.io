// Generated by purs version 0.11.7
"use strict";
var CSS_Common = require("../CSS.Common");
var CSS_Property = require("../CSS.Property");
var CSS_String = require("../CSS.String");
var CSS_Stylesheet = require("../CSS.Stylesheet");
var Control_Apply = require("../Control.Apply");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Generic = require("../Data.Generic");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Show = require("../Data.Show");
var Data_Unit = require("../Data.Unit");
var Prelude = require("../Prelude");
var Position = function (x) {
    return x;
};
var FloatLeft = (function () {
    function FloatLeft() {

    };
    FloatLeft.value = new FloatLeft();
    return FloatLeft;
})();
var FloatRight = (function () {
    function FloatRight() {

    };
    FloatRight.value = new FloatRight();
    return FloatRight;
})();
var FloatNone = (function () {
    function FloatNone() {

    };
    FloatNone.value = new FloatNone();
    return FloatNone;
})();
var Display = function (x) {
    return x;
};
var ClearFloatLeft = (function () {
    function ClearFloatLeft() {

    };
    ClearFloatLeft.value = new ClearFloatLeft();
    return ClearFloatLeft;
})();
var ClearFloatRight = (function () {
    function ClearFloatRight() {

    };
    ClearFloatRight.value = new ClearFloatRight();
    return ClearFloatRight;
})();
var ClearFloatBoth = (function () {
    function ClearFloatBoth() {

    };
    ClearFloatBoth.value = new ClearFloatBoth();
    return ClearFloatBoth;
})();
var ClearFloatNone = (function () {
    function ClearFloatNone() {

    };
    ClearFloatNone.value = new ClearFloatNone();
    return ClearFloatNone;
})();
var ClearFloatInherit = (function () {
    function ClearFloatInherit() {

    };
    ClearFloatInherit.value = new ClearFloatInherit();
    return ClearFloatInherit;
})();
var ClearFloatInlineStart = (function () {
    function ClearFloatInlineStart() {

    };
    ClearFloatInlineStart.value = new ClearFloatInlineStart();
    return ClearFloatInlineStart;
})();
var ClearFloatInlineEnd = (function () {
    function ClearFloatInlineEnd() {

    };
    ClearFloatInlineEnd.value = new ClearFloatInlineEnd();
    return ClearFloatInlineEnd;
})();
var zIndex = function ($86) {
    return CSS_Stylesheet.key(CSS_Property.valString)(CSS_String.fromString(CSS_Property.isStringKey)("z-index"))(Data_Show.show(Data_Show.showInt)($86));
};
var valPosition = new CSS_Property.Val(function (v) {
    return v;
});
var valFloat = new CSS_Property.Val(function (v) {
    if (v instanceof FloatLeft) {
        return CSS_String.fromString(CSS_Property.isStringValue)("left");
    };
    if (v instanceof FloatRight) {
        return CSS_String.fromString(CSS_Property.isStringValue)("right");
    };
    if (v instanceof FloatNone) {
        return CSS_String.fromString(CSS_Property.isStringValue)("none");
    };
    throw new Error("Failed pattern match at CSS.Display line 117, column 1 - line 117, column 33: " + [ v.constructor.name ]);
});
var valDisplay = new CSS_Property.Val(function (v) {
    return v;
});
var valClearFloat = new CSS_Property.Val(function (v) {
    if (v instanceof ClearFloatLeft) {
        return CSS_String.fromString(CSS_Property.isStringValue)("left");
    };
    if (v instanceof ClearFloatRight) {
        return CSS_String.fromString(CSS_Property.isStringValue)("right");
    };
    if (v instanceof ClearFloatBoth) {
        return CSS_String.fromString(CSS_Property.isStringValue)("both");
    };
    if (v instanceof ClearFloatNone) {
        return CSS_String.fromString(CSS_Property.isStringValue)("none");
    };
    if (v instanceof ClearFloatInherit) {
        return CSS_String.fromString(CSS_Property.isStringValue)("inherit");
    };
    if (v instanceof ClearFloatInlineStart) {
        return CSS_String.fromString(CSS_Property.isStringValue)("inline-start");
    };
    if (v instanceof ClearFloatInlineEnd) {
        return CSS_String.fromString(CSS_Property.isStringValue)("inline-end");
    };
    throw new Error("Failed pattern match at CSS.Display line 149, column 1 - line 149, column 43: " + [ v.constructor.name ]);
});
var tableRowGroup = Display(CSS_String.fromString(CSS_Property.isStringValue)("table-row-Group"));
var tableRow = Display(CSS_String.fromString(CSS_Property.isStringValue)("table-row"));
var tableHeaderGroup = Display(CSS_String.fromString(CSS_Property.isStringValue)("table-header-group"));
var tableFooterGroup = Display(CSS_String.fromString(CSS_Property.isStringValue)("table-footer-group"));
var tableColumnGroup = Display(CSS_String.fromString(CSS_Property.isStringValue)("table-column-group"));
var tableColumn = Display(CSS_String.fromString(CSS_Property.isStringValue)("table-column"));
var tableCell = Display(CSS_String.fromString(CSS_Property.isStringValue)("table-cell"));
var tableCaption = Display(CSS_String.fromString(CSS_Property.isStringValue)("table-caption"));
var table = Display(CSS_String.fromString(CSS_Property.isStringValue)("table"));
var $$static = Position(CSS_String.fromString(CSS_Property.isStringValue)("static"));
var runIn = Display(CSS_String.fromString(CSS_Property.isStringValue)("runIn"));
var relative = Position(CSS_String.fromString(CSS_Property.isStringValue)("relative"));
var position = CSS_Stylesheet.key(valPosition)(CSS_String.fromString(CSS_Property.isStringKey)("position"));
var noneFloat = new CSS_Common.None(FloatNone.value);
var noneClearFloat = new CSS_Common.None(ClearFloatNone.value);
var listItem = Display(CSS_String.fromString(CSS_Property.isStringValue)("list-item"));
var inlineTable = Display(CSS_String.fromString(CSS_Property.isStringValue)("inline-table"));
var inlineGrid = Display(CSS_String.fromString(CSS_Property.isStringValue)("inline-grid"));
var inlineFlex = Display(CSS_String.fromString(CSS_Property.isStringValue)("inline-flex"));
var inlineBlock = Display(CSS_String.fromString(CSS_Property.isStringValue)("inline-block"));
var inline = Display(CSS_String.fromString(CSS_Property.isStringValue)("inline"));
var inheritClearFloat = new CSS_Common.Inherit(ClearFloatInherit.value);
var grid = Display(CSS_String.fromString(CSS_Property.isStringValue)("grid"));
var genericPosition = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Display.Position" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Position))(Data_Generic.fromSpine(CSS_Property.genericValue)(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Display.Position", [ {
        sigConstructor: "CSS.Display.Position",
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(CSS_Property.genericValue)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("CSS.Display.Position", [ function ($dollarq) {
        return Data_Generic.toSpine(CSS_Property.genericValue)(v);
    } ]);
});
var genericFloat = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Display.FloatLeft" && v.value1.length === 0)) {
        return new Data_Maybe.Just(FloatLeft.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Display.FloatRight" && v.value1.length === 0)) {
        return new Data_Maybe.Just(FloatRight.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Display.FloatNone" && v.value1.length === 0)) {
        return new Data_Maybe.Just(FloatNone.value);
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Display.Float", [ {
        sigConstructor: "CSS.Display.FloatLeft",
        sigValues: [  ]
    }, {
        sigConstructor: "CSS.Display.FloatRight",
        sigValues: [  ]
    }, {
        sigConstructor: "CSS.Display.FloatNone",
        sigValues: [  ]
    } ]);
}, function (v) {
    if (v instanceof FloatLeft) {
        return new Data_Generic.SProd("CSS.Display.FloatLeft", [  ]);
    };
    if (v instanceof FloatRight) {
        return new Data_Generic.SProd("CSS.Display.FloatRight", [  ]);
    };
    if (v instanceof FloatNone) {
        return new Data_Generic.SProd("CSS.Display.FloatNone", [  ]);
    };
    throw new Error("Failed pattern match at CSS.Display line 112, column 8 - line 112, column 46: " + [ v.constructor.name ]);
});
var showFloat = new Data_Show.Show(Data_Generic.gShow(genericFloat));
var genericDisplay = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Display.Display" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Display))(Data_Generic.fromSpine(CSS_Property.genericValue)(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Display.Display", [ {
        sigConstructor: "CSS.Display.Display",
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(CSS_Property.genericValue)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("CSS.Display.Display", [ function ($dollarq) {
        return Data_Generic.toSpine(CSS_Property.genericValue)(v);
    } ]);
});
var genericClearFloat = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Display.ClearFloatLeft" && v.value1.length === 0)) {
        return new Data_Maybe.Just(ClearFloatLeft.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Display.ClearFloatRight" && v.value1.length === 0)) {
        return new Data_Maybe.Just(ClearFloatRight.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Display.ClearFloatBoth" && v.value1.length === 0)) {
        return new Data_Maybe.Just(ClearFloatBoth.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Display.ClearFloatNone" && v.value1.length === 0)) {
        return new Data_Maybe.Just(ClearFloatNone.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Display.ClearFloatInherit" && v.value1.length === 0)) {
        return new Data_Maybe.Just(ClearFloatInherit.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Display.ClearFloatInlineStart" && v.value1.length === 0)) {
        return new Data_Maybe.Just(ClearFloatInlineStart.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Display.ClearFloatInlineEnd" && v.value1.length === 0)) {
        return new Data_Maybe.Just(ClearFloatInlineEnd.value);
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Display.ClearFloat", [ {
        sigConstructor: "CSS.Display.ClearFloatLeft",
        sigValues: [  ]
    }, {
        sigConstructor: "CSS.Display.ClearFloatRight",
        sigValues: [  ]
    }, {
        sigConstructor: "CSS.Display.ClearFloatBoth",
        sigValues: [  ]
    }, {
        sigConstructor: "CSS.Display.ClearFloatNone",
        sigValues: [  ]
    }, {
        sigConstructor: "CSS.Display.ClearFloatInherit",
        sigValues: [  ]
    }, {
        sigConstructor: "CSS.Display.ClearFloatInlineStart",
        sigValues: [  ]
    }, {
        sigConstructor: "CSS.Display.ClearFloatInlineEnd",
        sigValues: [  ]
    } ]);
}, function (v) {
    if (v instanceof ClearFloatLeft) {
        return new Data_Generic.SProd("CSS.Display.ClearFloatLeft", [  ]);
    };
    if (v instanceof ClearFloatRight) {
        return new Data_Generic.SProd("CSS.Display.ClearFloatRight", [  ]);
    };
    if (v instanceof ClearFloatBoth) {
        return new Data_Generic.SProd("CSS.Display.ClearFloatBoth", [  ]);
    };
    if (v instanceof ClearFloatNone) {
        return new Data_Generic.SProd("CSS.Display.ClearFloatNone", [  ]);
    };
    if (v instanceof ClearFloatInherit) {
        return new Data_Generic.SProd("CSS.Display.ClearFloatInherit", [  ]);
    };
    if (v instanceof ClearFloatInlineStart) {
        return new Data_Generic.SProd("CSS.Display.ClearFloatInlineStart", [  ]);
    };
    if (v instanceof ClearFloatInlineEnd) {
        return new Data_Generic.SProd("CSS.Display.ClearFloatInlineEnd", [  ]);
    };
    throw new Error("Failed pattern match at CSS.Display line 144, column 8 - line 144, column 56: " + [ v.constructor.name ]);
});
var showClearFloat = new Data_Show.Show(Data_Generic.gShow(genericClearFloat));
var floatRight = FloatRight.value;
var floatLeft = FloatLeft.value;
var $$float = CSS_Stylesheet.key(valFloat)(CSS_String.fromString(CSS_Property.isStringKey)("float"));
var flex = Display(CSS_String.fromString(CSS_Property.isStringValue)("flex"));
var fixed = Position(CSS_String.fromString(CSS_Property.isStringValue)("fixed"));
var eqPosition = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(CSS_Property.eqValue)(x)(y);
    };
});
var ordPosition = new Data_Ord.Ord(function () {
    return eqPosition;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(CSS_Property.ordValue)(x)(y);
    };
});
var eqFloat = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof FloatLeft && y instanceof FloatLeft) {
            return true;
        };
        if (x instanceof FloatRight && y instanceof FloatRight) {
            return true;
        };
        if (x instanceof FloatNone && y instanceof FloatNone) {
            return true;
        };
        return false;
    };
});
var eqDisplay = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(CSS_Property.eqValue)(x)(y);
    };
});
var ordDisplay = new Data_Ord.Ord(function () {
    return eqDisplay;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(CSS_Property.ordValue)(x)(y);
    };
});
var eqClearFloat = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof ClearFloatLeft && y instanceof ClearFloatLeft) {
            return true;
        };
        if (x instanceof ClearFloatRight && y instanceof ClearFloatRight) {
            return true;
        };
        if (x instanceof ClearFloatBoth && y instanceof ClearFloatBoth) {
            return true;
        };
        if (x instanceof ClearFloatNone && y instanceof ClearFloatNone) {
            return true;
        };
        if (x instanceof ClearFloatInherit && y instanceof ClearFloatInherit) {
            return true;
        };
        if (x instanceof ClearFloatInlineStart && y instanceof ClearFloatInlineStart) {
            return true;
        };
        if (x instanceof ClearFloatInlineEnd && y instanceof ClearFloatInlineEnd) {
            return true;
        };
        return false;
    };
});
var displayNone = Display(CSS_String.fromString(CSS_Property.isStringValue)("none"));
var displayInherit = Display(CSS_String.fromString(CSS_Property.isStringValue)("inherit"));
var display = CSS_Stylesheet.key(valDisplay)(CSS_String.fromString(CSS_Property.isStringKey)("display"));
var clearRight = ClearFloatRight.value;
var clearLeft = ClearFloatLeft.value;
var clearInlineStart = ClearFloatInlineStart.value;
var clearInlineEnd = ClearFloatInlineEnd.value;
var clearBoth = ClearFloatBoth.value;
var clear = CSS_Stylesheet.key(valClearFloat)(CSS_String.fromString(CSS_Property.isStringKey)("clear"));
var block = Display(CSS_String.fromString(CSS_Property.isStringValue)("block"));
var absolute = Position(CSS_String.fromString(CSS_Property.isStringValue)("absolute"));
module.exports = {
    Position: Position,
    position: position,
    "static": $$static,
    absolute: absolute,
    fixed: fixed,
    relative: relative,
    Display: Display,
    inline: inline,
    block: block,
    listItem: listItem,
    runIn: runIn,
    inlineBlock: inlineBlock,
    table: table,
    inlineTable: inlineTable,
    tableRowGroup: tableRowGroup,
    tableHeaderGroup: tableHeaderGroup,
    tableFooterGroup: tableFooterGroup,
    tableRow: tableRow,
    tableColumnGroup: tableColumnGroup,
    tableColumn: tableColumn,
    tableCell: tableCell,
    tableCaption: tableCaption,
    displayNone: displayNone,
    displayInherit: displayInherit,
    flex: flex,
    inlineFlex: inlineFlex,
    grid: grid,
    inlineGrid: inlineGrid,
    display: display,
    FloatLeft: FloatLeft,
    FloatRight: FloatRight,
    FloatNone: FloatNone,
    floatLeft: floatLeft,
    floatRight: floatRight,
    "float": $$float,
    ClearFloatLeft: ClearFloatLeft,
    ClearFloatRight: ClearFloatRight,
    ClearFloatBoth: ClearFloatBoth,
    ClearFloatNone: ClearFloatNone,
    ClearFloatInherit: ClearFloatInherit,
    ClearFloatInlineStart: ClearFloatInlineStart,
    ClearFloatInlineEnd: ClearFloatInlineEnd,
    clearLeft: clearLeft,
    clearRight: clearRight,
    clearBoth: clearBoth,
    clearInlineStart: clearInlineStart,
    clearInlineEnd: clearInlineEnd,
    clear: clear,
    zIndex: zIndex,
    eqPosition: eqPosition,
    ordPosition: ordPosition,
    genericPosition: genericPosition,
    valPosition: valPosition,
    eqDisplay: eqDisplay,
    ordDisplay: ordDisplay,
    genericDisplay: genericDisplay,
    valDisplay: valDisplay,
    eqFloat: eqFloat,
    genericFloat: genericFloat,
    showFloat: showFloat,
    valFloat: valFloat,
    noneFloat: noneFloat,
    eqClearFloat: eqClearFloat,
    genericClearFloat: genericClearFloat,
    showClearFloat: showClearFloat,
    valClearFloat: valClearFloat,
    noneClearFloat: noneClearFloat,
    inheritClearFloat: inheritClearFloat
};
