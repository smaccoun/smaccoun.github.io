// Generated by purs version 0.11.7
"use strict";
var Control_Bind = require("../Control.Bind");
var Control_Monad_Except = require("../Control.Monad.Except");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var DOM_Event_Event = require("../DOM.Event.Event");
var DOM_Event_Types = require("../DOM.Event.Types");
var DOM_HTML_Event_EventTypes = require("../DOM.HTML.Event.EventTypes");
var DOM_HTML_Event_Types = require("../DOM.HTML.Event.Types");
var Data_Either = require("../Data.Either");
var Data_Foreign = require("../Data.Foreign");
var Data_Foreign_Index = require("../Data.Foreign.Index");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Identity = require("../Data.Identity");
var Data_Maybe = require("../Data.Maybe");
var Halogen_HTML_Core = require("../Halogen.HTML.Core");
var Halogen_HTML_Properties = require("../Halogen.HTML.Properties");
var Halogen_Query = require("../Halogen.Query");
var Halogen_Query_InputF = require("../Halogen.Query.InputF");
var Prelude = require("../Prelude");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var touchHandler = Unsafe_Coerce.unsafeCoerce;
var mouseHandler = Unsafe_Coerce.unsafeCoerce;
var keyHandler = Unsafe_Coerce.unsafeCoerce;
var input_ = function (f) {
    return function (v) {
        return Data_Maybe.Just.create(Halogen_Query.action(f));
    };
};
var input = function (f) {
    return function (x) {
        return Data_Maybe.Just.create(Halogen_Query.action(f(x)));
    };
};
var handler = function (et) {
    return function ($1) {
        return Halogen_HTML_Core.handler(et)(Data_Functor.map(Data_Functor.functorFn)(Data_Functor.map(Data_Maybe.functorMaybe)(Halogen_Query_InputF.Query.create))($1));
    };
};
var onAbort = handler(DOM_HTML_Event_EventTypes.abort);
var onChange = handler(DOM_HTML_Event_EventTypes.change);
var onClick = function ($2) {
    return handler(DOM_HTML_Event_EventTypes.click)(mouseHandler($2));
};
var onContextMenu = function ($3) {
    return handler(DOM_HTML_Event_EventTypes.contextmenu)(mouseHandler($3));
};
var onDoubleClick = function ($4) {
    return handler(DOM_HTML_Event_EventTypes.dblclick)(mouseHandler($4));
};
var onError = handler(DOM_HTML_Event_EventTypes.error);
var onInput = handler(DOM_HTML_Event_EventTypes.input);
var onInvalid = handler(DOM_HTML_Event_EventTypes.invalid);
var onKeyDown = function ($5) {
    return handler(DOM_HTML_Event_EventTypes.keydown)(keyHandler($5));
};
var onKeyPress = function ($6) {
    return handler(DOM_HTML_Event_EventTypes.keypress)(keyHandler($6));
};
var onKeyUp = function ($7) {
    return handler(DOM_HTML_Event_EventTypes.keyup)(keyHandler($7));
};
var onLoad = handler(DOM_HTML_Event_EventTypes.load);
var onMouseDown = function ($8) {
    return handler(DOM_HTML_Event_EventTypes.mousedown)(mouseHandler($8));
};
var onMouseEnter = function ($9) {
    return handler(DOM_HTML_Event_EventTypes.mouseenter)(mouseHandler($9));
};
var onMouseLeave = function ($10) {
    return handler(DOM_HTML_Event_EventTypes.mouseleave)(mouseHandler($10));
};
var onMouseMove = function ($11) {
    return handler(DOM_HTML_Event_EventTypes.mousemove)(mouseHandler($11));
};
var onMouseOut = function ($12) {
    return handler(DOM_HTML_Event_EventTypes.mouseout)(mouseHandler($12));
};
var onMouseOver = function ($13) {
    return handler(DOM_HTML_Event_EventTypes.mouseover)(mouseHandler($13));
};
var onMouseUp = function ($14) {
    return handler(DOM_HTML_Event_EventTypes.mouseup)(mouseHandler($14));
};
var onReset = handler(DOM_HTML_Event_EventTypes.reset);
var onResize = handler(DOM_HTML_Event_EventTypes.resize);
var onScroll = handler(DOM_HTML_Event_EventTypes.scroll);
var onSelect = handler(DOM_HTML_Event_EventTypes.select);
var onSubmit = handler(DOM_HTML_Event_EventTypes.submit);
var onTouchCancel = function ($15) {
    return handler(DOM_HTML_Event_EventTypes.touchcancel)(touchHandler($15));
};
var onTouchEnd = function ($16) {
    return handler(DOM_HTML_Event_EventTypes.touchend)(touchHandler($16));
};
var onTouchEnter = function ($17) {
    return handler(DOM_HTML_Event_EventTypes.touchenter)(touchHandler($17));
};
var onTouchLeave = function ($18) {
    return handler(DOM_HTML_Event_EventTypes.touchleave)(touchHandler($18));
};
var onTouchMove = function ($19) {
    return handler(DOM_HTML_Event_EventTypes.touchmove)(touchHandler($19));
};
var onTouchStart = function ($20) {
    return handler(DOM_HTML_Event_EventTypes.touchstart)(touchHandler($20));
};
var onTransitionEnd = handler(DOM_HTML_Event_EventTypes.transitionend);
var focusHandler = Unsafe_Coerce.unsafeCoerce;
var onBlur = function ($21) {
    return handler(DOM_HTML_Event_EventTypes.blur)(focusHandler($21));
};
var onFocus = function ($22) {
    return handler(DOM_HTML_Event_EventTypes.focus)(focusHandler($22));
};
var onFocusIn = function ($23) {
    return handler(DOM_HTML_Event_EventTypes.focusin)(focusHandler($23));
};
var onFocusOut = function ($24) {
    return handler(DOM_HTML_Event_EventTypes.focusout)(focusHandler($24));
};
var dragHandler = Unsafe_Coerce.unsafeCoerce;
var onDrag = function ($25) {
    return handler(DOM_HTML_Event_EventTypes.drag)(dragHandler($25));
};
var onDragEnd = function ($26) {
    return handler(DOM_HTML_Event_EventTypes.dragend)(dragHandler($26));
};
var onDragEnter = function ($27) {
    return handler(DOM_HTML_Event_EventTypes.dragenter)(dragHandler($27));
};
var onDragExit = function ($28) {
    return handler(DOM_HTML_Event_EventTypes.dragexit)(dragHandler($28));
};
var onDragLeave = function ($29) {
    return handler(DOM_HTML_Event_EventTypes.dragleave)(dragHandler($29));
};
var onDragOver = function ($30) {
    return handler(DOM_HTML_Event_EventTypes.dragover)(dragHandler($30));
};
var onDragStart = function ($31) {
    return handler(DOM_HTML_Event_EventTypes.dragstart)(dragHandler($31));
};
var onDrop = function ($32) {
    return handler(DOM_HTML_Event_EventTypes.drop)(dragHandler($32));
};
var clipboardHandler = Unsafe_Coerce.unsafeCoerce;
var onCopy = function ($33) {
    return handler(DOM_HTML_Event_EventTypes.copy)(clipboardHandler($33));
};
var onCut = function ($34) {
    return handler(DOM_HTML_Event_EventTypes.cut)(clipboardHandler($34));
};
var onPaste = function ($35) {
    return handler(DOM_HTML_Event_EventTypes.paste)(clipboardHandler($35));
};
var addForeignPropHandler = function (key) {
    return function (prop) {
        return function (reader) {
            return function (f) {
                return handler(key)(function ($36) {
                    return Data_Either.either(Data_Function["const"](Data_Maybe.Nothing.value))(f)(Control_Monad_Except.runExcept(Control_Bind.composeKleisliFlipped(Control_Monad_Except_Trans.bindExceptT(Data_Identity.monadIdentity))(reader)(Data_Foreign_Index.readProp(prop))(Data_Foreign.toForeign(DOM_Event_Event.currentTarget($36)))));
                });
            };
        };
    };
};
var onChecked = addForeignPropHandler(DOM_HTML_Event_EventTypes.change)("checked")(Data_Foreign.readBoolean);
var onSelectedIndexChange = addForeignPropHandler(DOM_HTML_Event_EventTypes.change)("selectedIndex")(Data_Foreign.readInt);
var onValueChange = addForeignPropHandler(DOM_HTML_Event_EventTypes.change)("value")(Data_Foreign.readString);
var onValueInput = addForeignPropHandler(DOM_HTML_Event_EventTypes.input)("value")(Data_Foreign.readString);
module.exports = {
    input: input,
    input_: input_,
    handler: handler,
    onAbort: onAbort,
    onError: onError,
    onLoad: onLoad,
    onScroll: onScroll,
    onChange: onChange,
    onInput: onInput,
    onInvalid: onInvalid,
    onReset: onReset,
    onSelect: onSelect,
    onSubmit: onSubmit,
    onTransitionEnd: onTransitionEnd,
    onCopy: onCopy,
    onPaste: onPaste,
    onCut: onCut,
    onClick: onClick,
    onContextMenu: onContextMenu,
    onDoubleClick: onDoubleClick,
    onMouseDown: onMouseDown,
    onMouseEnter: onMouseEnter,
    onMouseLeave: onMouseLeave,
    onMouseMove: onMouseMove,
    onMouseOver: onMouseOver,
    onMouseOut: onMouseOut,
    onMouseUp: onMouseUp,
    onKeyDown: onKeyDown,
    onKeyPress: onKeyPress,
    onKeyUp: onKeyUp,
    onBlur: onBlur,
    onFocus: onFocus,
    onFocusIn: onFocusIn,
    onFocusOut: onFocusOut,
    onDrag: onDrag,
    onDragEnd: onDragEnd,
    onDragExit: onDragExit,
    onDragEnter: onDragEnter,
    onDragLeave: onDragLeave,
    onDragOver: onDragOver,
    onDragStart: onDragStart,
    onDrop: onDrop,
    onTouchCancel: onTouchCancel,
    onTouchEnd: onTouchEnd,
    onTouchEnter: onTouchEnter,
    onTouchLeave: onTouchLeave,
    onTouchMove: onTouchMove,
    onTouchStart: onTouchStart,
    onResize: onResize,
    onValueChange: onValueChange,
    onValueInput: onValueInput,
    onSelectedIndexChange: onSelectedIndexChange,
    onChecked: onChecked
};