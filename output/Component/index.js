"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Data_Function = require("../Data.Function");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Maybe = require("../Data.Maybe");
var Halogen = require("../Halogen");
var Halogen_Component = require("../Halogen.Component");
var Halogen_HTML = require("../Halogen.HTML");
var Halogen_HTML_Core = require("../Halogen.HTML.Core");
var Halogen_HTML_Elements = require("../Halogen.HTML.Elements");
var Halogen_HTML_Events = require("../Halogen.HTML.Events");
var Halogen_HTML_Properties = require("../Halogen.HTML.Properties");
var Halogen_Query_HalogenM = require("../Halogen.Query.HalogenM");
var Prelude = require("../Prelude");
var ToggleState = (function () {
    function ToggleState(value0) {
        this.value0 = value0;
    };
    ToggleState.create = function (value0) {
        return new ToggleState(value0);
    };
    return ToggleState;
})();
var personalLinkView = function (srcUrl) {
    return function (displayText) {
        return Halogen_HTML_Elements.a([ Halogen_HTML_Properties.href(srcUrl) ])([ Halogen_HTML_Core.text(displayText) ]);
    };
};
var component = (function () {
    var render = function (state) {
        return Halogen_HTML_Elements.div_([ Halogen_HTML_Elements.div([ Halogen_HTML_Events.onClick(Halogen_HTML_Events.input_(ToggleState.create)) ])([ (function () {
            var $1 = !state.on;
            if ($1) {
                return Halogen_HTML_Elements.h1_([ Halogen_HTML_Core.text("Welcome!") ]);
            };
            return Halogen_HTML_Elements.img([ Halogen_HTML_Properties.src("../images/raccoon-icon.png") ]);
        })() ]), Halogen_HTML_Elements.a([ Halogen_HTML_Properties.href("https://www.linkedin.com/in/steven-maccoun-b4448b38/") ])([ Halogen_HTML_Core.text("Linkedin") ]), personalLinkView("https://github.com/smaccoun")("Github") ]);
    };
    var initialState = {
        on: false
    };
    var $$eval = function (v) {
        return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(function (state) {
            return {
                on: !state.on
            };
        }))(function () {
            return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(v.value0);
        });
    };
    return Halogen_Component.component(Halogen_HTML_Core.bifunctorHTML)({
        initialState: Data_Function["const"](initialState),
        render: render,
        "eval": $$eval,
        receiver: Data_Function["const"](Data_Maybe.Nothing.value)
    });
})();
module.exports = {
    ToggleState: ToggleState,
    component: component,
    personalLinkView: personalLinkView
};
