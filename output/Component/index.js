"use strict";
var CSS = require("../CSS");
var CSS_Font = require("../CSS.Font");
var CSS_Geometry = require("../CSS.Geometry");
var CSS_Property = require("../CSS.Property");
var CSS_Render = require("../CSS.Render");
var CSS_Size = require("../CSS.Size");
var CSS_Stylesheet = require("../CSS.Stylesheet");
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array = require("../Data.Array");
var Data_Either = require("../Data.Either");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Int = require("../Data.Int");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Semigroup = require("../Data.Semigroup");
var Data_StrMap = require("../Data.StrMap");
var Data_String = require("../Data.String");
var Data_Tuple = require("../Data.Tuple");
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
var style = (function () {
    var toString = function ($13) {
        return Data_String.joinWith("; ")(Data_StrMap.foldMap(Data_Monoid.monoidArray)(function (key) {
            return function (val) {
                return [ key + (": " + val) ];
            };
        })($13));
    };
    var rights = Data_Array.concatMap(Data_Foldable.foldMap(Data_Either.foldableEither)(Data_Monoid.monoidArray)(Data_Array.singleton));
    var property = function (v) {
        if (v instanceof CSS_Stylesheet.Property) {
            return new Data_Maybe.Just(new Data_Tuple.Tuple(v.value0, v.value1));
        };
        return Data_Maybe.Nothing.value;
    };
    var rules = function (rs) {
        var properties = Control_Bind.bind(Control_Bind.bindArray)(Data_Array.mapMaybe(property)(rs))(function ($14) {
            return rights(CSS_Render.collect($14));
        });
        return Data_StrMap.fromFoldable(Data_Foldable.foldableArray)(properties);
    };
    return function ($15) {
        return Halogen_HTML_Properties.attr("style")(toString(rules(CSS_Stylesheet.runS($15))));
    };
})();
var personalLinkIcon = function (v) {
    var pxS = function (i) {
        return CSS_Size.px(Data_Int.toNumber(i));
    };
    return Halogen_HTML_Elements.a([ Halogen_HTML_Properties.href(v.srcUrl), style(CSS_Geometry.paddingLeft(pxS(10))) ])([ Halogen_HTML_Elements.i([ Halogen_HTML_Properties.class_(v["className'"]), style(CSS_Font.fontSize(pxS(50))) ])([  ]) ]);
};
var linkedinIcon = {
    srcUrl: "https://www.linkedin.com/in/steven-maccoun-b4448b38/",
    displayText: "Linkedin",
    "className'": "fa fa-linkedin"
};
var githubLinkIcon = {
    srcUrl: "https://github.com/smaccoun",
    displayText: "Github",
    "className'": "fa fa-github"
};
var component = (function () {
    var render = function (state) {
        return Halogen_HTML_Elements.div_([ Halogen_HTML_Elements.div([ Halogen_HTML_Events.onClick(Halogen_HTML_Events.input_(ToggleState.create)) ])([ (function () {
            var $10 = !state.on;
            if ($10) {
                return Halogen_HTML_Elements.h1_([ Halogen_HTML_Core.text("Welcome!") ]);
            };
            return Halogen_HTML_Elements.img([ Halogen_HTML_Properties.src("../images/raccoon-icon.png") ]);
        })() ]), personalLinkIcon(linkedinIcon), personalLinkIcon(githubLinkIcon) ]);
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
    githubLinkIcon: githubLinkIcon,
    linkedinIcon: linkedinIcon,
    personalLinkIcon: personalLinkIcon,
    style: style
};
