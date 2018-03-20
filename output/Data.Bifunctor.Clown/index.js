// Generated by purs version 0.11.7
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Biapplicative = require("../Control.Biapplicative");
var Control_Biapply = require("../Control.Biapply");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var Clown = function (x) {
    return x;
};
var showClown = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Clown " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var ordClown = function (dictOrd) {
    return dictOrd;
};
var newtypeClown = new Data_Newtype.Newtype(function (n) {
    return n;
}, Clown);
var functorClown = new Data_Functor.Functor(function (v) {
    return function (v1) {
        return v1;
    };
});
var eqClown = function (dictEq) {
    return dictEq;
};
var bifunctorClown = function (dictFunctor) {
    return new Data_Bifunctor.Bifunctor(function (f) {
        return function (v) {
            return function (v1) {
                return Data_Functor.map(dictFunctor)(f)(v1);
            };
        };
    });
};
var biapplyClown = function (dictApply) {
    return new Control_Biapply.Biapply(function () {
        return bifunctorClown(dictApply.Functor0());
    }, function (v) {
        return function (v1) {
            return Control_Apply.apply(dictApply)(v)(v1);
        };
    });
};
var biapplicativeClown = function (dictApplicative) {
    return new Control_Biapplicative.Biapplicative(function () {
        return biapplyClown(dictApplicative.Apply0());
    }, function (a) {
        return function (v) {
            return Control_Applicative.pure(dictApplicative)(a);
        };
    });
};
module.exports = {
    Clown: Clown,
    newtypeClown: newtypeClown,
    eqClown: eqClown,
    ordClown: ordClown,
    showClown: showClown,
    functorClown: functorClown,
    bifunctorClown: bifunctorClown,
    biapplyClown: biapplyClown,
    biapplicativeClown: biapplicativeClown
};
